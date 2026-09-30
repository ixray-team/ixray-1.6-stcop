/**************************************************************************************
* Copyright (C) 2026 Anton Kovalev (vertver)
* New Sound Engine
***************************************************************************************
* Source code is licensed under the following terms:
*
* 1. IX-Ray Team License
*    Non-exclusive, royalty-free, perpetual license is hereby granted to:
*      - ForserX   (https://github.com/ForserX)
*      - Drombeys  (https://github.com/Drombeys)
*      - v2v3v4    (https://github.com/v2v3v4)
*
*    Permitted rights:
*      - Copy, modify, merge, publish and distribute this Software
*        and its documentation.
*
* 2. Public Access License
*    Non-exclusive, "access-view-study" rights granted to everyone else.
*
*    Permitted rights:
*      - Private copying is allowed, provided that no distribution occurs.
*      - Public cloning (i.e. "forking") is allowed, but any source code
*        modification or binary redistribution is prohibited.
*
* Usage of this Software beyond the rights granted above is strictly prohibited.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "SoundSource.h"
#include "ogg_utils.h"

#include <condition_variable>
#include <mutex>

#define SND_CACHE_CHUNK_LINES (32)
#define SND_CACHE_LINE_WIDTH (12)
#define SND_CACHE_LINE_CAPACITY ((SND_BLOCKSIZE + 1) * SND_CACHE_LINE_WIDTH)
#define SND_CACHE_LINE_MAX_TIME_NS (1000000000)

struct sound_decode_command {
    xr_string name;
    u32 position;
};

struct sound_cache_line_state {
    u32 start;
    u32 end;
    u64 timestamp;
    sound_source_state* owner;
    float data[SND_CHANNEL_COUNT][SND_CACHE_LINE_CAPACITY];
};

struct sound_source_pool_state {
    xrSRWLock lock;
    std::mutex decode_mutex;
    std::condition_variable decode_cv;
    xr_vector<sound_decode_command> decode_queue;
    bool decode_stop = false;
    ThreadID decode_thread = 0;
    xr_vector<u32> free_cache_lines;
    xr_vector<sound_cache_line_state*> cache_chunks;
    xr_hash_map<xr_string, sound_source_state> sources;
};

struct ogg_comment_decl {
    u32 version;
    u32 min_size;
    bool has_volume;
    bool has_ai_distance;
};


xrSRWLock snd_source_lock;
sound_stats snd_stats = {};
static sound_source_pool_state source_pool_state = {};

static const ogg_comment_decl ogg_comment_decls[] = {
    { 0x0001, 12, false, false },
    { 0x0002, 16, true, false },
    { OGG_COMMENT_VERSION, 20, true, true }
};

static bool
Snd_IsCacheLineValid(u32 cache_idx)
{
    return cache_idx != 0 && cache_idx <= (u32)source_pool_state.cache_chunks.size() * SND_CACHE_CHUNK_LINES;
}

static sound_cache_line_state*
Snd_GetCacheLine(u32 cache_idx)
{
    return &source_pool_state.cache_chunks[(cache_idx - 1) / SND_CACHE_CHUNK_LINES][(cache_idx - 1) % SND_CACHE_CHUNK_LINES];
}

static void
Snd_GrowCacheLines()
{
    sound_cache_line_state* chunk = xr_alloc<sound_cache_line_state>(SND_CACHE_CHUNK_LINES);
    u32 base_idx = (u32)source_pool_state.cache_chunks.size() * SND_CACHE_CHUNK_LINES;
    for (u32 line_idx = 0; line_idx < SND_CACHE_CHUNK_LINES; line_idx++) {
        chunk[line_idx].start = 0;
        chunk[line_idx].end = 0;
        chunk[line_idx].timestamp = 0;
        chunk[line_idx].owner = nullptr;
    }

    source_pool_state.cache_chunks.push_back(chunk);
    source_pool_state.free_cache_lines.reserve(source_pool_state.cache_chunks.size() * SND_CACHE_CHUNK_LINES);
    for (u32 line_idx = 0; line_idx < SND_CACHE_CHUNK_LINES; line_idx++) {
        source_pool_state.free_cache_lines.push_back(base_idx + line_idx + 1);
    }

    SND_STAT_SET(snd_stats.cache_lines_total, (u32)source_pool_state.cache_chunks.size() * SND_CACHE_CHUNK_LINES);
    SND_STAT_SET(snd_stats.cache_lines_free, (u32)source_pool_state.free_cache_lines.size());
}

static void
Snd_PurgeCacheLine(u32 cache_idx, bool purge_from_entry)
{
    if (!Snd_IsCacheLineValid(cache_idx)) {
        return;
    }

    sound_cache_line_state* line = Snd_GetCacheLine(cache_idx);
    if (purge_from_entry && line->owner != nullptr) {
        u32* entries = line->owner->cache_lines;
        for (u32 entry_idx = 0; entry_idx < SND_CACHE_ENTRY_COUNT; entry_idx++) {
            if (entries[entry_idx] == cache_idx) {
                entries[entry_idx] = 0;
                break;
            }
        }
    }

    line->owner = nullptr;
    line->start = 0;
    line->end = 0;
    line->timestamp = 0;
    source_pool_state.free_cache_lines.push_back(cache_idx);
    SND_STAT_SET(snd_stats.cache_lines_free, (u32)source_pool_state.free_cache_lines.size());
}

static u32
Snd_NewCacheLine()
{
    if (source_pool_state.free_cache_lines.empty()) {
        u32 oldest_idx = 0;
        u64 oldest_timestamp = (u64)-1;
        u32 line_count = (u32)source_pool_state.cache_chunks.size() * SND_CACHE_CHUNK_LINES;
        for (u32 line_idx = 1; line_idx <= line_count; line_idx++) {
            u64 timestamp = Snd_GetCacheLine(line_idx)->timestamp;
            if (timestamp != 0 && timestamp < oldest_timestamp) {
                oldest_timestamp = timestamp;
                oldest_idx = line_idx;
            }
        }

        if (oldest_idx == 0 || (Snd_GetTimestamp() - oldest_timestamp) < SND_CACHE_LINE_MAX_TIME_NS) {
            Snd_GrowCacheLines();
        } else {
            Snd_PurgeCacheLine(oldest_idx, true);
        }
    }

    if (source_pool_state.free_cache_lines.empty()) {
        return 0;
    }

    u32 cache_idx = source_pool_state.free_cache_lines.back();
    source_pool_state.free_cache_lines.pop_back();
    Snd_GetCacheLine(cache_idx)->timestamp = Snd_GetTimestamp();
    SND_STAT_SET(snd_stats.cache_lines_free, (u32)source_pool_state.free_cache_lines.size());
    return cache_idx;
}

static u32
Snd_FindCacheLine(const sound_source_state* source, u32 position)
{
    PROF_EVENT("Sound: FindCacheLine");
    if (position >= source->desc.frames_total) {
        return 0;
    }

    u32 needed_frames = std::min((u32)SND_BLOCKSIZE, source->desc.frames_total - position);
    for (u32 entry_idx = 0; entry_idx < SND_CACHE_ENTRY_COUNT; entry_idx++) {
        u32 cache_idx = source->cache_lines[entry_idx];
        if (!Snd_IsCacheLineValid(cache_idx)) {
            continue;
        }

        const sound_cache_line_state* line = Snd_GetCacheLine(cache_idx);
        if (line->end >= line->start && position >= line->start && position + needed_frames <= line->end) {
            SND_STAT_ADD(snd_stats.cache_hit_count, 1u);
            return cache_idx;
        }
    }

    return 0;
}

static u32
Snd_SeekSource(sound_source_state* source, u32 position, bool precise)
{
    if (ov_pcm_tell(&source->file) != position) {
        if (precise) {
            ov_pcm_seek(&source->file, position);
        } else {
            ov_pcm_seek_page(&source->file, position);
        }
    }

    return (u32)ov_pcm_tell(&source->file);
}

static u32
Snd_ReadFromSource(sound_source_state* source, float** out_buffer, u32 frames)
{
    PROF_EVENT("Sound: Decode Vorbis");
    if (source->file.datasource == NULL) {
        return 0;
    }

    float** pcm = nullptr;
    int unused = 0;
    u32 remaining = frames;
    u32 offset = 0;
    u32 channel_count = std::min((u32)SND_CHANNEL_COUNT, (u32)source->desc.channels_count);

    while (remaining) {
        int status = ov_read_float(&source->file, &pcm, remaining, &unused);
        if (status == 0) {
            break;
        }

        R_ASSERT2(status >= 0, "Decoding error");
        remaining -= status;

        for (u32 channel_idx = 0; channel_idx < channel_count; channel_idx++) {
            for (int frame_idx = 0; frame_idx < status; frame_idx++) {
                out_buffer[channel_idx][offset + frame_idx] = std::clamp(pcm[channel_idx][frame_idx], -1.0f, 1.0f);
            }
        }

        offset += status;
    }

    if (source->desc.channels_count == 1) {
        memcpy(out_buffer[1], out_buffer[0], frames * sizeof(float));
    }

    return frames - remaining;
}

static void
Snd_UpdateCache(sound_source_state* source, u32 position)
{
    PROF_EVENT("Sound: Update Slot Cache");

    u32 new_idx = 0;
    {
        xrSRWLockGuard cache_guard(source_pool_state.lock, false);
        if (Snd_FindCacheLine(source, position) != 0 || source->file.datasource == NULL) {
            return;
        }

        SND_STAT_ADD(snd_stats.cache_miss_count, 1u);
        new_idx = Snd_NewCacheLine();
        if (!Snd_IsCacheLineValid(new_idx)) {
            return;
        }
    }

    sound_cache_line_state* line = Snd_GetCacheLine(new_idx);
    u32 begin_pos = Snd_SeekSource(source, position, false);
    if (begin_pos + SND_CACHE_LINE_CAPACITY < position + SND_BLOCKSIZE) {
        begin_pos = Snd_SeekSource(source, position, true);
    }

    memset(line->data, 0, sizeof(line->data));

    float* channel_data[SND_CHANNEL_COUNT];
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        channel_data[channel_idx] = line->data[channel_idx];
    }


    u32 end_pos = begin_pos + Snd_ReadFromSource(source, channel_data, SND_CACHE_LINE_CAPACITY);


    xrSRWLockGuard publish_guard(source_pool_state.lock, false);
    line->owner = source;
    line->start = begin_pos;
    line->end = end_pos;

    u32* oldest_entry = NULL;
    u64 oldest_timestamp = (u64)-1;
    for (u32 entry_idx = 0; entry_idx < SND_CACHE_ENTRY_COUNT; entry_idx++) {
        u32* entry = &source->cache_lines[entry_idx];
        if (*entry == 0 || *entry == new_idx) {
            *entry = new_idx;
            return;
        }

        if (Snd_IsCacheLineValid(*entry) && Snd_GetCacheLine(*entry)->timestamp < oldest_timestamp) {
            oldest_timestamp = Snd_GetCacheLine(*entry)->timestamp;
            oldest_entry = entry;
        }
    }

    if (oldest_entry != NULL) {
        Snd_PurgeCacheLine(*oldest_entry, false);
        *oldest_entry = new_idx;
    }
}

sound_source_state*
Snd_LookupSource(const xr_string* name)
{
    auto src = source_pool_state.sources.find(*name);
    return (src == source_pool_state.sources.end() || !src->second.is_ready) ? NULL : &src->second;
}

sound_source_state*
Snd_FindSource(const xr_string* name)
{
    xrSRWLockGuard guard(snd_source_lock, true);
    sound_source_state* source = Snd_LookupSource(name);
    if (source != NULL) {
        source->desc.ref_count++;
    }

    return source;
}

sound_source_state*
Snd_AcquireSource(const xr_string* name)
{
    sound_source_state* source = Snd_FindSource(name);
    if (source != NULL || name->empty()) {
        return source;
    }

    PROF_EVENT("Sound: Load ogg");
    bool is_loader = false;
    while (!is_loader) {
        {
            xrSRWLockGuard guard(snd_source_lock);
            source = &source_pool_state.sources[*name];
            if (source->is_ready) {
                source->desc.ref_count++;
                return source;
            }
            if (!source->is_loading) {
                source->is_loading = true;
                is_loader = true;
            }
        }

        if (!is_loader) {
            std::this_thread::yield();
        }
    }

    string_path path, base_name;
    xr_strcpy(base_name, name->c_str());
    _strlwr(base_name);

    char* ext = strext(base_name);
    if (ext != NULL) {
        *ext = 0;
    }

    source->desc.name = base_name;

    xr_strconcat(path, base_name, ".ogg");
    if (!FS.exist("$level$", path)) {
        FS.update_path(path, _game_sounds_, path);
    } if (!FS.exist(path)) {
        FS.update_path(path, _game_sounds_, "$no_sound.ogg");
        Msg("! Can't find sound '%s'", source->desc.name.c_str());
    }
    source->desc.path = path;

    IReader* wave_file = FS.r_open(path);
    R_ASSERT3(wave_file && wave_file->length(), "Can't open wave file:", path);

    source->desc.data_size = wave_file->length();
    source->data = xr_alloc<u8>(source->desc.data_size);
    wave_file->r(source->data, wave_file->length());
    wave_file->close();
    source->reader = new IReader(source->data, source->desc.data_size);

    ov_callbacks callbacks = { ov_read_func, ov_seek_func, ov_close_func, ov_tell_func };
    ov_open_callbacks(source->reader, &source->file, NULL, 0, callbacks);

    vorbis_info* info = ov_info(&source->file, -1);
    R_ASSERT3(info, "Invalid source info:", source->desc.name.c_str());
    R_ASSERT(info->rate == SND_SAMPLERATE, "Invalid sample rate. Please, convert to 44100 Hz using converters like FFmpeg or foobar2000", name->c_str());

    source->desc.channels_count = info->channels;
    source->desc.frames_total = (u32)ov_pcm_total(&source->file, -1);
    source->desc.volume = 1.0f;
    source->desc.min_distance = 1.0f;
    source->desc.max_distance = 300.0f;
    source->desc.max_ai_distance = 300.0f;

    bool is_valid = false;
    vorbis_comment* comment = ov_comment(&source->file, -1);
    for (int comment_idx = 0; comment != NULL && !is_valid && comment_idx < comment->comments; comment_idx++) {
        int comment_length = comment->comment_lengths[comment_idx];
        if (comment->user_comments[comment_idx] == NULL || comment_length < 16) {
            continue;
        }

        IReader reader(comment->user_comments[comment_idx], comment_length);
        u32 version_id = reader.r_u32();
        for (u32 decl_idx = 0; decl_idx < sizeof(ogg_comment_decls) / sizeof(ogg_comment_decls[0]); decl_idx++) {
            const ogg_comment_decl* decl = &ogg_comment_decls[decl_idx];
            if (version_id != decl->version || reader.elapsed() < decl->min_size) {
                continue;
            }

            source->desc.min_distance = reader.r_float();
            source->desc.max_distance = reader.r_float();
            source->desc.volume = decl->has_volume ? reader.r_float() : 1.0f;
            source->desc.game_type = reader.r_u32();
            source->desc.max_ai_distance = decl->has_ai_distance ? reader.r_float() : 300.0f;
            is_valid = true;
            break;
        }
    }

    if (!is_valid) {
        Msg("~ Missing or invalid ogg-comment, file: %s", source->desc.name.c_str());
    }

    source->desc.volume = std::min(source->desc.volume, 1.0f);
    if (source->desc.min_distance < EPS_S) {
        source->desc.min_distance = 1.0f;
    } if (source->desc.max_distance < source->desc.min_distance) {
        source->desc.max_distance = source->desc.min_distance + 1.0f;
    } if (source->desc.max_ai_distance < EPS_S) {
        source->desc.max_ai_distance = source->desc.max_distance;
    }

    xrSRWLockGuard ready_guard(snd_source_lock);
    source->is_ready = true;
    source->is_loading = false;
    source->desc.ref_count++;
    return source;
}

void
Snd_ReleaseSource(const xr_string* name)
{
    if (name->empty()) {
        return;
    }

    {
        xrSRWLockGuard guard(snd_source_lock, true);
        sound_source_state* source = Snd_LookupSource(name);
        if (source == NULL) {
            return;
        }

        R_ASSERT(source->desc.ref_count);
        if (--source->desc.ref_count != 0) {
            return;
        }
    }

    xrSRWLockGuard guard(snd_source_lock);
    auto found_source = source_pool_state.sources.find(*name);
    if (found_source == source_pool_state.sources.end() || found_source->second.desc.ref_count != 0) {
        return;
    }
    sound_source_state* source = &found_source->second;

    ov_clear(&source->file);
    xr_delete(source->reader);
    xr_free(source->data);

    xrSRWLockGuard cache_guard(source_pool_state.lock, false);
    for (u32 entry_idx = 0; entry_idx < SND_CACHE_ENTRY_COUNT; entry_idx++) {
        u32 cache_idx = source->cache_lines[entry_idx];
        if (Snd_IsCacheLineValid(cache_idx) && Snd_GetCacheLine(cache_idx)->owner == source) {
            Snd_PurgeCacheLine(cache_idx, false);
        }

        source->cache_lines[entry_idx] = 0;
    }

    source_pool_state.sources.erase(found_source);
}

void
Snd_QueueDecode(const xr_string* name, u32 position)
{
    if (name->empty()) {
        return;
    }

    {
        std::lock_guard<std::mutex> guard(source_pool_state.decode_mutex);
        for (size_t request_idx = 0; request_idx < source_pool_state.decode_queue.size(); request_idx++) {
            const sound_decode_command* queued = &source_pool_state.decode_queue[request_idx];
            if (queued->position == position && queued->name == *name) {
                return;
            }
        }

        source_pool_state.decode_queue.push_back({ *name, position });
    }

    source_pool_state.decode_cv.notify_one();
}

bool
Snd_HasCacheLine(sound_source_state* source, u32 position)
{
    xrSRWLockGuard cache_guard(source_pool_state.lock, true);
    return Snd_FindCacheLine(source, position) != 0;
}

u32
Snd_CopyCached(sound_source_state* source, u32 position, float** out_data, u32 frames)
{
    xrSRWLockGuard cache_guard(source_pool_state.lock, true);
    u32 cache_idx = Snd_FindCacheLine(source, position);
    if (!Snd_IsCacheLineValid(cache_idx)) {
        return 0;
    }

    const sound_cache_line_state* line = Snd_GetCacheLine(cache_idx);
    if (position < line->start || line->end <= line->start || position >= line->end || position - line->start >= SND_CACHE_LINE_CAPACITY) {
        return 0;
    }

    u32 begin_offset = position - line->start;
	u32 frames_count = std::min(std::min(frames, line->end - position), SND_CACHE_LINE_CAPACITY - begin_offset);
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        memcpy(out_data[channel_idx], &line->data[channel_idx][begin_offset], frames_count * sizeof(float));
    }

    return frames_count;
}

static void
Snd_DecodeThreadProc(void*)
{
    PROF_THREAD("Sound Decode Thread");

    for (;;) {
        sound_decode_command request;

        {
            std::unique_lock<std::mutex> guard(source_pool_state.decode_mutex);
            while (source_pool_state.decode_queue.empty() && !source_pool_state.decode_stop) {
                source_pool_state.decode_cv.wait(guard);
            }
            if (source_pool_state.decode_stop) {
                return;
            }

            request = source_pool_state.decode_queue.front();
            source_pool_state.decode_queue.erase(source_pool_state.decode_queue.begin());
        }

        PROF_EVENT("Decode OGG");
        sound_source_state* source = Snd_FindSource(&request.name);
        if (source != NULL) {
            Snd_UpdateCache(source, request.position);
            Snd_ReleaseSource(&request.name);
        }
    }
}

static void
Snd_FreeCacheChunks()
{
    for (size_t chunk_idx = 0; chunk_idx < source_pool_state.cache_chunks.size(); chunk_idx++) {
        xr_free(source_pool_state.cache_chunks[chunk_idx]);
    }

    source_pool_state.cache_chunks.clear();
    source_pool_state.free_cache_lines.clear();
}

void
Snd_InitSources()
{
    Snd_FreeCacheChunks();
    Snd_GrowCacheLines();

    source_pool_state.decode_stop = false;
    source_pool_state.decode_thread = thread_spawn(Snd_DecodeThreadProc, "Sound Decode Thread", 0, NULL);
}

void
Snd_ShutdownSources()
{
    {
        std::lock_guard<std::mutex> guard(source_pool_state.decode_mutex);
        source_pool_state.decode_stop = true;
    }

    source_pool_state.decode_cv.notify_all();
    if (source_pool_state.decode_thread) {
        Platform::WaitForSingleObject(source_pool_state.decode_thread);
        source_pool_state.decode_thread = 0;
    }

    source_pool_state.decode_queue.clear();
    for (auto& source : source_pool_state.sources) {
        memset(source.second.cache_lines, 0, sizeof(source.second.cache_lines));
    }

    Snd_FreeCacheChunks();
}

u32
XRay::Sound::Mixer::GetSourceCount()
{
    xrSRWLockGuard guard(snd_source_lock, true);
    return (u32)source_pool_state.sources.size();
}

const sound_source_desc*
XRay::Sound::Mixer::GetSource(u32 index)
{
    xrSRWLockGuard guard(snd_source_lock, true);
    u32 ready_idx = 0;
    for (auto& source : source_pool_state.sources) {
        if (source.second.is_ready && ready_idx++ == index) {
            return &source.second.desc;
        }
    }

    return NULL;
}
