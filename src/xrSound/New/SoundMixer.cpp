/**************************************************************************************
* Copyright (C) 2025 Anton Kovalev (vertver)
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
#include "SoundMixer.h"
#include "SoundMixerInternal.h"
#include "SoundBackend.h"
#include "SoundDSP.h"
#include "../Sound.h"
#include "../SoundRender.h"
#include "../ai_sounds.h"
#include "pffft.h"

#ifndef DISABLE_STEAM_AUDIO
#   include <SteamAudio/phonon.h>
#elif !defined(DISABLE_RESONANCE_AUDIO)
#	include "../../3rd-party/resonance-audio/resonance_audio/api/binaural_surround_renderer.h"
#	include "../../3rd-party/resonance-audio/resonance_audio/api/resonance_audio_api.h"
#endif

#include "ogg_utils.h"

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#define SND_HRTF_SLOT_COUNT (512)
#define DEFAULT_SLOT_COUNT (512)
#define SND_MAX_PITCH (4)
#define CACHE_LINES_COUNT (1024)
#define CACHE_LINE_WIDTH (12)
#define CACHE_LINE_ENTRY_COUNT (32)
#define CACHE_LINE_MAX_TIME_NS (1000000000)

using namespace XRay::Sound;
enum class sound_cmd_id : u16
{
	invalid,
	play,
	pause,
	stop,
	destroy,
	stop_all,
	pause_all,
	resume_all,
	update_parameter,
	set_volume,
	set_panning
};

struct sound_command
{
	u32 slot;
	sound_cmd_id id;
	u16 param0;
	u64 param1;
	u64 param2;
	u64 param3;
	shared_str string_storage;
};

struct sound_source
{
	sound_source_public pub;

	OggVorbis_File file;
	IReader* reader;
	u8* data;
	u32 cache_lines[CACHE_LINE_ENTRY_COUNT];
};

struct sound_cache_line
{
	u32 start;
	u32 end;
	u64 timestamp;
	shared_str name;
	float data[SND_CHANNEL_COUNT][(SND_BLOCKSIZE+1) * CACHE_LINE_WIDTH];
};

struct sound_hrtf_slot_state
{
	// Common process buffers for any HRTF backend
#ifndef DISABLE_STEAM_AUDIO
	float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
	float* process_buffer[SND_CHANNEL_COUNT];

	IPLAudioBuffer buf_desc;
	IPLBinauralEffect effect;
#elif !defined(DISABLE_RESONANCE_AUDIO)
	vraudio::ResonanceAudioApi::SourceId source_id = vraudio::ResonanceAudioApi::kInvalidSourceId;
#endif
};

struct sound_bus_state 
{
	float data[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
};

struct sound_mixer_state
{
	xrSRWLock render_lock;
	xrSRWLock update_lock;
	xrSRWLock manage_lock;
	xrSRWLock source_lock;
	xrCriticalSection play_lock;

	sound_stats stats = { 0 };
	float dt;
	float time_factor = 1.0f;
	float master_volume = 0.0f;
	float effect_volume = 0.0f;
	float music_volume = 0.0f;
	float shooting_volume = 0.0f;
	float compression = 0.0f;
	float compressor_envelope[SND_CHANNEL_COUNT] = { FLT_EPSILON, FLT_EPSILON };
	Fvector P, D, N;             
	Fvector occ;
	Fmatrix m_V;

	xr_vector<u32> free_slots;
	xr_vector<u32> free_cachelines;
	xr_vector<sound_command> cmd;
	xr_vector<sound_slot_state> slots;
	xr_vector<sound_cache_line> cache_lines;
	xr_hash_set<ref_sound*> sounds;
	xr_hash_map<xr_string, sound_source> snd_sources;
	xr_vector<sound_zone_params> zones;

	// HRTF slot management (used by either SteamAudio or Resonance)
	xr_vector<u32> free_hrtf_slots;
	xr_vector<sound_hrtf_slot_state> hrtf_slots;

	sound_bus_state buses[SND_BUS_COUNT];

	bool editor_zone = false;
	bool hrtf_enabled;
#ifndef DISABLE_STEAM_AUDIO
	IPLContext ipl_context;
	IPLHRTF ipl_hrtf;
#elif !defined(DISABLE_RESONANCE_AUDIO)
	vraudio::ResonanceAudioApi* ra_api = nullptr;
#endif

#ifdef DEBUG_DRAW
	PFFFT_Setup* fft_setup;
	float* aligned_input_fft;
	float* aligned_output_fft;
	float fft_window[SND_BLOCKSIZE];
#endif

	float read_buffer[SND_CHANNEL_COUNT][(SND_BLOCKSIZE+1) * 10];
};

static sound_mixer_state GMixer = {};

#ifndef DISABLE_STEAM_AUDIO
void ConvertReverbSettings(
	const sound_reverb_settings* reverb_in,
	IPLReflectionEffectParams* reverb_out,
	IPLint32 sampleRate)
{
	// Set the effect type to parametric, as it aligns with the EAX parameters.
	reverb_out->type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC;

	// Map decay times.
	// Mid-frequency decay time (RT60)
	reverb_out->reverbTimes[1] = reverb_in->decay_time;
	// High-frequency decay time (RT60)
	reverb_out->reverbTimes[2] = reverb_in->decay_time * reverb_in->decay_hf_ratio;
	// Low-frequency decay time (RT60) - approximation
	reverb_out->reverbTimes[0] = reverb_in->decay_time;

	// Map the reverb delay to samples.
	// This assumes reverb_in->reverb_delay is in seconds. If it's in ms, divide by 1000.
	reverb_out->delay = (IPLint32)(reverb_in->reverb_delay * sampleRate);

	// Other parameters not directly mapped or are handled by the SDK's internal logic.
	// For example, 'air_absorption_hf' can influence EQ or filter settings,
	// and 'room' or 'reverb' are gain controls often applied at a higher level.
}

static IPLReflectionEffectParams
Snd_EngineToIPLParams(const sound_reverb_settings& settings)
{
	constexpr float duration = 4.0f;

	IPLReflectionEffectParams params = { .type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC };
	
	float decay_time = std::max(settings.decay_time, 0.1f);
	float hf_ratio = std::clamp(settings.decay_hf_ratio, 0.1f, 2.0f);

	params.reverbTimes[1] = decay_time;
	params.reverbTimes[2] = decay_time * hf_ratio;
	params.reverbTimes[0] = decay_time * (2.0f - hf_ratio);

	params.delay = static_cast<IPLint32>(std::round(std::clamp(settings.reverb_delay, 0.0f, 10.0f) * SND_SAMPLERATE));

	float room_hf_clamped = std::clamp(settings.room_hf, -10000.0f, 0.0f);
	float hf_gain = powf(10.0f, room_hf_clamped / 2000.0f);

	params.eq[0] = 1.0f;       // low
	params.eq[1] = 1.0f;       // mid
	params.eq[2] = hf_gain;    // high

	params.irSize = SND_SAMPLERATE * duration;
	params.numChannels = SND_CHANNEL_COUNT * SND_CHANNEL_COUNT;

	return params;
}
#endif

static void
Snd_GrowCacheLines(bool lock_render)
{
	if (lock_render) {
		GMixer.render_lock.AcquireExclusive();
		GMixer.manage_lock.AcquireExclusive();
	}

	xrSRWLockGuard guard0(GMixer.update_lock, false);
	bool locked = !GMixer.source_lock.TryAcquireExclusive();

	size_t old_cache_lines = GMixer.cache_lines.size();
	size_t new_cache_lines = std::max((size_t)CACHE_LINES_COUNT, GMixer.cache_lines.size() * 2);

	GMixer.cache_lines.resize(new_cache_lines);
	GMixer.free_cachelines.reserve(new_cache_lines);

	for (size_t i = old_cache_lines; i < new_cache_lines; i++) {
		GMixer.free_cachelines.push_back(i + 1);
	}

	if (lock_render) {
		GMixer.render_lock.ReleaseExclusive();
		GMixer.manage_lock.ReleaseExclusive();
	}

	if (!locked) {
		GMixer.source_lock.ReleaseExclusive();
	}

	GMixer.stats.cache_lines_total = new_cache_lines;
}

static void
Snd_GrowSlots(bool lock_update)
{
	xrSRWLockGuard guard0(GMixer.render_lock, false);
	xrSRWLockGuard guard1(GMixer.manage_lock, false);
	bool locked = !GMixer.source_lock.TryAcquireExclusive();

	if (lock_update) {
		GMixer.update_lock.AcquireExclusive();
	}

	size_t old_size = GMixer.slots.size();
	size_t new_size = std::max((size_t)DEFAULT_SLOT_COUNT, GMixer.slots.size() * 2);

	GMixer.slots.resize(new_size);
	GMixer.free_slots.reserve(new_size);

	GMixer.stats.possible_free_count += (new_size - old_size);
	for (size_t i = old_size; i < new_size; i++) {
		GMixer.free_slots.push_back(i + 1);
	}

	if (lock_update) {
		GMixer.update_lock.ReleaseExclusive();
	}

	if (!locked) {
		GMixer.source_lock.ReleaseExclusive();
	}
}

ICF void Snd_AcquireHRTFSlot(u32 slot_idx)
{
	if (!GMixer.hrtf_enabled) return;

	auto& slot = GMixer.slots[slot_idx - 1];
	if ((slot.flags & (u16)Mixer::Flags::Spatial) == 0) {
		return;
	}

	if (slot.hrtf_slot) {
		return;
	}

	if (GMixer.free_hrtf_slots.empty()) {
		return;
	}

	slot.hrtf_slot = GMixer.free_hrtf_slots[GMixer.free_hrtf_slots.size() - 1];
	GMixer.free_hrtf_slots.pop_back();

#ifndef DISABLE_STEAM_AUDIO
	if (GMixer.ipl_hrtf != nullptr) {
		iplBinauralEffectReset(GMixer.hrtf_slots[slot.hrtf_slot - 1].effect);
	}
#endif
}

ICF void Snd_ReleaseHRTFSlot(u32 slot_idx)
{
	if (!GMixer.hrtf_enabled) return;

	auto& slot = GMixer.slots[slot_idx - 1];
	if ((slot.flags & (u16)Mixer::Flags::Spatial) == 0) {
		return;
	}

	if (slot.hrtf_slot) {
		GMixer.free_hrtf_slots.emplace_back(slot.hrtf_slot);
#ifndef DISABLE_RESONANCE_AUDIO
		// Optionally reset resonance source state (no explicit reset API)
		if (GMixer.ra_api != nullptr) {
			// we keep the source around and overwrite its buffer next use
		}
#endif
		slot.hrtf_slot = 0;
	}
}

void
MixerNewState(u32 slot, Mixer::State state)
{
	if (slot == 0) {
		return;
	}

	GMixer.slots[slot - 1].prev_state = GMixer.slots[slot - 1].state;
	GMixer.slots[slot - 1].state = state;
	GMixer.slots[slot - 1].fake_state = state;
}

#define in_range(x, start, end) ((x) >= start && (x) <= end)

ICF u64 Snd_GetTimestamp()
{
	return std::chrono::high_resolution_clock::now().time_since_epoch().count();
}

ICF u32 Snd_Milliseconds()
{
	return (float)((Snd_GetTimestamp()) / 1000000);
}

ICF void Snd_PurgeCacheLine(u32 cache_idx, bool purge_from_entry)
{
	auto& line = GMixer.cache_lines[cache_idx - 1];

	if (purge_from_entry && line.name.size()) {
		auto found_source = GMixer.snd_sources.find(line.name.c_str());
		if (found_source != GMixer.snd_sources.end()) {
			for (u32& entry_cache_idx : found_source->second.cache_lines) {
				if (entry_cache_idx == cache_idx) {
					entry_cache_idx = 0;
					break;
				}
			}
		}
	}

	line.name = nullptr;
	memset(&line, 0, sizeof(line));
	GMixer.free_cachelines.push_back(cache_idx);
}

ICF void Snd_DestroySourceCache(sound_source& source)
{
	if (source.pub.ref_count == 0) {
		for (u32& cache_idx : source.cache_lines) {
			if (cache_idx != 0 && GMixer.cache_lines[cache_idx - 1].name.size()) {
				Snd_PurgeCacheLine(cache_idx, false);
				cache_idx = 0;
			}
		}
	}
}

ICF u32 Snd_NewCacheLine()
{
	u32 cache_idx = 0;
	if (GMixer.free_cachelines.empty())
	{
		u64 least_timestamp = (u64)-1;
		for (size_t i = 0; i < GMixer.cache_lines.size(); i++)
		{
			sound_cache_line& Line = GMixer.cache_lines[i];

			if (Line.timestamp == 0)
			{
				// TODO: this might be a problem in the future
				continue;
			}

			if (Line.timestamp < least_timestamp)
			{
				least_timestamp = Line.timestamp;
				cache_idx = i + 1;
				continue;
			}
		}

		if (cache_idx == 0 || (Snd_GetTimestamp() - least_timestamp) < CACHE_LINE_MAX_TIME_NS)
		{
			Snd_GrowCacheLines(false);
		}
		else
		{
			Snd_PurgeCacheLine(cache_idx, true);
		}
	}

	{
		cache_idx = GMixer.free_cachelines[GMixer.free_cachelines.size() - 1];
		GMixer.free_cachelines.pop_back();
		GMixer.cache_lines[cache_idx - 1].timestamp = Snd_GetTimestamp();
	}
	return cache_idx;
}

[[maybe_unused]] static u32
Snd_TellSource(sound_source& source)
{
	return ov_pcm_tell(&source.file);
}

ICF u32 Snd_SeekSource(sound_source& source, u32 position, bool precise)
{
	if (ov_pcm_tell(&source.file) != position) {
		if (precise) {
			ov_pcm_seek(&source.file, position);
		} else {
			ov_pcm_seek_page(&source.file, position);
		}
	}

	return ov_pcm_tell(&source.file);
}

ICF void Snd_LoadSource(sound_source& source, const char* name)
{
	PROF_EVENT("Sound: Load ogg");

	string_path fn, N;
	xr_strcpy(N, name);
	_strlwr(N);
	if (strext(N)) *strext(N) = 0;
	source.pub.name = N;

	xr_strconcat(fn, N, ".ogg");
	if (!FS.exist("$level$", fn))	FS.update_path(fn, _game_sounds_, fn);
	if (!FS.exist(fn)) {
		FS.update_path(fn, _game_sounds_, "$no_sound.ogg");
		Msg("! Can't find sound '%s'", source.pub.name.c_str());
	}

	source.pub.path = fn;
	IReader* m_wavefile = FS.r_open(source.pub.path.c_str());
	R_ASSERT3(m_wavefile && m_wavefile->length(), "Can't open wave file:", source.pub.path.c_str());
	if (source.data != nullptr) {
		xr_free(source.data);
	}

	source.pub.data_size = m_wavefile->length();
	source.data = xr_alloc<u8>(source.pub.data_size);
	m_wavefile->r(source.data, m_wavefile->length());
	m_wavefile->close();
	xr_delete(source.reader);
	source.reader = new IReader(source.data, source.pub.data_size);

	ov_callbacks ovc;
	ovc.read_func = ov_read_func;
	ovc.seek_func = ov_seek_func;
	ovc.close_func = ov_close_func;
	ovc.tell_func = ov_tell_func;
	ov_open_callbacks(source.reader, &source.file, nullptr, 0, ovc);

	vorbis_info* ovi = ov_info(&source.file, -1);
	R_ASSERT3(ovi, "Invalid source info:", source.pub.name.c_str());
	R_ASSERT(ovi->rate == SND_SAMPLERATE, "Invalid sample rate. Please, convert to 44100 Hz using converters like FFmpeg or foobar2000", name);
	source.pub.channels_count = ovi->channels;
	source.pub.frames_total = ov_pcm_total(&source.file, -1);
	source.pub.volume = 1.f;
	source.pub.min_distance = 1.0f;
	source.pub.max_distance = 300.0f;
	source.pub.max_ai_distance = 300.0f;

	vorbis_comment* ovm = ov_comment(&source.file, -1);
	if (ovm->comments) {
		IReader F(ovm->user_comments[0], ovm->comment_lengths[0]);
		u32 vers = F.r_u32();
		if (vers == 0x0001) {
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = 1.0f;
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = 300.0f;
		} else if (vers == 0x0002) {
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = F.r_float();
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = 300.0f;
		} else if (vers == OGG_COMMENT_VERSION) {
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = F.r_float();
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = F.r_float();
		} else {
			Msg("! Invalid ogg-comment version, file: %s", source.pub.name.c_str());
		}
	} else {
		Msg("~ Missing ogg-comment, file: %s", source.pub.name.c_str());
	}

	source.pub.volume = std::min(source.pub.volume, 1.0f);
}

ICF sound_source* Snd_FindSound(const xr_string& name)
{
	if (name.empty()) {
		return nullptr;
	}

	xrSRWLockGuard guard(GMixer.source_lock, true);
	auto found_source = GMixer.snd_sources.find(name);
	if (found_source == GMixer.snd_sources.end()) {
		return nullptr;
	}

	found_source->second.pub.ref_count++;
	return &found_source->second;
}

ICF sound_source* Snd_AcquireSound(const xr_string& name, bool fail_if_not_found)
{
	sound_source* source = Snd_FindSound(name);
	if (source != nullptr || name.empty()) {
		return source;
	}

	R_ASSERT(!fail_if_not_found);

	// TODO: async file load?
	xrSRWLockGuard guard(GMixer.source_lock);
	source = &GMixer.snd_sources[name];
	if (source->reader == nullptr) {
		Snd_LoadSource(*source, name.c_str());
	}

	source->pub.ref_count++;
	return source;
}

ICF void Snd_ReleaseSound(const xr_string& name)
{
	if (name.empty()) {
		return;
	}

	{
		xrSRWLockGuard guard(GMixer.source_lock, true);
		auto found_source = GMixer.snd_sources.find(name);
		if (found_source == GMixer.snd_sources.end()) {
			return;
		}

		R_ASSERT(found_source->second.pub.ref_count);
		if (--found_source->second.pub.ref_count != 0) {
			return;
		}
	}

	xrSRWLockGuard guard(GMixer.source_lock);
	auto found_source = GMixer.snd_sources.find(name);
	if (found_source == GMixer.snd_sources.end() || found_source->second.pub.ref_count != 0) {
		return;
	}

	auto& source = found_source->second;
	ov_clear(&source.file);
	xr_delete(source.reader);
	xr_free(source.data);

	Snd_DestroySourceCache(source);
	GMixer.snd_sources.erase(found_source);
}

ICF u32 Snd_ReadFromSource(sound_source& source, float** buffer, u32 frames)
{
	PROF_EVENT("Sound: Decode Vorbis");

	if (source.file.datasource == nullptr) {
		return 0;
	}

	float** pcm; int section; u32 last_frames = frames;
	u32 offset = 0;
	do {
		int status = ov_read_float(&source.file, &pcm, last_frames, &section);
		if (status == 0) {
			break;
		} else {
			R_ASSERT2(status >= 0, "Decoding error");
			last_frames -= status;
		}

		for (size_t ch = 0; ch < std::min((u8)SND_CHANNEL_COUNT, source.pub.channels_count); ch++) {
			for (size_t idx = 0; idx < status; idx++) {
				buffer[ch][offset + idx] = std::clamp(pcm[ch][idx], -1.0f, 1.0f);
			}
		}

		offset += status;
	} while (last_frames);

	if (source.pub.channels_count == 1) {
		memcpy(buffer[1], buffer[0], frames * sizeof(float));
	}


	return frames - last_frames;
}

enum class ESlotOcclusionResult
{
	False,
	True,
	SOM
};

ICF ESlotOcclusionResult Snd_SlotOcclusion(u32 slot_idx, const sound_source& source, float dt, float* occ_volume)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	if (slot.state != Mixer::State::Playing)
	{
		return ESlotOcclusionResult::False;
	}

	ESlotOcclusionResult Result = ESlotOcclusionResult::True;

	if (source.pub.channels_count == 1 && slot.flags & (u32)Mixer::Flags::Spatial)
	{
		// Check range
		Fvector& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
		Fvector& distances = slot.parameters[(u32)Mixer::ParameterId::DistanceRange];
		float dist = GMixer.P.distance_to(pos);
		if (dist > distances.y)
		{
			if (occ_volume)
			{
				*occ_volume = 0.0f;
			}
			return ESlotOcclusionResult::False;
		}

		if (occ_volume != nullptr)
		{
			float occ = Sound->get_occlusion_to(GMixer.P, pos);
			clamp(occ, 0.f, 1.f);
			*occ_volume = occ;

			if (occ < 0.01f)
			{
				Result = ESlotOcclusionResult::SOM;
			}
		}
	}

	return Result;
}

ICF u32 Snd_FindAvailableCacheLine(sound_source& source, u32 position)
{
	u32 needed_frames = std::min((u32)SND_BLOCKSIZE, source.pub.frames_total - position);
	u32 found_cache_idx = 0;
	for (const u32& cache_idx : source.cache_lines) {
		if (cache_idx != 0) {
			auto& line = GMixer.cache_lines[cache_idx - 1];
			if (in_range(position, line.start, line.end) && in_range(position + needed_frames, line.start, line.end)) {
				found_cache_idx = cache_idx;
				GMixer.stats.cache_hit_count++;
				break;
			}
		}
	}

	return found_cache_idx;
}

// 'position' is a source frame index, not necessarily slot.position
ICF void Snd_UpdateCache(sound_source& source, u32 position)
{
	PROF_EVENT("Sound: Update Slot Cache");

	{
		xrSRWLockGuard guard2(GMixer.source_lock, true);
		u32 found_cache_idx = Snd_FindAvailableCacheLine(source, position);
		if (found_cache_idx == 0 && source.file.datasource != nullptr) {
			GMixer.stats.cache_miss_count++;
			found_cache_idx = Snd_NewCacheLine();
			auto& line = GMixer.cache_lines[found_cache_idx - 1];

			u32 cache_size = ((SND_BLOCKSIZE + 1) * CACHE_LINE_WIDTH);

			{
				// TODO: parallel decoding for each slot
				u32 begin_pos = Snd_SeekSource(source, position, false);
				u32 end_pos = begin_pos + cache_size;
				if (end_pos < (position + SND_BLOCKSIZE)) {
					begin_pos = Snd_SeekSource(source, position, true);
				}

				memset(line.data, 0, sizeof(line.data));

				float* ch_data[SND_CHANNEL_COUNT];
				for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
					ch_data[i] = line.data[i];
				}
				end_pos = begin_pos + Snd_ReadFromSource(source, ch_data, cache_size);

				//VERIFY(in_range(slot.position, begin_pos, end_pos));
				line.name = source.pub.name;
				line.start = begin_pos;
				line.end = end_pos;
			}

			bool inserted = false;
			for (u32& cache_idx : source.cache_lines) {
				if (cache_idx == found_cache_idx) {
					inserted = true;
					break;
				}

				if (cache_idx == 0) {
					cache_idx = found_cache_idx;
					inserted = true;
					break;
				}
			}

			if (!inserted) {
				u64 least_timestamp = (u64)-1;
				u32 cache_entry_idx = 0;
				for (size_t i = 0; i < CACHE_LINE_ENTRY_COUNT; i++) {
					if (source.cache_lines[i] == 0 || source.cache_lines[i] == found_cache_idx) {
						continue;
					}

					if (GMixer.cache_lines[source.cache_lines[i] - 1].timestamp < least_timestamp) {
						least_timestamp = GMixer.cache_lines[source.cache_lines[i] - 1].timestamp;
						cache_entry_idx = i + 1;
						continue;
					}
				}

				if (cache_entry_idx != 0) {
					Snd_PurgeCacheLine(source.cache_lines[cache_entry_idx - 1], false);
					source.cache_lines[cache_entry_idx - 1] = found_cache_idx;
				} else {
					Snd_GrowCacheLines(false);
				}
			}
		}
	}
}

ICF u32 Snd_ReadSlotData(u32 slot_idx, sound_source& source, float** data, u32 frames_count)
{
	auto& slot = GMixer.slots[slot_idx - 1];

	u32 read_position = slot.position;
	u32 frames_to_read = frames_count;

	while (frames_to_read && read_position < source.pub.frames_total) {
		u32 found_cache_idx = Snd_FindAvailableCacheLine(source, read_position);

		if (found_cache_idx == 0) {
			Snd_UpdateCache(source, read_position);
			found_cache_idx = Snd_FindAvailableCacheLine(source, read_position);
			VERIFY(found_cache_idx != 0);
		}

		if (found_cache_idx == 0) {
			break;
		}

		auto& cache_line = GMixer.cache_lines[found_cache_idx - 1];
		u32 begin_offset = read_position - cache_line.start;
		u32 cache_frames = std::min(frames_to_read, cache_line.end - read_position);
		if (cache_frames == 0) {
			break;
		}

		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			memcpy(&data[ch][frames_count - frames_to_read], &cache_line.data[ch][begin_offset], cache_frames * sizeof(float));
		}

		frames_to_read -= cache_frames;
		read_position += cache_frames;
	}

	return frames_count - frames_to_read;
}

ICF void Snd_ReadSlot(u32 slot_idx, sound_source& source, float** data, u32 frames_count)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	if (source.pub.frames_total == 0) {
		MixerNewState(slot_idx, Mixer::State::Stopped);
		return;
	}

	u32 last_frames = frames_count;
	while (last_frames) {
		float* offfseted_data[SND_CHANNEL_COUNT];
		u32 buf_offset = (frames_count - last_frames);
		for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
			offfseted_data[i] = &data[i][buf_offset];
		}

		u32 read_frames = Snd_ReadSlotData(slot_idx, source, offfseted_data, last_frames);

		last_frames -= read_frames;
		slot.position = std::min(slot.position + read_frames, source.pub.frames_total);

		if (slot.position < source.pub.frames_total) {
			if (read_frames == 0) {
				MixerNewState(slot_idx, Mixer::State::Stopped);
				break;
			}

			continue;
		}

		slot.position = 0;
		if ((slot.flags & (u32)Mixer::Flags::Looped) == 0) {
			MixerNewState(slot_idx, Mixer::State::Stopped);
			break;
		}
	}
}

ICF void Snd_ProcessSlot(u32 slot_idx, sound_source& source, float** data)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	float pitch = slot.parameters[(u32)Mixer::ParameterId::Pitch].x;

	u32 output_frames = SND_BLOCKSIZE;
	float ratio = std::clamp(pitch * GMixer.time_factor, 0.0f, (float)SND_MAX_PITCH);
	u32 input_frames = std::max((u32)((float)output_frames * ratio), 1u);

	bool is_music = (slot.flags & (u16)Mixer::Flags::Intro);

	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		memset(GMixer.read_buffer[ch], 0, (input_frames + 1) * sizeof(float));
	}

	if (is_music || fis_zero(1.0 - GMixer.time_factor)) {
		Snd_ReadSlot(slot_idx, source, data, SND_BLOCKSIZE);
	} else {
		float* offfseted_data[SND_CHANNEL_COUNT];
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			offfseted_data[ch] = GMixer.read_buffer[ch];
		}

		Snd_ReadSlot(slot_idx, source, offfseted_data, input_frames+1);
		if (slot.position > 0) {
			slot.position -= 1; // account 1 sample of tail for interpolation
		}

		DSP_ResampleBuffer(offfseted_data, data, slot.history, input_frames, output_frames);
	}
}

ICF void Snd_PrecacheRenderCallback()
{
	PROF_EVENT("Sound: Precache Stage");

	static u64 counter = 0;
	static u64 timestamp = Snd_GetTimestamp();

	xrSRWLockGuard g1(GMixer.manage_lock);
	float dt = (float)((double)(Snd_GetTimestamp() - timestamp) / 1000000000.0);

	GMixer.stats.frame_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
	timestamp = Snd_GetTimestamp();

	if (counter % 100 == 0) {
		GMixer.stats.cache_hit_count = 0;
		GMixer.stats.cache_miss_count = 0;
	}

	for (size_t i = 0; i < GMixer.slots.size(); i++) {
		auto& slot = GMixer.slots[i];

		sound_source* source = Snd_FindSound(slot.sound_name);
		if (source != nullptr && Snd_SlotOcclusion(i + 1, *source, dt, nullptr) != ESlotOcclusionResult::False)
		{
			Snd_AcquireHRTFSlot(i + 1);
			Snd_UpdateCache(*source, slot.position);
		} else {
			Snd_ReleaseHRTFSlot(i + 1);
		}

		if (source != nullptr) {
			Snd_ReleaseSound(slot.sound_name);
		}

		if (slot.state == Mixer::State::Delay)
		{
			slot.delay -= dt;
			if (slot.delay <= 0.f)
			{
				MixerNewState(i + 1, Mixer::State::Playing);
			}
		}
	}

	GMixer.stats.cache_lines_free = GMixer.free_cachelines.size();
	GMixer.stats.precache_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
	counter++;
}

ICF void Snd_PhononSpatialProcess(float** data, u32 slot_idx)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	if ((slot.flags & (u32)Mixer::Flags::Spatial) == 0) {
		return;
	}

	Fvector& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
	Fvector& distances = slot.parameters[(u32)Mixer::ParameterId::DistanceRange];

	if (slot.hrtf_slot == 0) {
		dsp_stuff stuff = {
			.dt = GMixer.dt,
			.panning = slot.panning,
			.camera_position = &GMixer.P,
			.camera_direction = &GMixer.D,
			.camera_normal = &GMixer.N,
			.obj_position = &pos
		};

		DSP_SpatialProcess(data, slot.parameters[(u32)Mixer::ParameterId::DistanceRange], stuff, false /*slot.flags& (u32)Mixer::Flags::NoOCC */);
		return;
	}

	auto& hrtf_slot = GMixer.hrtf_slots[slot.hrtf_slot - 1];

#ifndef DISABLE_STEAM_AUDIO
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		memcpy(hrtf_slot.process_buffer[ch], data[ch], SND_BLOCKSIZE * sizeof(float));
	}
#endif

	Fvector relative_pos;
	float distance;
	dsp_stuff stuff = {
		.dt = GMixer.dt,
		.panning = slot.panning,
		.camera_position = &GMixer.P,
		.camera_direction = &GMixer.D,
		.camera_normal = &GMixer.N,
		.obj_position = &pos
	};

	DSP_CalculateRelativePosition(stuff, relative_pos, distance);
	distance = std::max(distance, 0.1f);

#ifndef DISABLE_STEAM_AUDIO
	relative_pos.normalize();

	// HRTF
	IPLBinauralEffectParams binaural_params = { 
		.direction = {relative_pos.x, relative_pos.y, relative_pos.z}, 
		.spatialBlend = 1.0f, .hrtf = GMixer.ipl_hrtf
	};

	IPLAudioBuffer out_buf = { .numChannels = SND_CHANNEL_COUNT, .numSamples = SND_BLOCKSIZE, .data = data };
	R_ASSERT(iplBinauralEffectApply(hrtf_slot.effect, &binaural_params, &hrtf_slot.buf_desc, &out_buf) == IPL_STATUS_SUCCESS);
#elif !defined(DISABLE_RESONANCE_AUDIO)

	GMixer.ra_api->SetHeadPosition(GMixer.P.x, GMixer.P.y, GMixer.P.z);
	if (GMixer.ra_api != nullptr)
	{
		GMixer.ra_api->SetSourcePosition(hrtf_slot.source_id, pos.x, pos.y, pos.z);
		GMixer.ra_api->SetPlanarBuffer(hrtf_slot.source_id, (const float* const*)data, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
		GMixer.ra_api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, data);
	}
#endif

	// Attenuation
	float min_distance = std::max(distances.x, EPS_S);
	float max_distance = std::max(distances.y, min_distance + EPS_S);

	distance = std::clamp(distance, min_distance, max_distance);
	float att = min_distance / (psSoundRolloff * distance);
	att *= att;
	att *= 1.0f - std::clamp(std::max(distance - min_distance, 0.0f) / (max_distance - min_distance), 0.0f, 1.0f);
	att = std::clamp(att, 0.f, 1.f);
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
			data[ch][i] *= att;
		}
	}
}

ICF void Snd_RenderSlot(u32 slot_idx, sound_source& Source, float** process_buffer, float dt)
{
	auto& Slot = GMixer.slots[slot_idx - 1];

	float occ_volume = 1.0f;
	ESlotOcclusionResult OCCResult = Snd_SlotOcclusion(slot_idx, Source, dt, &occ_volume);
	if (OCCResult == ESlotOcclusionResult::False)
	{
		// TODO: hack for simulated sounds
		Slot.position = std::min(Slot.position + SND_BLOCKSIZE, Source.pub.frames_total);
		if (Slot.position == Source.pub.frames_total && (Slot.flags & (u16)Mixer::Flags::Looped) == 0)
		{
			MixerNewState(slot_idx, Mixer::State::Stopped);
		}

		return;
	}

	// Clear process buffer and read data from source
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
	{
		memset(process_buffer[ch], 0, SND_BLOCKSIZE * sizeof(float));
	}

	Snd_ProcessSlot(slot_idx, Source, process_buffer);

	Fvector& pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
	Fvector& volumes = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel];
	float begin_factor = 1.0f, end_factor = 1.0f;

	// Deferred stopping
	bool IsMusic = (Slot.flags & (u16)Mixer::Flags::Intro);
	if ((Slot.flags & (u16)Mixer::Flags::NoOCC) == 0 || OCCResult != ESlotOcclusionResult::SOM)
	{
		occ_volume = 1.f;
	}

	// Update fade volume for non-intro sounds (fade in on startup)
	// if (!is_music) {
	//	slot.fade_volume += dt * 10.0f;
	//	clamp(slot.fade_volume, 0.0f, 1.0f);
	//}
	// else
	{
		Slot.fade_volume = 1.f;
	}

	if (Slot.stopping_position != (u32)-1)
	{
		u32 stopping_total = Source.pub.frames_total - Slot.stopping_position;
		if (stopping_total > 1 && Slot.position >= Slot.stopping_position)
		{
			u32 begin_offset = Slot.position - Slot.stopping_position;
			u32 write_count = IsMusic ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * GMixer.time_factor);
			u32 end_offset = std::min(begin_offset + write_count, Source.pub.frames_total - 1);
			begin_factor = 1.0f - ((float)begin_offset / (float)(stopping_total - 1));
			end_factor = 1.0f - ((float)end_offset / (float)(stopping_total - 1));
			begin_factor = std::clamp(begin_factor, 0.0f, 1.0f);
			end_factor = std::clamp(end_factor, 0.0f, 1.0f);
		}
	}

	// Apply final volumes
	float slot_volume = volumes.x * volumes.y * volumes.z;

	float vol_mixer = GMixer.effect_volume;
	if (Slot.flags & (u16)Mixer::Flags::Music)
	{
		vol_mixer = GMixer.music_volume;
	}
	else if (Slot.flags & (u16)Mixer::Flags::Shooting)
	{
		vol_mixer = GMixer.shooting_volume;
	}

	float volume_final = occ_volume * slot_volume * vol_mixer * Slot.fade_volume;
	begin_factor *= volume_final;
	end_factor *= volume_final;

	float left_panning = Slot.parameters[(u32)Mixer::ParameterId::Panning].x;
	float right_panning = Slot.parameters[(u32)Mixer::ParameterId::Panning].y;

	// Spatial processing
	if (!(Slot.flags & (u32)Mixer::Flags::Intro) && Source.pub.channels_count == 1)
	{
		PROF_EVENT("Slot Spatial");

		if (Slot.flags & (u32)Mixer::Flags::Spatial)
		{
#if !defined(DISABLE_STEAM_AUDIO) || 0 //! defined(DISABLE_RESONANCE_AUDIO)
			if (psSoundFlags.is(ss_HRTF) && GMixer.hrtf_enabled)
			{
				Snd_PhononSpatialProcess(process_buffer, SlotIdx);
			}
			else
#endif
			{
				dsp_stuff stuff = {
					.dt = GMixer.dt,
					.panning = Slot.panning,
					.camera_position = &GMixer.P,
					.camera_direction = &GMixer.D,
					.camera_normal = &GMixer.N,
					.obj_position = &pos
				};

				DSP_SpatialProcess(process_buffer, Slot.parameters[(u32)Mixer::ParameterId::DistanceRange], stuff, false /* slot.flags& (u32)Mixer::Flags::NoOCC */);
			}
		}

		if (GMixer.editor_zone)
		{
			Slot.zone_idx = 1;
		}

		u32 zone_idx = Slot.zone_idx;
		if (zone_idx && zone_idx <= GMixer.zones.size())
		{
			sound_zone_params& zone = GMixer.zones[zone_idx - 1];
			zone.use_count++;
			zone.last_use_ms = Snd_Milliseconds();

			float* reverb_buffer[SND_CHANNEL_COUNT] = {};
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
			{
				reverb_buffer[ch] = zone.data[ch];
			}

			// TODO(vertver): better volume attenutation for reverb stuff
			DSP_MixBufferPanning(reverb_buffer, process_buffer, begin_factor, end_factor, left_panning, right_panning, SND_BLOCKSIZE);
		}
	}

	// TODO(vertver): push data to buses instead of main
	int bus_idx = IsMusic ? SND_BUS_MUSIC : SND_BUS_EFFECTS;
	float* bus_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
	{
		bus_buffer[ch] = GMixer.buses[bus_idx].data[ch];
	}

	// Bus mixing
	DSP_MixBufferPanning(bus_buffer, process_buffer, begin_factor, end_factor, left_panning, right_panning, SND_BLOCKSIZE);
}

ICF void Snd_MixerRenderCallback(float* buffer)
{
	PROF_EVENT("Sound: Render Stage");

	xrSRWLockGuard guard(GMixer.render_lock, true);

	static u64 timestamp = Snd_GetTimestamp();
	float dt = (float)((double)(Snd_GetTimestamp() - timestamp) / 1000000000.0);
	timestamp = Snd_GetTimestamp();

	memset(buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
	static float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};

	float* process_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		process_buffer[i] = _process_buffer[i];
	}

	for (size_t i = 0; i < SND_BUS_COUNT; i++)
	{
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
		{
			memset(GMixer.buses[i].data[ch], 0, SND_BLOCKSIZE * sizeof(float));
		}
	}

	for (auto& zone : GMixer.zones)
	{
		zone.use_count = 0;
		memset(zone.data, 0, sizeof(zone.data));
	}

	for (size_t i = 0; i < GMixer.slots.size(); i++)
	{
		PROF_EVENT("Slot Render");
		if (GMixer.slots[i].state != Mixer::State::Playing)
		{
			continue;
		}

		sound_source* source = Snd_FindSound(GMixer.slots[i].sound_name);
		if (source == nullptr)
		{
			MixerNewState(i + 1, Mixer::State::Stopped);
			continue;
		}

		Snd_RenderSlot(i + 1, *source, process_buffer, dt);
		Snd_ReleaseSound(GMixer.slots[i].sound_name);
	}

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		process_buffer[i] = _process_buffer[i];
	}

	// Reverb mixing
	if (psSoundFlags.is(ss_EFX))
	{
		for (auto& zone : GMixer.zones)
		{
			if (zone.use_count == 0 && (zone.last_use_ms + 3000) < Snd_Milliseconds())
			{
				continue;
			}

			PROF_EVENT("Reverb rendering");
			float* reverb_buffer[SND_CHANNEL_COUNT] = {};
			float* bus_buffer[SND_CHANNEL_COUNT] = {};

			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
			{
				reverb_buffer[ch] = zone.data[ch];
				bus_buffer[ch] = GMixer.buses[SND_BUS_REVERB].data[ch];
			}

#ifndef DISABLE_RESONANCE_AUDIO
			DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, reverb_buffer, 1.0f, SND_BLOCKSIZE, zone.compressor_envelope[0]);
			zone.state.ra_context->SetPlanarBuffer(zone.state.buffer, reverb_buffer, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
			zone.state.ra_context->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, process_buffer);
			DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, bus_buffer, 1.0f, SND_BLOCKSIZE, zone.compressor_envelope[1]);
#endif

#ifndef DISABLE_STEAM_AUDIO
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
			{
				IPLAudioBuffer reverb_buf = {.numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &reverb_buffer[ch]};
				IPLAudioBuffer out_buf = {.numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &process_buffer[ch]};
				IPLReflectionEffectParams params = {};
				ConvertReverbSettings(&zone.settings, &params, SND_SAMPLERATE);
				iplReflectionEffectApply(zone.state.effect[ch], &params, &reverb_buf, &out_buf, nullptr);
			}
#endif
			float reverb_gain = std::clamp(zone.settings.reverb, 0.0f, 1.0f) * 0.010f;
			DSP_MixBuffer(bus_buffer, process_buffer, reverb_gain, reverb_gain, SND_BLOCKSIZE);
		}
	}

	{
		PROF_EVENT("Sound Mixing");
		float* master_buffer[SND_CHANNEL_COUNT] = {};
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
		{
			master_buffer[ch] = GMixer.buses[SND_BUS_MASTER].data[ch];
		}

		// Master mixing
		for (size_t i = 0; i < SND_BUS_COUNT; i++)
		{
			float* bus_buffer[SND_CHANNEL_COUNT] = {};
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
			{
				bus_buffer[ch] = GMixer.buses[i].data[ch];
			}

			DSP_MixBuffer(master_buffer, bus_buffer, 1.0f, 1.0f, SND_BLOCKSIZE);
		}

		DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, master_buffer, GMixer.compression, SND_BLOCKSIZE, GMixer.compressor_envelope);

		// Clipping and master volume adjust
		for (size_t i = 0; i < SND_BLOCKSIZE; i++)
		{
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
			{
				float sample = master_buffer[ch][i];
				sample = std::clamp(sample, -1.0f, 1.0f) * GMixer.master_volume;
				buffer[i * SND_CHANNEL_COUNT + ch] = sample;
			}
		}
	}

#ifdef DEBUG_DRAW
	for (size_t i = 0; i < SND_BLOCKSIZE; i++)
	{
		float sample = 0.0f;
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++)
		{
			sample += buffer[i * SND_CHANNEL_COUNT + ch];
		}

		sample /= float(SND_CHANNEL_COUNT);
		GMixer.aligned_input_fft[i] = GMixer.fft_window[i] * sample;
	}

	pffft_transform_ordered(GMixer.fft_setup, GMixer.aligned_input_fft, GMixer.aligned_output_fft, nullptr, PFFFT_FORWARD);
	GMixer.stats.spectral_data[0] = lin2dB(fabs(GMixer.aligned_output_fft[0]) / float(SND_BLOCKSIZE));

	for (size_t k = 1; k < SND_BLOCKSIZE / 2; k++)
	{
		float re = GMixer.aligned_output_fft[k];
		float im = GMixer.aligned_output_fft[SND_BLOCKSIZE + k - 1];
		float mag = sqrtf(re * re + im * im) / float(SND_BLOCKSIZE);
		GMixer.stats.spectral_data[k] = lin2dB(mag);
	}

	GMixer.stats.spectral_data[SND_BLOCKSIZE / 2] = lin2dB(fabs(GMixer.aligned_output_fft[SND_BLOCKSIZE / 2]) / float(SND_BLOCKSIZE));

	float volumes[SND_CHANNEL_COUNT] = {};
	for (size_t j = 0; j < SND_BLOCKSIZE; j++)
	{
		for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
		{
			volumes[i] = (volumes[i] + fabs(buffer[j * SND_CHANNEL_COUNT + i])) * 0.5f;
		}
	}

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		volumes[i] = lin2dB(volumes[i]);
	}

	memcpy(GMixer.stats.channel_volumes, volumes, sizeof(volumes));
#endif

	GMixer.stats.render_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
}

void 
Mixer::Initialize()
{
	GMixer.slots.clear();
	GMixer.free_slots.clear();
	GMixer.free_cachelines.clear();
	GMixer.cache_lines.clear();
	GMixer.sounds.clear();
	GMixer.cmd.clear();
	GMixer.cmd.reserve(256);
	Snd_GrowSlots(true);
	Snd_GrowCacheLines(true);
	
#ifndef DISABLE_STEAM_AUDIO
	GMixer.hrtf_enabled = true;
	IPLContextSettings ipl_settings = { .version = STEAMAUDIO_VERSION, .simdLevel = IPL_SIMDLEVEL_SSE2 };
	IPLAudioSettings settings = { .samplingRate = SND_SAMPLERATE, .frameSize = SND_BLOCKSIZE };
	IPLerror err = iplContextCreate(&ipl_settings, &GMixer.ipl_context);
	R_ASSERT(err == IPL_STATUS_SUCCESS);

	GMixer.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		GMixer.free_hrtf_slots[i] = i + 1;
	}

	IPLHRTFSettings hrtf_settings = { .type = IPL_HRTFTYPE_DEFAULT, .volume = 1.0f };
	R_ASSERT(iplHRTFCreate(GMixer.ipl_context, &settings, &hrtf_settings, &GMixer.ipl_hrtf) == IPL_STATUS_SUCCESS);

	GMixer.hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		IPLBinauralEffectSettings binaural = { .hrtf = GMixer.ipl_hrtf };
		R_ASSERT(iplBinauralEffectCreate(GMixer.ipl_context, &settings, &binaural, &GMixer.hrtf_slots[i].effect) == IPL_STATUS_SUCCESS);
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			GMixer.hrtf_slots[i].process_buffer[ch] = GMixer.hrtf_slots[i]._process_buffer[ch];
		}

		GMixer.hrtf_slots[i].buf_desc = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = GMixer.hrtf_slots[i].process_buffer };
	}
#elif !defined(DISABLE_RESONANCE_AUDIO)
	// Initialize Resonance Audio API and HRTF slots
	GMixer.hrtf_enabled = true;
	GMixer.ra_api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	
	R_ASSERT(GMixer.ra_api != nullptr);

	GMixer.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		GMixer.free_hrtf_slots[i] = i + 1;
	}

	GMixer.hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		// Create a Resonance sound object source for this slot
		GMixer.hrtf_slots[i].source_id = GMixer.ra_api->CreateSoundObjectSource(vraudio::RenderingMode::kBinauralHighQuality);
	}
#endif

#ifdef DEBUG_DRAW
#pragma todo(replace with aligned allocators)
	// Blackman-Harris window
	for (int i = 0; i < SND_BLOCKSIZE; ++i) {
		GMixer.fft_window[i] = .5 * (1. - cosf(2. * 3.1415926535897932384 * (f64)i / (f64)(SND_BLOCKSIZE-1)));
	}

	GMixer.aligned_input_fft = (float*)aligned_alloc(16, SND_BLOCKSIZE * sizeof(float));
	GMixer.aligned_output_fft = (float*)aligned_alloc(16, SND_BLOCKSIZE * 2 * sizeof(float));
	GMixer.fft_setup = pffft_new_setup(SND_BLOCKSIZE, PFFFT_REAL);
#endif

	Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void 
Mixer::Shutdown()
{
	Backend::Shutdown();

#ifdef DEBUG_DRAW
	if (GMixer.fft_setup) {
		pffft_destroy_setup(GMixer.fft_setup);
		GMixer.fft_setup = nullptr;
	}
#endif

#ifndef DISABLE_STEAM_AUDIO
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		if (GMixer.hrtf_slots[i].effect != nullptr) {
			iplBinauralEffectRelease(&GMixer.hrtf_slots[i].effect);
		}
	}

	if (GMixer.ipl_hrtf) iplHRTFRelease(&GMixer.ipl_hrtf);
	if (GMixer.ipl_context) iplContextRelease(&GMixer.ipl_context);
#endif

#ifndef DISABLE_RESONANCE_AUDIO
	if (GMixer.ra_api) {
		for (size_t i = 0; i < GMixer.hrtf_slots.size(); ++i) {
			auto id = GMixer.hrtf_slots[i].source_id;
			if (id != vraudio::ResonanceAudioApi::kInvalidSourceId) {
				GMixer.ra_api->DestroySource(id);
			}
		}
		delete GMixer.ra_api;
		GMixer.ra_api = nullptr;
	}
#endif

	GMixer.hrtf_slots.clear();
	GMixer.free_hrtf_slots.clear();

	GMixer.slots.clear();
	GMixer.free_slots.clear();
	GMixer.free_cachelines.clear();
	GMixer.cache_lines.clear();
	GMixer.sounds.clear();
	GMixer.cmd.clear();
}

ICF void DestroyInternal(int slot)
{
	if (slot == 0) {
		return;
	}

	if (!GMixer.slots[slot - 1].sound_name.empty()) {
		Snd_ReleaseSound(GMixer.slots[slot - 1].sound_name);
		GMixer.slots[slot - 1].sound_name.clear();
	}

	memset(GMixer.slots[slot - 1].parameters, 0, sizeof(GMixer.slots[slot - 1].parameters));
	memset(GMixer.slots[slot - 1].history, 0, sizeof(GMixer.slots[slot - 1].history));
	GMixer.slots[slot - 1].position = 0;
	GMixer.slots[slot - 1].stopping_position = (u32)-1;
	GMixer.slots[slot - 1].flags = 0;
	GMixer.slots[slot - 1].state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].prev_state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].fake_state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].fade_volume = 1.0f;
	GMixer.free_slots.push_back(slot);
}
void 
Mixer::Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float shooting_volume, float compression, const Fmatrix& mtx, Fvector P, Fvector D, Fvector N)
{
	PROF_EVENT("Sound: Update Stage");
	sound_event* handler = (sound_event*)event_handler;

	static u64 timestamp = Snd_GetTimestamp();
	GMixer.dt = (float)((Snd_GetTimestamp() - timestamp) / 1000000) * 0.001f;
	timestamp = Snd_GetTimestamp();

	GMixer.time_factor = std::clamp(time_factor, 0.1f, 10.0f);
	GMixer.compression = compression;
	GMixer.master_volume = volume;
	GMixer.effect_volume = eff_volume;
	GMixer.music_volume = mus_volume;
	GMixer.shooting_volume = shooting_volume;
	GMixer.m_V = mtx;
	GMixer.P = P;
	GMixer.D = D;
	GMixer.N = N;

	GMixer.render_lock.AcquireExclusive();
	GMixer.manage_lock.AcquireExclusive();
	GMixer.update_lock.AcquireExclusive();

	for (auto& RefSound : GMixer.sounds)
	{
		if (RefSound == nullptr || !RefSound->slot() || RefSound->_g_object() == nullptr || !RefSound->unique_id())
		{
			continue;
		}

		auto& Slot = GMixer.slots[RefSound->slot() - 1];

		if (Slot.fake_state != State::Playing || Slot.state != State::Playing)
		{
			continue;
		}

		CObject* Object = RefSound->_g_object();
		if (Slot.flags & (u16)Flags::Spatial && (Slot.flags & (u16)Flags::NoPosUpdate) == 0)
		{
			if (Object != nullptr)
			{
				auto& Pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
				Pos = ((IRenderable*)Object)->renderable.xform.c;
			}
		}

		// Periodic AI sound-event propagation:
		// re-emit the sound event every s_f_def_event_pulse seconds while the sound
		// is playing so NPCs keep tracking ongoing / moving / looped fire.
		if (RefSound->_p != nullptr && RefSound->_p->g_type != 0)
		{
			RefSound->TimeToPropagade -= GMixer.dt;
			if (RefSound->TimeToPropagade <= 0.0f)
			{
				RefSound->TimeToPropagade = s_f_def_event_pulse;
				if (handler != nullptr)
				{
					const Fvector& Dist = Slot.parameters[(u32)Mixer::ParameterId::DistanceRange];
					const float SndVolume = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].y;
					float Clip = Dist.z * SndVolume;
					float Range = std::min(Dist.z, Clip);
					if (Range >= 0.1f)
					{
						handler(RefSound->_p, Range);
					}
				}
			}
		}
	}

	for (size_t i = 0; i < GMixer.slots.size(); i++)
	{
		auto& slot = GMixer.slots[i];
		if (slot.flags & (u16)Flags::NoFeedback && slot.state == State::Stopped)
		{
			Destroy(i + 1);
		}
		else
		{
			bool occ_enable = ((slot.flags & ((u16)Flags::Intro | (u16)Flags::NoOCC)) == 0) && slot.state == State::Playing;
			if (occ_enable)
			{
				Fvector pos = (slot.flags & (u16)Flags::Spatial) ? slot.parameters[(u32)Mixer::ParameterId::Position] : GMixer.P;
				float dist = GMixer.P.distance_to(pos);
				if (dist <= slot.parameters[(u32)Mixer::ParameterId::DistanceRange].y)
				{
					float out_occ = ::Sound->get_occlusion(pos, 0.2f, &GMixer.occ);
					float& old_occ = slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
					volume_lerp(old_occ, out_occ, 1.0f, GMixer.dt);

					CDB::MODEL* env_model = ::Sound->get_geometry_env();
					CDB::COLLIDER* collider = ::Sound->get_geometry_db();
					if (env_model != nullptr)
					{
						Fvector dir = {0, -1, 0};
						collider->ray_options(CDB::OPT_ONLYNEAREST);
						collider->ray_query(env_model, pos, dir, 1000.f);
						xr_vector<Fvector>& Verts = env_model->get_verts();
						xr_vector<CDB::TRI>& Tris = env_model->get_tris();
						if (collider->r_count())
						{
							CDB::RESULT* r = collider->r_begin();
							CDB::TRI& T = Tris[r->id];
							auto& verts = T.verts;

							Fvector tri_norm;
							tri_norm.mknormal(Verts[verts[0]], Verts[verts[1]], Verts[verts[2]]);

							R_ASSERT(T.dummy < GMixer.zones.size());
							slot.zone_idx = T.dummy + 1;
						}
						else
						{
							slot.zone_idx = 0;
						}
					}
					else
					{
						slot.zone_idx = 0;
					}
				}
			}
		}
	}

	xrCriticalSectionGuard guard(GMixer.play_lock);
	for (const auto& cmd : GMixer.cmd)
	{
		switch (cmd.id)
		{
			case sound_cmd_id::play:
			{
				bool IsSoundExists = (cmd.param1 && GMixer.sounds.contains((ref_sound*)cmd.param1));
				ref_sound* sound = IsSoundExists ? (ref_sound*)cmd.param1 : nullptr;
				u16 flags = cmd.param0;

				auto& ActualSlot = GMixer.slots[cmd.slot - 1];
				bool IsSameFile = ActualSlot.sound_name == cmd.string_storage.c_str();
				if (!IsSameFile && !ActualSlot.sound_name.empty())
				{
					Snd_ReleaseSound(ActualSlot.sound_name);
					ActualSlot.sound_name.clear();
				}

				sound_source* SourcePtr = IsSameFile ? Snd_FindSound(ActualSlot.sound_name) : Snd_AcquireSound(cmd.string_storage.c_str(), false);

				if (SourcePtr == nullptr)
				{
					MixerNewState(cmd.slot, State::Stopped);
					break;
				}

				if (IsSameFile)
				{
					// The slot already holds a reference on it
					Snd_ReleaseSound(ActualSlot.sound_name);
				}

				auto& Source = *SourcePtr;
				memset(ActualSlot.parameters, 0, sizeof(ActualSlot.parameters));
				memset(ActualSlot.history, 0, sizeof(ActualSlot.history));
				ActualSlot.parameters[(u32)Mixer::ParameterId::VolumePerChannel] = Fvector(Source.pub.volume, 1.0f, 1.0f);
				ActualSlot.parameters[(u32)Mixer::ParameterId::DistanceRange] = Fvector(Source.pub.min_distance, Source.pub.max_distance, Source.pub.max_ai_distance);
				ActualSlot.parameters[(u32)Mixer::ParameterId::Pitch] = Fvector{1.0f, 1.0f, 1.0f};
				ActualSlot.parameters[(u32)Mixer::ParameterId::Panning] = Fvector{1.0f, 1.0f, 1.0f};
				ActualSlot.position = 0;
				ActualSlot.stopping_position = (u32)-1;
				ActualSlot.sound_name = cmd.string_storage.c_str();
				ActualSlot.flags = flags;
				ActualSlot.fade_volume = 0.0f;

				if (ActualSlot.flags & (u16)Flags::Spatial && sound != nullptr && sound->_g_object() != nullptr)
				{
					ActualSlot.parameters[(u32)Mixer::ParameterId::Position] = ((IRenderable*)sound->_g_object())->renderable.xform.c;
				}

				if (handler != nullptr)
				{
					float Clip = Source.pub.max_ai_distance * Source.pub.volume;
					float Range = std::min(Source.pub.max_ai_distance, Clip);

					if (Range >= 0.1f && sound != nullptr && sound->_p != nullptr)
					{
						if (CObject* Object = sound->_g_object())
						{
							if (flags & (u16)Flags::NoFeedback)
							{
								ref_sound_data_ptr DataPtr = new ref_sound_data();
								DataPtr->slot = cmd.slot;
								DataPtr->g_type = 0;
								DataPtr->g_object = Object;
								DataPtr->dont_destroy_slot = true;
								DataPtr->fn_attached[0] = Source.pub.path;
								handler(DataPtr, Range);
							}
							else
							{
								handler(sound->_p, Range);
							}
						}
					}
				}

				if (!fis_zero(cmd.param2))
				{
					ActualSlot.delay = (float)*(double*)&cmd.param2;
					MixerNewState(cmd.slot, State::Delay);
				}
				else
				{
					MixerNewState(cmd.slot, State::Playing);
				}
			}
			break;
			case sound_cmd_id::pause:
			{
				MixerNewState(cmd.slot, State::Paused);
			}
			break;
			case sound_cmd_id::stop:
			{
				if (cmd.param0)
				{
					GMixer.slots[cmd.slot - 1].flags &= ~((u8)Flags::Looped);
					GMixer.slots[cmd.slot - 1].stopping_position = GMixer.slots[cmd.slot - 1].position;
				}
				else
				{
					MixerNewState(cmd.slot, State::Stopped);
					GMixer.slots[cmd.slot - 1].position = 0;
					GMixer.slots[cmd.slot - 1].stopping_position = (u32)-1;
				}
			}
			break;
			case sound_cmd_id::destroy:
			{
				if (GMixer.slots[cmd.slot - 1].state != State::Delay)
				{
					DestroyInternal(cmd.slot);
				}
			}
			break;
			case sound_cmd_id::stop_all:
			{
				for (size_t i = 0; i < GMixer.slots.size(); i++)
				{
					if (GMixer.slots[i].sound_name.size() && GMixer.slots[i].state != State::Stopped)
					{
						GMixer.slots[i].position = 0;
						GMixer.slots[i].stopping_position = (u32)-1;
						GMixer.slots[i].prev_state = GMixer.slots[i].state;
						GMixer.slots[i].state = State::Stopped;
						GMixer.slots[i].fake_state = State::Stopped;
					}
				}
			}
			break;
			case sound_cmd_id::pause_all:
			{
				for (size_t i = 0; i < GMixer.slots.size(); i++)
				{
					if (GMixer.slots[i].sound_name.size() && GMixer.slots[i].state != State::Stopped && GMixer.slots[i].state != State::Paused)
					{
						GMixer.slots[i].prev_state = GMixer.slots[i].state;
						GMixer.slots[i].state = State::Paused;
					}
				}
			}
			break;
			case sound_cmd_id::resume_all:
			{
				for (size_t i = 0; i < GMixer.slots.size(); i++)
				{
					if (GMixer.slots[i].state == State::Paused && GMixer.slots[i].prev_state != State::Paused)
					{
						GMixer.slots[i].state = GMixer.slots[i].prev_state;
						GMixer.slots[i].prev_state = State::Paused;
					}
				}
			}
			break;
			case sound_cmd_id::update_parameter:
			{
				GMixer.slots[cmd.slot - 1].parameters[(u32)cmd.param0] = Fvector{(float)*(double*)&cmd.param1, (float)*(double*)&cmd.param2, (float)*(double*)&cmd.param3};

				auto& slot = GMixer.slots[cmd.slot - 1];
				if (slot.flags & (u16)Flags::Spatial && cmd.param0 == (u16)ParameterId::Position)
				{
					auto& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
					float out_occ = ::Sound->get_occlusion(pos, 0.2f, &GMixer.occ);
					float& old_occ = slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
					volume_lerp(old_occ, out_occ, 1.0f, GMixer.dt);
				}
			}
			break;
			case sound_cmd_id::set_volume:
			{
				GMixer.slots[cmd.slot - 1].parameters[(u32)ParameterId::VolumePerChannel].y = *(double*)&cmd.param1;
			}
			break;
			case sound_cmd_id::set_panning:
			{
				GMixer.slots[cmd.slot - 1].parameters[(u32)ParameterId::Panning].x = *(double*)&cmd.param1;
				GMixer.slots[cmd.slot - 1].parameters[(u32)ParameterId::Panning].y = *(double*)&cmd.param2;
			}
			break;
		}
	}

	GMixer.cmd.resize(0);
	GMixer.stats.update_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;

	GMixer.update_lock.ReleaseExclusive();
	GMixer.manage_lock.ReleaseExclusive();
	GMixer.render_lock.ReleaseExclusive();
}

void Mixer::StopAll()
{
	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{.id = sound_cmd_id::stop_all});
}

void Mixer::PauseAll()
{
	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{ .id = sound_cmd_id::pause_all });
}

void Mixer::ResumeAll()
{
	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{ .id = sound_cmd_id::resume_all });
}

void Mixer::DereferenceObjects(CObject** object, int count)
{
	xrSRWLockGuard g0(GMixer.update_lock);
	xrSRWLockGuard g1(GMixer.manage_lock);

	for (auto& SoundRef : GMixer.sounds)
	{
		if (SoundRef == nullptr || SoundRef->_p == nullptr)
		{
			continue;
		}

		for (size_t i = 0; i < count; i++)
		{
			if (object[i] == SoundRef->_g_object())
			{
				SoundRef->_p->g_object = nullptr;
			}
		}
	}
}

u32 Mixer::Create()
{
	if (GMixer.free_slots.empty())
	{
		Snd_GrowSlots(true);
	}

	xrSRWLockGuard g0(GMixer.update_lock);

	u32 SlotIdx = GMixer.free_slots[GMixer.free_slots.size() - 1];
	GMixer.free_slots.pop_back();
	GMixer.stats.possible_free_count--;

	return SlotIdx;
}

void Mixer::Destroy(u32 slot)
{
	if (slot == 0 || GMixer.slots[slot - 1].state == State::Delay)
	{
		return;
	}

	GMixer.slots[slot - 1].fake_state = State::Stopped;

	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{ .slot = slot, .id = sound_cmd_id::destroy });
	GMixer.stats.possible_free_count++;
}

void Mixer::Play(u32 slot, u16 flags, ref_sound* sound, double delay)
{
	xrCriticalSectionGuard guard(GMixer.play_lock);

	if (slot == 0 || sound == nullptr || sound->_p == nullptr || sound->_p->fn_attached[0] == nullptr) {
		return;
	}

	GMixer.slots[slot - 1].fake_state = State::Playing;
	GMixer.cmd.emplace_back(sound_command{ 
		.slot = slot, .id = sound_cmd_id::play, .param0 = flags, .param1 = (u64)sound, 
		.param2 = *(u64*)&delay, .param3 = (u64)sound->_g_object(), 
		.string_storage = sound->_p->fn_attached[0]
	});
}

void Mixer::PlayNoFeedback(u16 flags, ref_sound* sound, CObject* obj, double delay, float* pitch, float* volume, Fvector* distance, Fvector* pos)
{
	xrCriticalSectionGuard guard(GMixer.play_lock);

	u32 slot_idx = Create();
	if (slot_idx == 0)
	{
		return;
	}

	auto& slot = GMixer.slots[slot_idx - 1];
	slot.state = State::Paused;

	slot.fake_state = State::Playing;
	GMixer.cmd.emplace_back(sound_command{
		.slot = slot_idx, .id = sound_cmd_id::play, .param0 = flags, .param1 = (u64)sound, .param2 = *(u64*)&delay, .param3 = (u64)obj, .string_storage = sound->_p->fn_attached[0]
	});

	auto params = sound->_p->get_params();
	Fvector distances = {params.min_distance, params.max_distance, params.max_ai_distance};

	if (sound->slot())
	{
		pitch = (pitch ? pitch : &params.freq);
		distance = (distance ? distance : &distances);
		pos = (pos ? pos : &params.position);
		volume = (volume ? volume : &params.volume);
	}

	if (pitch)
	{
		Mixer::UpdateParameter(slot_idx, ParameterId::Pitch, Fvector{*pitch, 1.f, 1.f});
	}
	if (distance)
	{
		Mixer::UpdateParameter(slot_idx, ParameterId::DistanceRange, *distance);
	}
	if (pos)
	{
		Mixer::UpdateParameter(slot_idx, ParameterId::Position, *pos);
	}
	if (volume)
	{
		Mixer::SetVolume(slot_idx, *volume);
	}
}

void Mixer::Pause(u32 slot)
{
	if (slot == 0) 
	{
		return;
	}

	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.slots[slot - 1].fake_state = State::Paused;
	GMixer.cmd.emplace_back(sound_command{ .slot = slot, .id = sound_cmd_id::pause });
}

void Mixer::Stop(u32 slot, bool deferred)
{
	if (slot == 0)
	{
		return;
	}

	auto& Slot = GMixer.slots[slot - 1];
	if (Slot.state == State::Delay)
	{
		return;
	}

	if (!deferred)
	{
		Slot.fake_state = State::Stopped;
	}

	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{.slot = slot, .id = sound_cmd_id::stop, .param0 = deferred});
}

void Mixer::UpdateParameter(u32 slot, ParameterId parameter, Fvector value)
{
	if (slot == 0)
	{
		return;
	}

	double p0 = value.x, p1 = value.y, p2 = value.z;
	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{.slot = slot, .id = sound_cmd_id::update_parameter, .param0 = (u16)parameter, .param1 = *(u64*)&p0, .param2 = *(u64*)&p1, .param3 = *(u64*)&p2});
}

void Mixer::SetVolume(u32 slot, double volume)
{
	if (slot == 0)
	{
		return;
	}

	volume = std::clamp(volume, 0.0, 1.0);
	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{.slot = slot, .id = sound_cmd_id::set_volume, .param1 = *(u64*)&volume});
}

void Mixer::SetPanning(u32 slot, double left, double right)
{
	if (slot == 0) {
		return;
	}

	left = std::clamp(left, 0.0, 1.0);
	right = std::clamp(right, 0.0, 1.0);

	xrCriticalSectionGuard guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(sound_command{ .slot = slot,.id = sound_cmd_id::set_panning, .param1 = *(u64*)&left, .param2 = *(u64*)&right });
}

xr_vector<sound_slot_state>& Mixer::GetSlots()
{
	return GMixer.slots;
}

xrSRWLock& Mixer::GetUpdateMutex()
{
	return GMixer.update_lock;
}

xrSRWLock& Mixer::GetManageMutex()
{
	return GMixer.manage_lock;
}

sound_stats* Mixer::GetStats()
{
	return &GMixer.stats;
}

float Mixer::GetPlaytime(u32 slot)
{
	if (slot == 0)
	{
		return 0.0f;
	}

	return (((float)GMixer.slots[slot - 1].position) / (float)SND_SAMPLERATE);
}

float Mixer::GetDuration(u32 slot)
{
	xrSRWLockGuard guard(GMixer.source_lock, true);

	if (slot == 0)
	{
		return 0.0f;
	}

	if (!GMixer.slots[slot - 1].sound_name.size() || !GMixer.snd_sources.contains(GMixer.slots[slot - 1].sound_name))
	{
		return 0.0f;
	}

	auto& source = GMixer.snd_sources.at(GMixer.slots[slot - 1].sound_name);
	return (float)source.pub.frames_total / (float)SND_SAMPLERATE;
}

bool Mixer::SlotIsRelated(u32 slot)
{
	if (slot == 0)
	{
		return false;
	}

	const xr_string& name = GMixer.slots[slot - 1].sound_name;
	sound_source* source = Snd_FindSound(name);
	if (source == nullptr)
	{
		return false;
	}

	bool result = Snd_SlotOcclusion(slot, *source, 0.0f, nullptr) != ESlotOcclusionResult::False;
	Snd_ReleaseSound(name);
	return result;
}

u32 Mixer::GetGameType(u32 slot)
{
	xrSRWLockGuard guard(GMixer.source_lock, true);

	if (slot == 0) {
		return 0.0f;
	}

	if (!GMixer.slots[slot - 1].sound_name.size() || !GMixer.snd_sources.contains(GMixer.slots[slot - 1].sound_name)) {
		return 0.0f;
	}

	auto& source = GMixer.snd_sources.at(GMixer.slots[slot - 1].sound_name);
	return source.pub.game_type;
}

u32 
Mixer::GetFlags(u32 slot)
{
	if (slot == 0) {
		return 0;
	}

	return GMixer.slots[slot - 1].flags;
}

Mixer::State
Mixer::GetState(u32 slot)
{
	if (slot == 0) {
		return State::Stopped;
	}

	return GMixer.slots[slot - 1].fake_state;
}

u32
Mixer::GetSourceCount()
{
	xrSRWLockGuard guard(GMixer.source_lock, true);

	return GMixer.snd_sources.size();
}

const sound_source_public*
Mixer::GetSource(u32 index)
{
	xrSRWLockGuard guard(GMixer.source_lock, true);

	u32 counter = 0;
	for (const auto& [key, source] : GMixer.snd_sources) {
		if (counter == index) {
			return &source.pub;
		}

		counter++;
	}

	return nullptr;
}

Fvector*
Mixer::GetParameters(u32 slot)
{
	if (slot == 0) {
		return NULL;
	}

	return GMixer.slots[slot - 1].parameters;
}

void 
Mixer::AddEditorZone(sound_zone_params& params)
{
	GMixer.editor_zone = true;
	ResetZones();
	AddZone(params);
}

#ifndef DISABLE_RESONANCE_AUDIO
ICF float DbToLinear(float db)
{
	return powf(10.0f, db / 20.0f);
}

ICF void Snd_EngineToResonanceParams(const sound_zone_params& s,
	vraudio::ReflectionProperties& out_ref,
	vraudio::ReverbProperties& out_rev)
{
	out_ref = vraudio::ReflectionProperties();
	out_rev = vraudio::ReverbProperties();

	constexpr float HF_REF = 5000.0f;

	// Room dimensions
	out_ref.room_dimensions[0] = s.size.x;
	out_ref.room_dimensions[1] = s.size.y;
	out_ref.room_dimensions[2] = s.size.z;

	out_ref.room_position[0] = s.center.x;
	out_ref.room_position[1] = s.center.y;
	out_ref.room_position[2] = s.center.z;

	out_ref.room_rotation[0] = out_ref.room_rotation[1] = out_ref.room_rotation[2] = 0.0f;
	out_ref.room_rotation[3] = 1.0f;

	// High-frequency cutoff from EAX RoomHF (dB)
	// RoomHF = -10000..0 dB  (attenuation)
	// cutoff = HF_REF * 10^(RoomHF/20)
	float hf_db = std::clamp(s.settings.room_hf, -10000.0f, 0.0f);
	float cutoff = HF_REF * DbToLinear(hf_db);
	out_ref.cutoff_frequency = std::clamp(cutoff, 200.0f, 20000.0f);

	// Reflection coefficients = diffusion
	float diffusion = std::clamp(s.settings.environment_diffusion, 0.0f, 1.0f);
	for (int i = 0; i < 6; i++)
		out_ref.coefficients[i] = diffusion;

	// Early reflection gain (EAX Reflections)
	//	EAX uses dB -> RA uses linear gain
	out_ref.gain = DbToLinear(std::clamp(s.settings.reflections, -10000.0f, 0.0f));

	// Late reverb gain (EAX Reverb)
	out_rev.gain = DbToLinear(std::clamp(s.settings.reverb, -10000.0f, 0.0f));

	// RT60 mapping for 9 bands
	const float base_rt60 = std::clamp(s.settings.decay_time, 0.1f, 20.0f);
	const float hf_ratio = std::clamp(s.settings.decay_hf_ratio, 0.1f, 4.0f);

	// Air absorption HF is in dB/metre for HF_REF
	float air_hf = std::max(0.0f, s.settings.air_absorption_hf);
	float hf_abs_scale = powf(10.0f, -(air_hf) / 20.0f);

	for (int i = 0; i < 9; i++)
	{
		bool isHF = (i >= 6);

		float rt = base_rt60;
		if (isHF)
			rt *= hf_ratio * hf_abs_scale;

		out_rev.rt60_values[i] = std::clamp(rt, 0.05f, 120.0f);
	}
}
#endif

void
Mixer::AddZone(sound_zone_params& params)
{
#ifndef DISABLE_STEAM_AUDIO
	// TODO: convolution and baking reverb
	IPLAudioSettings settings = { .samplingRate = SND_SAMPLERATE, .frameSize = SND_BLOCKSIZE };
	IPLReflectionEffectSettings reflect_settings = { 
		.type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC, .irSize = SND_BLOCKSIZE*4, .numChannels = 1 
	};

	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		R_ASSERT(iplReflectionEffectCreate(GMixer.ipl_context, &settings, &reflect_settings, &params.state.effect[ch]) == IPL_STATUS_SUCCESS);
	}
#endif

#ifndef DISABLE_RESONANCE_AUDIO
	vraudio::ReflectionProperties reflection_properties = { };
	vraudio::ReverbProperties reverb_properties = {};

	Snd_EngineToResonanceParams(params, reflection_properties, reverb_properties);
	params.state.ra_context = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	params.state.ra_context->EnableRoomEffects(true);
	params.state.ra_context->SetReverbProperties(reverb_properties);
	params.state.ra_context->SetReflectionProperties(reflection_properties);
	params.state.buffer = params.state.ra_context->CreateSoundObjectSource(vraudio::RenderingMode::kStereoPanning);
#endif

	GMixer.zones.emplace_back(std::move(params));
}

void 
Mixer::ResetZones()
{
	xrSRWLockGuard guard(GMixer.render_lock);

	for (auto& zone : GMixer.zones) {
#ifndef DISABLE_STEAM_AUDIO
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			if (zone.state.effect[ch] != nullptr) {
				iplReflectionEffectRelease(&zone.state.effect[ch]);
			}
		}
#endif

#ifndef DISABLE_RESONANCE_AUDIO
		delete zone.state.ra_context;
		zone.state.ra_context = nullptr;
#endif
	}

	GMixer.zones.clear();
}

const xr_vector<sound_zone_params>& Mixer::GetZones()
{
	return GMixer.zones;
}

ref_sound::ref_sound()
{
	xrSRWLockGuard g1(GMixer.manage_lock);

	if (!GMixer.sounds.contains(this)) {
		GMixer.sounds.emplace(this);
	}
}

ref_sound::~ref_sound()
{
	xrSRWLockGuard g1(GMixer.manage_lock);

	if (GMixer.sounds.contains(this)) {
		GMixer.sounds.erase(this);
	}
}