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
#include "pffft.h"

#define DISABLE_STEAM_AUDIO
//#define DISABLE_RESONANCE_AUDIO

#ifndef DISABLE_STEAM_AUDIO
#   include <SteamAudio/phonon.h>
#endif

#ifndef DISABLE_RESONANCE_AUDIO
#include "../../3rd-party/resonance-audio/resonance_audio/api/binaural_surround_renderer.h"
#include "../../3rd-party/resonance-audio/resonance_audio/api/resonance_audio_api.h"
#endif

#include "ogg_utils.h"

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#define SND_HRTF_SLOT_COUNT (512)
#define DEFAULT_SLOT_COUNT (4096)
#define CACHE_LINES_COUNT (2048 * 4)
#define CACHE_LINE_WIDTH (12)
#define CACHE_LINE_ENTRY_COUNT (32)

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
	set_volume
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
	float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
	float* process_buffer[SND_CHANNEL_COUNT];
#ifndef DISABLE_STEAM_AUDIO
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

	sound_stats stats = { 0 };
	float time_factor = 1.0f;
	float master_volume = 0.0f;
	float effect_volume = 0.0f;
	float music_volume = 0.0f;
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
	xr_hash_map<xr_string, sound_source> sources;
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

static sound_mixer_state mixer = {};

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
Snd_AcquireHRTFSlot(u32 slot_idx)
{
	if (!mixer.hrtf_enabled) return;

	auto& slot = mixer.slots[slot_idx - 1];
	if ((slot.flags & (u16)Mixer::Flags::Spatial) == 0) {
		return;
	}

	if (slot.hrtf_slot) {
		return;
	}

	if (mixer.free_hrtf_slots.empty()) {
		return;
	}

	slot.hrtf_slot = mixer.free_hrtf_slots[mixer.free_hrtf_slots.size() - 1];
	mixer.free_hrtf_slots.pop_back();

#ifndef DISABLE_STEAM_AUDIO
	if (mixer.ipl_hrtf != nullptr) {
		iplBinauralEffectReset(mixer.hrtf_slots[slot.hrtf_slot - 1].effect);
	}
#endif

#ifndef DISABLE_RESONANCE_AUDIO
	// For Resonance we don't need to reset the effect; ensure source exists
	if (mixer.ra_api != nullptr) {
		// no-op here; SetPlanarBuffer will overwrite previous frame data
	}
#endif
}

static void
Snd_ReleaseHRTFSlot(u32 slot_idx)
{
	if (!mixer.hrtf_enabled) return;

	auto& slot = mixer.slots[slot_idx - 1];
	if ((slot.flags & (u16)Mixer::Flags::Spatial) == 0) {
		return;
	}

	if (slot.hrtf_slot) {
		mixer.free_hrtf_slots.emplace_back(slot.hrtf_slot);
#ifndef DISABLE_RESONANCE_AUDIO
		// Optionally reset resonance source state (no explicit reset API)
		if (mixer.ra_api != nullptr) {
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

	mixer.slots[slot - 1].prev_state = mixer.slots[slot - 1].state;
	mixer.slots[slot - 1].state = state;
	mixer.slots[slot - 1].fake_state = state;
}

#define in_range(x, start, end) ((x) >= start && (x) <= end)

static u64
Snd_GetTimestamp()
{
	return std::chrono::high_resolution_clock::now().time_since_epoch().count();
}

static u32
Snd_Milliseconds()
{
	return (float)((Snd_GetTimestamp()) / 1000000);
}

static void
Snd_PurgeCacheLine(u32 cache_idx, bool purge_from_entry)
{
	auto& line = mixer.cache_lines[cache_idx - 1];

	if (purge_from_entry) {
		auto& source = mixer.sources[line.name.c_str()];
		for (u32& entry_cache_idx : source.cache_lines) {
			if (entry_cache_idx == cache_idx) {
				entry_cache_idx = 0;
				break;
			}
		}
	}

	memset(&mixer.cache_lines[cache_idx - 1], 0, sizeof(mixer.cache_lines[cache_idx - 1]));
	mixer.free_cachelines.push_back(cache_idx);
	cache_idx = 0;
}

static void
Snd_DestroySourceCache(sound_source& source)
{
	if (source.pub.ref_count == 0) {
		for (u32& cache_idx : source.cache_lines) {
			if (cache_idx != 0 && mixer.cache_lines[cache_idx - 1].name.size()) {
				Snd_PurgeCacheLine(cache_idx, false);
				cache_idx = 0;
			}
		}
	}
}

static u32
Snd_NewCacheLine()
{
	u32 cache_idx = 0;
	if (mixer.free_cachelines.empty()) {
		u64 least_timestamp = (u64)-1;
		for (size_t i = 0; i < mixer.cache_lines.size(); i++) {
			if (mixer.cache_lines[i].timestamp == 0) {
				// TODO: this might be a problem in the future
				continue;
			}

			if (mixer.cache_lines[i].timestamp < least_timestamp) {
				least_timestamp = mixer.cache_lines[i].timestamp;
				cache_idx = i + 1;
				continue;
			}
		}

		R_ASSERT(cache_idx);
		Snd_PurgeCacheLine(cache_idx, true);
	}

	{
		cache_idx = mixer.free_cachelines[mixer.free_cachelines.size() - 1];
		mixer.free_cachelines.pop_back();
		mixer.cache_lines[cache_idx - 1].timestamp = Snd_GetTimestamp();
	}
	return cache_idx;
}

static u32
Snd_TellSource(sound_source& source)
{
	return ov_pcm_tell(&source.file);
}

static u32
Snd_SeekSource(sound_source& source, u32 position, bool precise)
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

static void
Snd_LoadSource(sound_source& source, const char* name)
{
	PROF_EVENT("Sound: Load ogg");

	string_path fn, N;
	xr_strcpy(N, name);
	_strlwr(N);
	if (strext(N)) *strext(N) = 0;
	source.pub.name = N;

	xr_strconcat(fn, N, ".ogg");
	if (!FS.exist("$level$", fn))	FS.update_path(fn, "$game_sounds$", fn);
	if (!FS.exist(fn)) {
		FS.update_path(fn, "$game_sounds$", "$no_sound.ogg");
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
	source.reader = new IReader(source.data, source.pub.data_size);

	ov_callbacks ovc;
	ovc.read_func = ov_read_func;
	ovc.seek_func = ov_seek_func;
	ovc.close_func = ov_close_func;
	ovc.tell_func = ov_tell_func;
	ov_open_callbacks(source.reader, &source.file, nullptr, 0, ovc);

	vorbis_info* ovi = ov_info(&source.file, -1);
	R_ASSERT3(ovi, "Invalid source info:", source.pub.name.c_str());
	R_ASSERT(ovi->rate == SND_SAMPLERATE);
	source.pub.channels_count = ovi->channels;
	source.pub.frames_total = ov_pcm_total(&source.file, -1);

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

static void
Snd_AcquireSound(const char* name, bool fail_if_not_found)
{
	if (strlen(name) == 0) {
		return;
	}

	if (!mixer.sources.contains(name)) {
		R_ASSERT(!fail_if_not_found);
		// TODO: async file load?
		Snd_LoadSource(mixer.sources[name], name);
	}

	auto& source = mixer.sources[name];
	InterlockedIncrement(&source.pub.ref_count);
}

static void
Snd_ReleaseSound(const char* name)
{
	if (strlen(name) == 0) {
		return;
	}

	R_ASSERT(mixer.sources.contains(name));
	auto* source = &mixer.sources.at(name);
	R_ASSERT(source->pub.ref_count);
	InterlockedDecrement(&source->pub.ref_count);

	if (source->pub.ref_count == 0) {
		ov_clear(&source->file);
		if (source->data != nullptr) {
			xr_free(source->data);
		}

		Snd_DestroySourceCache(*source);
		mixer.sources.erase(name);
	}
}

static u32
Snd_ReadFromSource(sound_source& source, float** buffer, u32 frames)
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
		} else if (status < 0) {
			R_ASSERT2(false, "Decoding error");
		} else {
			last_frames -= status;
		}

		for (size_t ch = 0; ch < std::min((u8)SND_CHANNEL_COUNT, source.pub.channels_count); ch++) {
			for (size_t idx = 0; idx < status; idx++) {
				buffer[ch][offset + idx] = pcm[ch][idx];
			}
		}

		offset += status;
	} while (last_frames);

	if (source.pub.channels_count == 1) {
		memcpy(buffer[1], buffer[0], frames * sizeof(float));
	}

	return frames - last_frames;
}

static bool
Snd_SlotOcclusion(u32 slot_idx, float dt, float* occ_volume)
{
	auto& slot = mixer.slots[slot_idx - 1];
	if (slot.state != Mixer::State::Playing) {
		return false;
	}

	auto& source = mixer.sources.at(slot.sound_name.c_str());
	if (source.pub.channels_count == 1 && slot.flags & (u32)Mixer::Flags::Spatial) {
		// Check range
		Fvector& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
		Fvector& distances = slot.parameters[(u32)Mixer::ParameterId::DistanceRange];
		float dist = mixer.P.distance_to(pos);
		if (dist > distances.y) {
			if (occ_volume) *occ_volume = 0.0f;
			return false;
		}

		if (occ_volume != nullptr) {
			float occ = Sound->get_occlusion_to(mixer.P, pos);
			*occ_volume = occ;
		}
	}

	return true;
}

static u32
Snd_FindAvailableCacheLine(sound_source& source, u32 position)
{
	u32 needed_frames = std::min((u32)SND_BLOCKSIZE, source.pub.frames_total - position);
	u32 found_cache_idx = 0;
	for (const u32& cache_idx : source.cache_lines) {
		if (cache_idx != 0) {
			auto& line = mixer.cache_lines[cache_idx - 1];
			if (in_range(position, line.start, line.end) && in_range(position + needed_frames, line.start, line.end)) {
				found_cache_idx = cache_idx;
				mixer.stats.cache_hit_count++;
				break;
			}
		}
	}

	return found_cache_idx;
}

static void
Snd_UpdateCache(u32 slot_idx)
{
	PROF_EVENT("Sound: Update Slot Cache");

	auto& slot = mixer.slots[slot_idx - 1];
	if (slot.sound_name.empty() || !mixer.sources.contains(slot.sound_name.c_str())) {
		return;
	}

	Snd_AcquireSound(slot.sound_name.c_str(), true);
	auto& source = mixer.sources[slot.sound_name.c_str()];

	u32 found_cache_idx = Snd_FindAvailableCacheLine(source, slot.position);
	if (found_cache_idx == 0 && source.file.datasource != nullptr) {
		mixer.stats.cache_miss_count++;
		found_cache_idx = Snd_NewCacheLine();
		auto& line = mixer.cache_lines[found_cache_idx - 1];

		u32 cache_size = ((SND_BLOCKSIZE+1) * CACHE_LINE_WIDTH);

		{
			// TODO: parallel decoding for each slot
			u32 begin_pos = Snd_SeekSource(source, slot.position, false);
			u32 end_pos = begin_pos + cache_size;
			if (end_pos < (slot.position + SND_BLOCKSIZE)) {
				begin_pos = Snd_SeekSource(source, slot.position, true);
			} 
			
			memset(line.data, 0, cache_size * sizeof(float));

			float* ch_data[SND_CHANNEL_COUNT];
			for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
				ch_data[i] = line.data[i];
			}
			end_pos = begin_pos + Snd_ReadFromSource(source, ch_data, cache_size);

			R_ASSERT(in_range(slot.position, begin_pos, end_pos));
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
			for (size_t i = 0; i < CACHE_LINE_ENTRY_COUNT; i++)  {
				if (source.cache_lines[i] == 0 || source.cache_lines[i] == found_cache_idx) {
					continue;
				}

				if (mixer.cache_lines[source.cache_lines[i]-1].timestamp < least_timestamp) {
					least_timestamp = mixer.cache_lines[source.cache_lines[i]-1].timestamp;
					cache_entry_idx = i + 1;
					continue;
				}
			}

			if (cache_entry_idx != 0) {
				Snd_PurgeCacheLine(source.cache_lines[cache_entry_idx - 1], false);
				source.cache_lines[cache_entry_idx - 1] = found_cache_idx;
			} else {
				VERIFY2(false, "very bad bad bad");
			}
		}
	}

	Snd_ReleaseSound(slot.sound_name.c_str());
}

static u32
Snd_ReadSlotData(u32 slot_idx, float** data, u32 frames_count)
{
	auto& slot = mixer.slots[slot_idx - 1];
	if (slot.sound_name.c_str() == nullptr || !mixer.sources.contains(slot.sound_name.c_str())) {
		return frames_count;
	}

	Snd_AcquireSound(slot.sound_name.c_str(), true);

	u32 frames_to_read = frames_count;
	do {
		auto& source = mixer.sources[slot.sound_name.c_str()];
		u32 found_cache_idx = Snd_FindAvailableCacheLine(source, slot.position);
		u32 begin_pos = 0, end_pos = 0;

		if (found_cache_idx == 0) {
			Snd_UpdateCache(slot_idx);
			found_cache_idx = Snd_FindAvailableCacheLine(source, slot.position);
			VERIFY(found_cache_idx != 0);
		}

		if (found_cache_idx == 0) {
			break;
		}

		auto& cache_line = mixer.cache_lines[found_cache_idx - 1];
		u32 begin_offset = slot.position - cache_line.start;
		u32 last_frames = source.pub.frames_total - slot.position;
		u32 cache_frames = std::min(std::min(frames_to_read, cache_line.end-slot.position), (u32)SND_BLOCKSIZE);
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			memcpy(&data[ch][frames_count-frames_to_read], &cache_line.data[ch][begin_offset], cache_frames * sizeof(float));
		}

		frames_to_read -= cache_frames;
		if (slot.position + cache_frames >= source.pub.frames_total) {
			break;
		}
	} while (frames_to_read);

	Snd_ReleaseSound(slot.sound_name.c_str());

	return (u32)std::min(std::max((s64)frames_count- (s64)frames_to_read, (s64)0), (s64)frames_count);
}

static void
Snd_ReadSlot(u32 slot_idx, float** data, u32 frames_count)
{
	auto& slot = mixer.slots[slot_idx - 1];
	auto& source = mixer.sources.at(slot.sound_name.c_str());

	u32 last_frames = frames_count;
	while (last_frames) {
		float* offfseted_data[SND_CHANNEL_COUNT];
		u32 buf_offset = (frames_count - last_frames);
		for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
			offfseted_data[i] = &data[i][buf_offset];
		}

		u32 read_frames = Snd_ReadSlotData(slot_idx, offfseted_data, last_frames);
		if (slot.position + read_frames >= source.pub.frames_total) {
			slot.position = 0;
			if (slot.flags & (u32)Mixer::Flags::Looped) {
				continue;
			} else {
				MixerNewState(slot_idx, Mixer::State::Stopped);
				break;
			}
		}

		u32 total_frames = source.pub.frames_total;
		slot.position = std::min(slot.position + read_frames, total_frames);
		last_frames -= read_frames;
	}
}

static void
Snd_ProcessSlot(u32 slot_idx, float** data)
{
	auto& slot = mixer.slots[slot_idx - 1];
	float pitch = slot.parameters[(u32)Mixer::ParameterId::Pitch].x;

	u32 output_frames = SND_BLOCKSIZE;
	float scaled_frames = (float)output_frames * pitch * mixer.time_factor;
	u32 input_frames = std::min((u32)scaled_frames, (u32)SND_BLOCKSIZE);//ceilf(scaled_frames - 1e-6f);

	bool is_music = (slot.flags & (u16)Mixer::Flags::Intro);

	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		memset(mixer.read_buffer[ch], 0, (input_frames + 1) * sizeof(float));
	}
	if (is_music || fis_zero(1.0 - mixer.time_factor)) {
		Snd_ReadSlot(slot_idx, data, SND_BLOCKSIZE);
	} else {
		float* offfseted_data[SND_CHANNEL_COUNT];
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			offfseted_data[ch] = mixer.read_buffer[ch];
		}

		Snd_ReadSlot(slot_idx, offfseted_data, input_frames+1);
		slot.position -= 1; // account 1 sample of tail for interpolation
		DSP_ResampleBuffer(offfseted_data, data, slot.history, input_frames, output_frames);
	}
}

static void
Snd_PrecacheRenderCallback()
{
	PROF_EVENT("Sound: Precache Stage");

	static u64 counter = 0;
	static u64 timestamp = Snd_GetTimestamp();

	xrSRWLockGuard g1(mixer.manage_lock);
	float dt = (float)((double)(Snd_GetTimestamp() - timestamp) / 1000000000.0);

	mixer.stats.frame_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
	timestamp = Snd_GetTimestamp();

	if (counter % 100 == 0) {
		mixer.stats.cache_hit_count = 0;
		mixer.stats.cache_miss_count = 0;
	}

	for (size_t i = 0; i < mixer.slots.size(); i++) {
		if (Snd_SlotOcclusion(i + 1, dt, nullptr)) {
			Snd_AcquireHRTFSlot(i + 1);
			Snd_UpdateCache(i + 1);
		} else {
			Snd_ReleaseHRTFSlot(i + 1);
		}
	}

	mixer.stats.cache_lines_total = CACHE_LINES_COUNT;
	mixer.stats.cache_lines_free = mixer.free_cachelines.size();
	mixer.stats.precache_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
	counter++;
}

static void
Snd_PhononSpatialProcess(float** data, u32 slot_idx)
{
	auto& slot = mixer.slots[slot_idx - 1];
	if ((slot.flags & (u32)Mixer::Flags::Spatial) == 0) {
		return;
	}

	Fvector& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
	Fvector& distances = slot.parameters[(u32)Mixer::ParameterId::DistanceRange];

	if (slot.hrtf_slot == 0) {
		DSP_SpatialProcess(data, slot.parameters[(u32)Mixer::ParameterId::DistanceRange], mixer.P, mixer.D, mixer.N, pos);
		return;
	}

#ifndef DISABLE_STEAM_AUDIO
	auto& hrtf_slot = mixer.hrtf_slots[slot.hrtf_slot - 1];
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		memcpy(hrtf_slot.process_buffer[ch], data[ch], SND_BLOCKSIZE * sizeof(float));
	}

	Fvector relative_pos;
	float distance;
	DSP_CalculateRelativePosition(mixer.P, mixer.D, mixer.N, pos, relative_pos, distance);
	distance = std::max(distance, 0.1f);
	relative_pos.normalize();

	// HRTF
	IPLBinauralEffectParams binaural_params = { 
		.direction = {relative_pos.x, relative_pos.y, relative_pos.z}, 
		.spatialBlend = 1.0f, .hrtf = mixer.ipl_hrtf
	};
	IPLAudioBuffer out_buf = { .numChannels = SND_CHANNEL_COUNT, .numSamples = SND_BLOCKSIZE, .data = data };
	R_ASSERT(iplBinauralEffectApply(hrtf_slot.effect, &binaural_params, &hrtf_slot.buf_desc, &out_buf) == IPL_STATUS_SUCCESS);

	// Attenuation
	distance = std::clamp(distance, distances.x, distances.y);
	float att = distances.x / (psSoundRolloff * distance);
	att *= att;
	att *= 1.0f - std::clamp(std::max(distance - distances.x, 0.0f) / ((distances.y - distances.x)), 0.0f, 1.0f);
	att = std::clamp(att, 0.f, 1.f);
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
			data[ch][i] *= att;
		}
	}
#elif !defined(DISABLE_RESONANCE_AUDIO)
	auto& hrtf_slot = mixer.hrtf_slots[slot.hrtf_slot - 1];
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		memcpy(hrtf_slot.process_buffer[ch], data[ch], SND_BLOCKSIZE * sizeof(float));
	}

	Fvector relative_pos;
	float distance;
	DSP_CalculateRelativePosition(mixer.P, mixer.D, mixer.N, pos, relative_pos, distance);
	distance = std::max(distance, 0.1f);

	mixer.ra_api->SetHeadPosition(mixer.P.x, mixer.P.y, mixer.P.z);
	if (mixer.ra_api != nullptr) {
		mixer.ra_api->SetSourcePosition(hrtf_slot.source_id, pos.x, pos.y, pos.z);
		mixer.ra_api->SetPlanarBuffer(hrtf_slot.source_id, (const float* const*)data, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
		mixer.ra_api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, data);
	}

	// Attenuation
	distance = std::clamp(distance, distances.x, distances.y);
	float att = distances.x / (psSoundRolloff * distance);
	att *= att;
	att *= 1.0f - std::clamp(std::max(distance - distances.x, 0.0f) / ((distances.y - distances.x)), 0.0f, 1.0f);
	att = std::clamp(att, 0.f, 1.f);
	for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
		for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
			data[ch][i] *= att;
		}
	}
#endif
}

static void
Snd_MixerRenderCallback(float* buffer)
{
	PROF_EVENT("Sound: Render Stage");

	xrSRWLockGuard guard(mixer.render_lock, true);

	static u64 timestamp = Snd_GetTimestamp();
	float dt = (float)((double)(Snd_GetTimestamp() - timestamp) / 1000000000.0);
	timestamp = Snd_GetTimestamp();

	memset(buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
	static float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = { 0 };

	float* process_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
		process_buffer[i] = _process_buffer[i];
	}

	for (size_t i = 0; i < SND_BUS_COUNT; i++) {
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			memset(mixer.buses[i].data[ch], 0, SND_BLOCKSIZE * sizeof(float));
		}
	}

	for (auto& zone : mixer.zones) {
		zone.use_count = 0;
		memset(zone.data, 0, sizeof(zone.data));
	}

	for (size_t i = 0; i < mixer.slots.size(); i++) {
		PROF_EVENT("Slot Render");
		if (mixer.slots[i].state != Mixer::State::Playing) {
			continue;
		}

		if (!mixer.sources.contains(mixer.slots[i].sound_name.c_str())) {
			MixerNewState(i + 1, Mixer::State::Stopped);
			continue;
		}

		auto& slot = mixer.slots[i];
		auto& source = mixer.sources.at(mixer.slots[i].sound_name);

		float occ_volume = 1.0f;
		if (!Snd_SlotOcclusion(i + 1, dt, &occ_volume)) {
			// TODO: hack for simulated sounds
			slot.position = std::min(slot.position + SND_BLOCKSIZE, source.pub.frames_total);
			if (slot.position == source.pub.frames_total) {
				MixerNewState(i + 1, Mixer::State::Stopped);
				continue;
			}

			continue;
		}

		// Clear process buffer and read data from source
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			memset(process_buffer[ch], 0, SND_BLOCKSIZE * sizeof(float));
		}

		Snd_ProcessSlot(i + 1, process_buffer);

		if (!mixer.sources.contains(mixer.slots[i].sound_name)) {
			MixerNewState(i + 1, Mixer::State::Stopped);
			continue;
		}

		Fvector& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
		Fvector& volumes = slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel];
		float begin_factor = 1.0f, end_factor = 1.0f;

		// Deferred stopping
		bool is_music = (slot.flags & (u16)Mixer::Flags::Intro);
		
		if (slot.stopping_position != (u32)-1) {
			u32 stopping_total = source.pub.frames_total - slot.stopping_position;
			if (stopping_total != 0) {
				u32 begin_offset = slot.position - slot.stopping_position;
				u32 write_count = is_music ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * mixer.time_factor);
				u32 end_offset = std::min(begin_offset + write_count, source.pub.frames_total - 1);
				begin_factor = 1.0f - ((float)begin_offset / (float)(stopping_total - 1));
				end_factor = 1.0f - ((float)end_offset / (float)(stopping_total - 1));
			}
		}

		// Apply final volumes
		float slot_volume = volumes.x * volumes.y * volumes.z;
		float volume_final = occ_volume * slot_volume * (is_music ? mixer.music_volume : mixer.effect_volume);
		begin_factor *= volume_final;
		end_factor *= volume_final;

		// Spatial processing
		if (!(slot.flags & (u32)Mixer::Flags::Intro) && source.pub.channels_count == 1) {
			PROF_EVENT("Slot Spatial");
			if (slot.flags & (u32)Mixer::Flags::Spatial) {
#if !defined(DISABLE_STEAM_AUDIO) || 0//!defined(DISABLE_RESONANCE_AUDIO)
				if (psSoundFlags.is(ss_HRTF) && mixer.hrtf_enabled)
				{
					Snd_PhononSpatialProcess(process_buffer, i + 1);
				}
				else
#endif
				{
					DSP_SpatialProcess(process_buffer, slot.parameters[(u32)Mixer::ParameterId::DistanceRange], mixer.P, mixer.D, mixer.N, pos);
				}
			}

			if (mixer.editor_zone)
			{
				slot.zone_idx = 1;
			}

			u32 slot_idx = slot.zone_idx;// ((slot.flags & (u32)Mixer::Flags::Spatial) ? slot.zone_idx : );
			if (slot_idx && !mixer.zones.empty()) {
				sound_zone_params& zone = mixer.zones.at(slot_idx - 1);
				zone.use_count++;
				zone.last_use_ms = Snd_Milliseconds();

				float* reverb_buffer[SND_CHANNEL_COUNT] = {};
				for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
					reverb_buffer[ch] = zone.data[ch];
				}

				// TODO(vertver): better volume attenutation for reverb stuff
				DSP_MixBuffer(reverb_buffer, process_buffer, begin_factor, end_factor, SND_BLOCKSIZE);
			}
		}

		// TODO(vertver): push data to buses instead of main
		int bus_idx = is_music ? SND_BUS_MUSIC : SND_BUS_EFFECTS;
		float* bus_buffer[SND_CHANNEL_COUNT] = {};
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			bus_buffer[ch] = mixer.buses[bus_idx].data[ch];
		}

		// Bus mixing
		DSP_MixBuffer(bus_buffer, process_buffer, begin_factor, end_factor, SND_BLOCKSIZE);
	}

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
		process_buffer[i] = _process_buffer[i];
	}

	// Reverb mixing
	if (psSoundFlags.is(ss_EFX)) {
		for (auto& zone : mixer.zones) {
			if (zone.use_count == 0 && (zone.last_use_ms + 3000) < Snd_Milliseconds()) {
				for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
					//zone.position[ch] = 0;
				}
				continue;
			}

			PROF_EVENT("Reverb rendering");
			float* reverb_buffer[SND_CHANNEL_COUNT] = {};
			float* bus_buffer[SND_CHANNEL_COUNT] = {};

			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
				reverb_buffer[ch] = zone.data[ch];
				bus_buffer[ch] = mixer.buses[SND_BUS_REVERB].data[ch];
			}

#if 0
			DSP_AlgorithmicReverb(zone.state, zone.settings, reverb_buffer, process_buffer, SND_BLOCKSIZE);
			DSP_MixBuffer(bus_buffer, process_buffer, zone.settings.reverb * 0.5, zone.settings.reverb * 0.5, SND_BLOCKSIZE);
#else

#ifndef DISABLE_RESONANCE_AUDIO
			zone.state.ra_context->SetPlanarBuffer(zone.state.buffer, reverb_buffer, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
			zone.state.ra_context->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, process_buffer);
			DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, bus_buffer, 1.0f, SND_BLOCKSIZE, zone.compressor_envelope);
#endif

#ifndef DISABLE_STEAM_AUDIO
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
				IPLAudioBuffer reverb_buf = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &reverb_buffer[ch] };
				IPLAudioBuffer out_buf = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &process_buffer[ch] };
				IPLReflectionEffectParams params = {};
				ConvertReverbSettings(&zone.settings, &params, SND_SAMPLERATE);
				iplReflectionEffectApply(zone.effect[ch], &params, &reverb_buf, &out_buf, nullptr);
			}
#endif
			float reverb_gain = std::clamp(zone.settings.reverb, 0.0f, 1.0f) * 0.010f;
			DSP_MixBuffer(bus_buffer, process_buffer, reverb_gain, reverb_gain, SND_BLOCKSIZE);
#endif
		}
	}

	{
		PROF_EVENT("Sound Mixing");
		float* master_buffer[SND_CHANNEL_COUNT] = {};
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
#if 0
			master_buffer[ch] = mixer.buses[SND_BUS_REVERB].data[ch];
#else
			master_buffer[ch] = mixer.buses[SND_BUS_MASTER].data[ch];
#endif
		}

#if 1
		// Master mixing
		for (size_t i = 0; i < SND_BUS_COUNT; i++) {
			float* bus_buffer[SND_CHANNEL_COUNT] = {};
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
				bus_buffer[ch] = mixer.buses[i].data[ch];
			}

			DSP_MixBuffer(master_buffer, bus_buffer, 1.0f, 1.0f, SND_BLOCKSIZE);
		}
#endif

		DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, master_buffer, mixer.compression, SND_BLOCKSIZE, mixer.compressor_envelope);

		// Clipping and master volume adjust
		for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
			for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
				float sample = master_buffer[ch][i];
				sample = std::clamp(sample, -1.0f, 1.0f) * mixer.master_volume;
				buffer[i * SND_CHANNEL_COUNT + ch] = sample;
			}
		}
	}

#ifdef DEBUG_DRAW

	/*
	for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
		float sample = ((buffer[i * SND_CHANNEL_COUNT + 0] + buffer[i * SND_CHANNEL_COUNT + 1]) * 0.5f);
		mixer.aligned_input_fft[i] = mixer.fft_window[i] * sample;
	}

	pffft_transform_ordered(mixer.fft_setup, mixer.aligned_input_fft, mixer.aligned_output_fft, NULL, PFFFT_FORWARD);
	float* fft_out_real = &mixer.aligned_output_fft[0];
	float* fft_out_imag = &mixer.aligned_output_fft[SND_BLOCKSIZE];

	for (size_t i = 0; i < SND_BLOCKSIZE*2; i++) {
		float sample = sqrtf(powf(fft_out_real[i], 2) + powf(fft_out_imag[i], 2));
		//float sample = sqrtf(powf(fft_out_real[i], 2) + powf(fft_out_imag[i], 2));
		//mixer.stats.spectral_data[i] = sample;// lin2dB(sample / f32(SND_BLOCKSIZE));
		//float sample = (atan2f(fft_out_imag[i], fft_out_real[i]) + M_PI) / M_PI * 2;// () / M_PI * 2;// sqrtf(powf(, 2) + powf(, 2));
		mixer.stats.spectral_data[i/2] = lin2dB(sample / f32(SND_BLOCKSIZE));
	}

	float volumes[SND_CHANNEL_COUNT] = { 0 };
	for (size_t j = 0; j < SND_BLOCKSIZE; j++) {
		for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
			volumes[i] = (volumes[i] + fabs(buffer[j * SND_CHANNEL_COUNT + i])) * 0.5f;
		}
	}

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
		volumes[i] = lin2dB(volumes[i]);
	}

	memcpy(mixer.stats.channel_volumes, volumes, sizeof(volumes));
	*/
#endif

	mixer.stats.render_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
}

void 
Mixer::Initialize()
{
	mixer.slots.clear();
	mixer.free_slots.clear();
	mixer.free_cachelines.clear();
	mixer.cache_lines.clear();
	mixer.sounds.clear();
	mixer.cmd.clear();
	mixer.slots.resize(DEFAULT_SLOT_COUNT);
	mixer.free_slots.resize(DEFAULT_SLOT_COUNT);
	mixer.cache_lines.resize(CACHE_LINES_COUNT);
	mixer.free_cachelines.resize(CACHE_LINES_COUNT);
	mixer.cmd.reserve(256);
	mixer.stats.possible_free_count = DEFAULT_SLOT_COUNT;
	for (size_t i = 0; i < DEFAULT_SLOT_COUNT; i++) {
		mixer.free_slots[i] = i + 1;
	}
	for (size_t i = 0; i < CACHE_LINES_COUNT; i++) {
		mixer.free_cachelines[i] = i + 1;
	}

#ifndef DISABLE_STEAM_AUDIO
	mixer.hrtf_enabled = true;
	IPLContextSettings ipl_settings = { .version = STEAMAUDIO_VERSION, .simdLevel = IPL_SIMDLEVEL_SSE2 };
	IPLAudioSettings settings = { .samplingRate = SND_SAMPLERATE, .frameSize = SND_BLOCKSIZE };
	IPLerror err = iplContextCreate(&ipl_settings, &mixer.ipl_context);
	R_ASSERT(err == IPL_STATUS_SUCCESS);

	mixer.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		mixer.free_hrtf_slots[i] = i + 1;
	}

	IPLHRTFSettings hrtf_settings = { .type = IPL_HRTFTYPE_DEFAULT, .volume = 1.0f };
	R_ASSERT(iplHRTFCreate(mixer.ipl_context, &settings, &hrtf_settings, &mixer.ipl_hrtf) == IPL_STATUS_SUCCESS);

	mixer.hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		IPLBinauralEffectSettings binaural = { .hrtf = mixer.ipl_hrtf };
		R_ASSERT(iplBinauralEffectCreate(mixer.ipl_context, &settings, &binaural, &mixer.hrtf_slots[i].effect) == IPL_STATUS_SUCCESS);
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			mixer.hrtf_slots[i].process_buffer[ch] = mixer.hrtf_slots[i]._process_buffer[ch];
		}

		mixer.hrtf_slots[i].buf_desc = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = mixer.hrtf_slots[i].process_buffer };
	}
#elif !defined(DISABLE_RESONANCE_AUDIO)
	// Initialize Resonance Audio API and HRTF slots
	mixer.hrtf_enabled = true;
	mixer.ra_api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	
	R_ASSERT(mixer.ra_api != nullptr);

	mixer.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		mixer.free_hrtf_slots[i] = i + 1;
	}

	mixer.hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			mixer.hrtf_slots[i].process_buffer[ch] = mixer.hrtf_slots[i]._process_buffer[ch];
		}
		// Create a Resonance sound object source for this slot
		mixer.hrtf_slots[i].source_id = mixer.ra_api->CreateSoundObjectSource(vraudio::RenderingMode::kBinauralHighQuality);
	}
#endif

#ifdef DEBUG_DRAW
#pragma todo(replace with aligned allocators)
	// Blackman-Harris window
	for (int i = 0; i < SND_BLOCKSIZE; ++i) {
		mixer.fft_window[i] = .5 * (1. - cosf(2. * 3.1415926535897932384 * (f64)i / (f64)(SND_BLOCKSIZE-1)));
	}

	mixer.aligned_input_fft = (float*)_aligned_malloc(SND_BLOCKSIZE * sizeof(float), 16);
	mixer.aligned_output_fft = (float*)_aligned_malloc(SND_BLOCKSIZE * 2 * sizeof(float), 16);
	mixer.fft_setup = pffft_new_setup(SND_BLOCKSIZE, PFFFT_REAL);
#endif

	Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void 
Mixer::Shutdown()
{
	Backend::Shutdown();

#ifdef DEBUG_DRAW
	if (mixer.fft_setup) {
		pffft_destroy_setup(mixer.fft_setup);
		mixer.fft_setup = nullptr;
	}
#endif

#ifndef DISABLE_STEAM_AUDIO
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++) {
		if (mixer.hrtf_slots[i].effect != nullptr) {
			iplBinauralEffectRelease(&mixer.hrtf_slots[i].effect);
		}
	}

	if (mixer.ipl_hrtf) iplHRTFRelease(&mixer.ipl_hrtf);
	if (mixer.ipl_context) iplContextRelease(&mixer.ipl_context);
	mixer.hrtf_slots.clear();
	mixer.free_hrtf_slots.clear();
#endif

#ifndef DISABLE_RESONANCE_AUDIO
	if (mixer.ra_api) {
		for (size_t i = 0; i < mixer.hrtf_slots.size(); ++i) {
			auto id = mixer.hrtf_slots[i].source_id;
			if (id != vraudio::ResonanceAudioApi::kInvalidSourceId) {
				mixer.ra_api->DestroySource(id);
			}
		}
		delete mixer.ra_api;
		mixer.ra_api = nullptr;
	}
	mixer.hrtf_slots.clear();
	mixer.free_hrtf_slots.clear();
#endif

	mixer.slots.clear();
	mixer.free_slots.clear();
	mixer.free_cachelines.clear();
	mixer.cache_lines.clear();
	mixer.sounds.clear();
	mixer.cmd.clear();
}

static void
DestroyInternal(int slot)
{
	if (slot == 0) {
		return;
	}

	if (!mixer.slots[slot - 1].sound_name.empty()) {
		Snd_ReleaseSound(mixer.slots[slot - 1].sound_name.c_str());
		mixer.slots[slot - 1].sound_name.clear();
	}

	memset(mixer.slots[slot - 1].parameters, 0, sizeof(mixer.slots[slot - 1].parameters));
	memset(mixer.slots[slot - 1].history, 0, sizeof(mixer.slots[slot - 1].history));
	mixer.slots[slot - 1].position = 0;
	mixer.slots[slot - 1].stopping_position = (u32)-1;
	mixer.slots[slot - 1].flags = 0;
	mixer.slots[slot - 1].state = Mixer::State::Stopped;
	mixer.slots[slot - 1].prev_state = Mixer::State::Stopped;
	mixer.slots[slot - 1].fake_state = Mixer::State::Stopped;
	mixer.free_slots.push_back(slot);
}

IC void	volume_lerp(float& c, float t, float s, float dt)
{
	float diff = t - c;
	float diff_a = _abs(diff);
	if (diff_a < EPS_S) return;
	float mot = s * dt;
	if (mot > diff_a) mot = diff_a;
	c += (diff / diff_a) * mot;
}

void 
Mixer::Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float compression, const Fmatrix& mtx, Fvector P, Fvector D, Fvector N)
{
	PROF_EVENT("Sound: Update Stage");
	sound_event* handler = (sound_event*)event_handler;

	static u64 timestamp = Snd_GetTimestamp();
	float dt = (float)((Snd_GetTimestamp() - timestamp) / 1000000) * 0.001f;
	timestamp = Snd_GetTimestamp();

	mixer.time_factor = std::clamp(time_factor, 0.1f, 10.0f);
	mixer.compression = compression;
	mixer.master_volume = volume;
	mixer.effect_volume = eff_volume;
	mixer.music_volume = mus_volume;
	mixer.m_V = mtx;
	mixer.P = P;
	mixer.D = D;
	mixer.N = N;

	mixer.update_lock.AcquireExclusive();
	mixer.manage_lock.AcquireExclusive();

	for (auto sound : mixer.sounds) {
		if (sound == nullptr || !sound->unique_id() || !sound->slot() || sound->_g_object() == nullptr) {
			continue;
		}

		auto& slot = mixer.slots[sound->slot() - 1];
		if (slot.fake_state != State::Playing || slot.state != State::Playing) {
			continue;
		}
		CObject* object = sound->_g_object();
		if (slot.flags & (u16)Flags::Spatial && (slot.flags & (u16)Flags::NoPosUpdate) == 0) {
			if (object != nullptr) {
				auto& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
				pos = ((IRenderable*)object)->renderable.xform.c;
			}
		}
	}

	for (size_t i = 0; i < mixer.slots.size(); i++) {
		auto& slot = mixer.slots[i];
		if (slot.flags & (u16)Flags::NoFeedback && slot.state == State::Stopped) {
			Destroy(i + 1);
		} else if (((slot.flags & (u16)Flags::Intro) == 0) && slot.state == State::Playing) {
			Fvector pos = (slot.flags & (u16)Flags::Spatial) ? slot.parameters[(u32)Mixer::ParameterId::Position] : mixer.P ;
			float dist = mixer.P.distance_to(pos);
			if (dist <= slot.parameters[(u32)Mixer::ParameterId::DistanceRange].y) {
				float out_occ = ::Sound->get_occlusion(pos, 0.2f, &mixer.occ);
				float& old_occ = slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
				volume_lerp(old_occ, out_occ, 1.0f, dt);

				CDB::MODEL* env_model = ::Sound->get_geometry_env();
				CDB::COLLIDER* collider = ::Sound->get_geometry_db();
				if (env_model != nullptr) {
					Fvector	dir = { 0,-1,0 };
					collider->ray_options(CDB::OPT_ONLYNEAREST);
					collider->ray_query(env_model, pos, dir, 1000.f);
					if (collider->r_count()) {
						CDB::RESULT* r = collider->r_begin();
						CDB::TRI* T = env_model->get_tris() + r->id;
						Fvector* V = env_model->get_verts();

						Fvector tri_norm;
						tri_norm.mknormal(V[T->verts[0]], V[T->verts[1]], V[T->verts[2]]);

						float dot = dir.dotproduct(tri_norm);
						R_ASSERT(T->dummy < mixer.zones.size());
						slot.zone_idx = T->dummy + 1;
					} else {
						slot.zone_idx = 0;
					}
				} else {
					slot.zone_idx = 0;
				}
			}
		}
	}

	for (const auto& cmd : mixer.cmd) {
		switch (cmd.id) {
		case sound_cmd_id::play: {
#if 1
			bool sound_exists = (cmd.param1 && mixer.sounds.contains((ref_sound*)cmd.param1));
			ref_sound* sound = sound_exists ? (ref_sound*)cmd.param1 : nullptr;
#else
			ref_sound* sound = (ref_sound*)cmd.param1;
#endif
			u16 flags = cmd.param0;
			double delay = *(double*)&cmd.param2;
			CObject* obj = (CObject*)cmd.param3;

			bool is_same_file = mixer.slots[cmd.slot - 1].sound_name == cmd.string_storage.c_str();
			if (!is_same_file && !mixer.slots[cmd.slot - 1].sound_name.empty()) {
				Snd_ReleaseSound(mixer.slots[cmd.slot - 1].sound_name.c_str());
				mixer.slots[cmd.slot - 1].sound_name.clear();
			}

			if (!is_same_file) {
				Snd_AcquireSound(cmd.string_storage.c_str(), false);
			}

			auto& source = mixer.sources.at(cmd.string_storage.c_str());
			memset(mixer.slots[cmd.slot - 1].parameters, 0, sizeof(mixer.slots[cmd.slot - 1].parameters));
			memset(mixer.slots[cmd.slot - 1].history, 0, sizeof(mixer.slots[cmd.slot - 1].history));
			mixer.slots[cmd.slot - 1].parameters[(u32)Mixer::ParameterId::VolumePerChannel] = Fvector(source.pub.volume, 1.0f, 1.0f);
			mixer.slots[cmd.slot - 1].parameters[(u32)Mixer::ParameterId::DistanceRange] = Fvector(source.pub.min_distance, source.pub.max_distance, source.pub.max_ai_distance);
			mixer.slots[cmd.slot - 1].parameters[(u32)Mixer::ParameterId::Pitch] = Fvector(1.0f, 1.0f, 1.0f);
			mixer.slots[cmd.slot - 1].position = 0;
			mixer.slots[cmd.slot - 1].stopping_position = (u32)-1;
			mixer.slots[cmd.slot - 1].sound_name = cmd.string_storage.c_str();
			mixer.slots[cmd.slot - 1].flags = flags;
			
			if (handler != nullptr) {
				float clip = source.pub.max_ai_distance * source.pub.volume;
				float range = _min(source.pub.max_ai_distance, clip);

				if (range >= 0.1f) {
					if (flags & (u16)Flags::NoFeedback) {
						if (obj) {
							ref_sound_data_ptr data_ptr = new ref_sound_data();
							data_ptr->slot = cmd.slot;
							data_ptr->g_type = 0;
							data_ptr->g_object = obj;
							data_ptr->dont_destroy_slot = true;
							data_ptr->fn_attached[0] = source.pub.path;
							handler(data_ptr, range);
						}
					} else {
						if (sound != nullptr && sound->_p != nullptr && sound->_p->g_object != nullptr) {
							handler(sound->_p, range);
						}
					}
				}
			}

			MixerNewState(cmd.slot, State::Playing);
		} break;
		case sound_cmd_id::pause: {
			MixerNewState(cmd.slot, State::Paused);
		} break;
		case sound_cmd_id::stop: {
			if (cmd.param0) {
				mixer.slots[cmd.slot - 1].flags &= ~((u32)Flags::Looped);
				mixer.slots[cmd.slot - 1].stopping_position = mixer.slots[cmd.slot - 1].position;
			} else {
				MixerNewState(cmd.slot, State::Stopped);
				mixer.slots[cmd.slot - 1].position = 0;
				mixer.slots[cmd.slot - 1].stopping_position = (u32)-1;
			}
		} break;
		case sound_cmd_id::destroy: {
			DestroyInternal(cmd.slot);
		} break;
		case sound_cmd_id::stop_all: {
			for (size_t i = 0; i < mixer.slots.size(); i++) {
				if (mixer.slots[i].sound_name.size() && mixer.slots[i].state != State::Stopped) {
					mixer.slots[i].position = 0;
					mixer.slots[i].stopping_position = (u32)-1;
					mixer.slots[i].prev_state = mixer.slots[i].state;
					mixer.slots[i].state = State::Stopped;
					mixer.slots[i].fake_state = State::Stopped;
				}
			}
		} break;
		case sound_cmd_id::pause_all: {
			for (size_t i = 0; i < mixer.slots.size(); i++) {
				if (mixer.slots[i].sound_name.size() && mixer.slots[i].state != State::Stopped) {
					mixer.slots[i].prev_state = mixer.slots[i].state;
					mixer.slots[i].state = State::Paused;
					mixer.slots[i].fake_state = State::Paused;
				}
			}
		} break;
		case sound_cmd_id::resume_all: {
			for (size_t i = 0; i < mixer.slots.size(); i++) {
				if (mixer.slots[i].state == State::Paused) {
					mixer.slots[i].state = mixer.slots[i].prev_state;
					mixer.slots[i].prev_state = State::Paused;
					mixer.slots[i].fake_state = State::Playing;
				}
			}
		} break;
		case sound_cmd_id::update_parameter: {
			mixer.slots[cmd.slot - 1].parameters[(u32)cmd.param0] = Fvector(*(double*)&cmd.param1, *(double*)&cmd.param2, *(double*)&cmd.param3);

			auto& slot = mixer.slots[cmd.slot - 1];
			if (slot.flags & (u16)Flags::Spatial && cmd.param0 == (u16)ParameterId::Position) {
				auto& pos = slot.parameters[(u32)Mixer::ParameterId::Position];
				float out_occ = ::Sound->get_occlusion(pos, 0.2f, &mixer.occ);
				float& old_occ = slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
				volume_lerp(old_occ, out_occ, 1.0f, dt);
			}
		} break;
		case sound_cmd_id::set_volume: {
			mixer.slots[cmd.slot - 1].parameters[(u32)ParameterId::VolumePerChannel].y = *(double*)&cmd.param1;
		} break;
		}
	}

	mixer.cmd.resize(0);
	mixer.stats.update_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;

	mixer.manage_lock.ReleaseExclusive();
	mixer.update_lock.ReleaseExclusive();
}

void 
Mixer::StopAll()
{
	mixer.cmd.emplace_back(sound_command{ .id = sound_cmd_id::stop_all });
}

void 
Mixer::PauseAll()
{
	mixer.cmd.emplace_back(sound_command{ .id = sound_cmd_id::pause_all });
}

void
Mixer::ResumeAll()
{
	mixer.cmd.emplace_back(sound_command{ .id = sound_cmd_id::resume_all });
}

void 
Mixer::DereferenceObjects(CObject** object, int count)
{
	xrSRWLockGuard g0(mixer.update_lock);
	xrSRWLockGuard g1(mixer.manage_lock);

	for (auto sound : mixer.sounds) {
		if (sound == nullptr || sound->_p == nullptr) {
			continue;
		}

		for (size_t i = 0; i < count; i++) {
			if (object[i] == sound->_g_object()) {
				sound->_p->g_object = nullptr;
			}
		}
	}
}

u32 
Mixer::Create()
{
	xrSRWLockGuard g0(mixer.update_lock);

	// TODO: fix memory leak (probably xrGame-related issue)
	//Msg("Free Sounds: %d", mixer.free_slots.size());
	VERIFY2(!mixer.free_slots.empty(), "Unnable to allocate new sound slot");
	if (mixer.free_slots.empty()) {
		return 0;
	}

	u32 slot_idx = mixer.free_slots[mixer.free_slots.size() - 1];
	mixer.free_slots.pop_back();
	mixer.stats.possible_free_count--;

	return slot_idx;
}

void
Mixer::Destroy(u32 slot)
{
	if (slot == 0) {
		return;
	}

	mixer.slots[slot - 1].fake_state = State::Stopped;
	//mixer.slots[slot - 1].sound = nullptr;
	mixer.cmd.emplace_back(sound_command{ .slot = slot, .id = sound_cmd_id::destroy });
	mixer.stats.possible_free_count++;
}

void 
Mixer::Play(u32 slot, u16 flags, ref_sound* sound, double delay)
{
	if (slot == 0 || sound == nullptr || sound->_p == nullptr || sound->_p->fn_attached[0] == nullptr) {
		return;
	}

	mixer.slots[slot - 1].fake_state = State::Playing;
	mixer.cmd.emplace_back(sound_command{ 
		.slot = slot, .id = sound_cmd_id::play, .param0 = flags, .param1 = (u64)sound, 
		.param2 = *(u64*)&delay, .param3 = (u64)sound->_g_object(), 
		.string_storage = sound->_p->fn_attached[0].c_str()
	});
}

void 
Mixer::PlayNoFeedback(u16 flags, ref_sound* sound, CObject* obj, double delay, float* pitch, float* volume, Fvector* distance, Fvector* pos)
{
	u32 slot_idx = Create();
	if (slot_idx == 0) {
		return;
	}

	auto& slot = mixer.slots[slot_idx-1];
	slot.state = State::Paused;

	slot.fake_state = State::Playing;
	mixer.cmd.emplace_back(sound_command { 
		.slot = slot_idx, .id = sound_cmd_id::play, .param0 = flags, .param1 = (u64)sound,
		.param2 = *(u64*)&delay, .param3 = (u64)obj, .string_storage = sound->_p->fn_attached[0]
	});

	auto params = sound->_p->get_params();
	Fvector distances = { params.min_distance, params.max_distance, params.max_ai_distance };

	if (sound->slot()) {
		pitch = (pitch ? pitch : &params.freq);
		distance = (distance ? distance : &distances);
		pos = (pos ? pos : &params.position);
		volume = (volume ? volume : &params.volume);
	}

	if (pitch) Mixer::UpdateParameter(slot_idx, ParameterId::Pitch, Fvector(*pitch));
	if (distance) Mixer::UpdateParameter(slot_idx, ParameterId::DistanceRange, *distance);
	if (pos) Mixer::UpdateParameter(slot_idx, ParameterId::Position, *pos);
	if (volume) Mixer::SetVolume(slot_idx, *volume);
}

void 
Mixer::Pause(u32 slot)
{
	if (slot == 0) {
		return;
	}

	mixer.slots[slot - 1].fake_state = State::Paused;
	mixer.cmd.emplace_back(sound_command{ .slot = slot, .id = sound_cmd_id::pause });
}

void
Mixer::Stop(u32 slot, bool deferred)
{
	if (slot == 0) {
		return;
	}

	if (!deferred) {
		mixer.slots[slot - 1].fake_state = State::Stopped;
	}
	mixer.cmd.emplace_back(sound_command{ .slot = slot, .id = sound_cmd_id::stop, .param0 = deferred });
}

void
Mixer::UpdateParameter(u32 slot, ParameterId parameter, Fvector value)
{
	if (slot == 0) {
		return;
	}

	double p0 = value.x, p1 = value.y, p2 = value.z;
	mixer.cmd.emplace_back(sound_command{.slot=slot,.id=sound_cmd_id::update_parameter,.param0=(u16)parameter,.param1=*(u64*)&p0,.param2=*(u64*)&p1,.param3=*(u64*)&p2});
}

void 
Mixer::SetVolume(u32 slot, double volume)
{
	if (slot == 0) {
		return;
	}

	if (std::abs(volume) > 10)
	{
		volume = 1.0;
	}

	auto out_val = mixer.cmd.emplace_back(sound_command{ .slot = slot,.id = sound_cmd_id::set_volume, .param1 = *(u64*)&volume });
}

xr_vector<sound_slot_state>&
Mixer::GetSlots()
{
	return mixer.slots;
}

xrSRWLock&
Mixer::GetUpdateMutex()
{
	return mixer.update_lock;
}

xrSRWLock&
Mixer::GetManageMutex()
{
	return mixer.manage_lock;
}

sound_stats
Mixer::GetStats()
{
	return mixer.stats;
}

float 
Mixer::GetPlaytime(u32 slot)
{
	if (slot == 0) {
		return 0.0f;
	}

	return (((float)mixer.slots[slot - 1].position) / (float)SND_SAMPLERATE);
}

float
Mixer::GetDuration(u32 slot)
{
	if (slot == 0) {
		return 0.0f;
	}

	if (!mixer.slots[slot - 1].sound_name.size() || !mixer.sources.contains(mixer.slots[slot - 1].sound_name.c_str())) {
		return 0.0f;
	}

	auto& source = mixer.sources.at(mixer.slots[slot - 1].sound_name.c_str());
	return (float)source.pub.frames_total / (float)SND_SAMPLERATE;
}

bool 
Mixer::SlotIsRelated(u32 slot)
{
	return Snd_SlotOcclusion(slot, 0.0f, nullptr);
}

u32
Mixer::GetGameType(u32 slot)
{
	if (slot == 0) {
		return 0.0f;
	}

	if (!mixer.slots[slot - 1].sound_name.size() || !mixer.sources.contains(mixer.slots[slot - 1].sound_name.c_str())) {
		return 0.0f;
	}

	auto& source = mixer.sources.at(mixer.slots[slot - 1].sound_name.c_str());
	return source.pub.game_type;
}

u32 
Mixer::GetFlags(u32 slot)
{
	if (slot == 0) {
		return 0;
	}

	return mixer.slots[slot - 1].flags;
}

Mixer::State
Mixer::GetState(u32 slot)
{
	if (slot == 0) {
		return State::Stopped;
	}

	return mixer.slots[slot - 1].fake_state;
}

u32
Mixer::GetSourceCount()
{
	return mixer.sources.size();
}

const sound_source_public*
Mixer::GetSource(u32 index)
{
	u32 counter = 0;
	for (const auto& [key, source] : mixer.sources) {
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

	return mixer.slots[slot - 1].parameters;
}

void 
Mixer::AddEditorZone(sound_zone_params& params)
{
	mixer.editor_zone = true;
	ResetZones();
	AddZone(params);
}

#ifndef DISABLE_RESONANCE_AUDIO
static void
Snd_EngineToResonanceParams(const sound_reverb_settings& s,
	vraudio::ReflectionProperties& out_ref,
	vraudio::ReverbProperties& out_rev)
{
	// zero-init
	out_ref = vraudio::ReflectionProperties();
	out_rev = vraudio::ReverbProperties();

	// Room geometry: use environment_size (fallback to 10m)
	float room_size = std::max(s.environment_size, 10.0f);
	out_ref.room_dimensions[0] = room_size;
	out_ref.room_dimensions[1] = room_size;
	out_ref.room_dimensions[2] = room_size;

	// Default room position/rotation (origin, identity)
	out_ref.room_position[0] = out_ref.room_position[1] = out_ref.room_position[2] = 0.0f;
	out_ref.room_rotation[0] = out_ref.room_rotation[1] = out_ref.room_rotation[2] = 0.0f;
	out_ref.room_rotation[3] = 1.0f;

	// Cutoff frequency derived from room_hf (interpreted as dB HF attenuation).
	// Map 0 dB -> ~20kHz, negative values reduce cutoff.
	float hf_db = s.room_hf;
	float cutoff = 20000.0f * powf(10.0f, hf_db / 20.0f);
	out_ref.cutoff_frequency = std::clamp(cutoff, 200.0f, 20000.0f);

	// Reflection coefficients: use environment_diffusion (0..1) as uniform coefficient
	float diffusion = std::clamp(s.environment_diffusion, 0.0f, 1.0f);
	for (int i = 0; i < 6; ++i) out_ref.coefficients[i] = diffusion;
	// Reflection gain from reflections parameter (kept in reasonable range)
	out_ref.gain = std::clamp(s.reflections, 0.0f, 3.16f);

	// Reverb (late) RT60 mapping:
	// - base RT60 from decay_time
	// - apply decay_hf_ratio on high-frequency bands
	// - reduce HF RT60 by air_absorption_hf
	float base_rt60 = std::clamp(s.decay_time, 0.05f, 60.0f);
	float hf_ratio = std::clamp(s.decay_hf_ratio, 0.1f, 4.0f);
	// Normalize air absorption to [0..1] (engine units unknown — clamp defensively)
	float air_abs = std::clamp(s.air_absorption_hf, 0.0f, 100.0f) / 100.0f;

	for (int i = 0; i < 9; ++i) {
		// bands 0..5 -> low/mid, 6..8 -> high
		if (i >= 6)
			out_rev.rt60_values[i] = base_rt60 * hf_ratio * (1.0f - 0.5f * air_abs);
		else
			out_rev.rt60_values[i] = base_rt60 * (1.0f - 0.25f * air_abs);

		out_rev.rt60_values[i] = std::clamp(out_rev.rt60_values[i], 0.01f, 120.0f);
	}

	out_rev.gain = 1.0f;
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
		R_ASSERT(iplReflectionEffectCreate(mixer.ipl_context, &settings, &reflect_settings, &params.state.effect[ch]) == IPL_STATUS_SUCCESS);
	}
#endif

#ifndef DISABLE_RESONANCE_AUDIO
	vraudio::ReflectionProperties reflection_properties = { };
	vraudio::ReverbProperties reverb_properties = {};

	Snd_EngineToResonanceParams(params.settings, reflection_properties, reverb_properties);
	params.state.ra_context = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	params.state.ra_context->EnableRoomEffects(true);
	params.state.ra_context->SetReverbProperties(reverb_properties);
	params.state.ra_context->SetReflectionProperties(reflection_properties);
	params.state.buffer = params.state.ra_context->CreateSoundObjectSource(vraudio::RenderingMode::kStereoPanning);
#endif

	mixer.zones.emplace_back(std::move(params));
}

void 
Mixer::ResetZones()
{
	xrSRWLockGuard guard(mixer.render_lock);

	for (auto& zone : mixer.zones) {
#ifndef DISABLE_STEAM_AUDIO
		for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
			if (zone.state.effect[ch] != nullptr) {
				iplReflectionEffectRelease(&zone.effect[ch]);
			}
		}
#endif

#ifndef DISABLE_RESONANCE_AUDIO
		delete zone.state.ra_context;
#endif
	}

	mixer.zones.clear();
}

const xr_vector<sound_zone_params>& 
Mixer::GetZones()
{
	return mixer.zones;
}

ref_sound::ref_sound()
{
	xrSRWLockGuard g1(mixer.manage_lock);

	if (!mixer.sounds.contains(this)) {
		mixer.sounds.emplace(this);
	}
}

ref_sound::~ref_sound()
{
	xrSRWLockGuard g1(mixer.manage_lock);

	if (mixer.sounds.contains(this)) {
		mixer.sounds.erase(this);
	}
}