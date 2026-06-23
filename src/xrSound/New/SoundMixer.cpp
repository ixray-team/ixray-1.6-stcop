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
#include "ReverbInterface.h"
#include "ogg_utils.h"

#include "../Sound.h"
#include "../SoundRender.h"
#include "../ai_sounds.h"
#include "SoundConvolution.h"

#include <pffft.h>

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#include "ReverbInterface.h"
#include "SoundSpatializer.h"

#ifndef DISABLE_STEAM_AUDIO
#include "../Plugins/SteamAudio.h"
#endif
#ifndef DISABLE_RESONANCE_AUDIO
#include "../Plugins/ResonanceAudio.h"
#endif

IReverInterface* GReverInterface = nullptr;
ISoundSpatializer* GSpatializer = nullptr;

#define DEFAULT_SLOT_COUNT (512)
#define SND_MAX_PITCH (4)
#define SND_MAX_VELOCITY (100.0f)
#define CACHE_LINES_COUNT (1024)
#define CACHE_LINE_WIDTH (12)
#define CACHE_LINE_ENTRY_COUNT (32)
#define CACHE_LINE_MAX_TIME_NS (1000000000)

using namespace XRay::Sound;
enum class ESoundMixerCommands : u16
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

struct SoundCommand
{
	u32 slot;
	ESoundMixerCommands id;
	u16 param0;
	u64 param1;
	u64 param2;
	u64 param3;
	shared_str string_storage;
};

// Asynchronous decode request: decode the cache line covering `pos` for `name`
// on the dedicated decode thread instead of blocking the audio thread.
struct sound_decode_request
{
	xr_string name;
	u32 pos;
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
	float data[SND_CHANNEL_COUNT][(SND_BLOCKSIZE + 1) * CACHE_LINE_WIDTH];
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

	xrCriticalSection DecodeLock;
	xr_vector<sound_decode_request> DecodeQueue;
	bool DecodeStop = false;
	ThreadID DecodeThread = 0;

	sound_stats stats = {0};
	float dt;
	float time_factor = 1.0f;
	float master_volume = 0.0f;
	float effect_volume = 0.0f;
	float music_volume = 0.0f;
	float shooting_volume = 0.0f;
	float compression = 0.0f;
	float compressor_envelope[SND_CHANNEL_COUNT] = {FLT_EPSILON, FLT_EPSILON};
	Fvector P, D, N;
	Fvector listener_velocity;
	Fvector occ;
	Fmatrix m_V;

	xr_vector<u32> free_slots;
	xr_vector<u32> free_cachelines;
	xr_vector<SoundCommand> cmd;
	xr_vector<sound_slot_state> slots;
	xr_vector<sound_cache_line> cache_lines;
	xr_hash_set<ref_sound*> sounds;
	xr_hash_map<xr_string, sound_source> snd_sources;
	xr_vector<sound_zone_params> zones;

	// HRTF slot management (index pool; backend state lives in the spatializer plugin)
	xr_vector<u32> free_hrtf_slots;

	sound_bus_state buses[SND_BUS_COUNT];

	CConvolutionReverb ShootingReverbFar;
	CConvolutionReverb ShootingReverbIndoor;
	float* ShootingSendFar[SND_CHANNEL_COUNT] = {nullptr};
	float* ShootingSendIndoor[SND_CHANNEL_COUNT] = {nullptr};

	bool IsOutdoorSend = false;
	bool IsIndoorSend = false;

	float IndoorFactor = 0.0f;

	bool editor_zone = false;
	bool hrtf_enabled;

#ifdef DEBUG_DRAW
	PFFFT_Setup* fft_setup;
	float* aligned_input_fft;
	float* aligned_output_fft;
	float fft_window[SND_BLOCKSIZE];
#endif

	float read_buffer[SND_CHANNEL_COUNT][(SND_BLOCKSIZE + 1) * 10];
};

static sound_mixer_state GMixer = {};

static void Snd_GrowCacheLines(bool IsLockRender)
{
	if (IsLockRender)
	{
		GMixer.render_lock.AcquireExclusive();
		GMixer.manage_lock.AcquireExclusive();
	}

	xrSRWLockGuard Guard0(GMixer.update_lock, false);
	bool IsLocked = !GMixer.source_lock.TryAcquireExclusive();

	size_t OldCacheLines = GMixer.cache_lines.size();
	size_t NewCacheLines = std::max((size_t)CACHE_LINES_COUNT, GMixer.cache_lines.size() * 2);

	GMixer.cache_lines.resize(NewCacheLines);
	GMixer.free_cachelines.reserve(NewCacheLines);

	for (size_t Iter = OldCacheLines; Iter < NewCacheLines; Iter++)
	{
		GMixer.free_cachelines.push_back(Iter + 1);
	}

	if (IsLockRender)
	{
		GMixer.render_lock.ReleaseExclusive();
		GMixer.manage_lock.ReleaseExclusive();
	}

	if (!IsLocked)
	{
		GMixer.source_lock.ReleaseExclusive();
	}

	GMixer.stats.cache_lines_total = NewCacheLines;
}

static void Snd_GrowSlots(bool IsLockUpdate)
{
	xrSRWLockGuard Guard0(GMixer.render_lock, false);
	xrSRWLockGuard Guard1(GMixer.manage_lock, false);
	bool Locked = !GMixer.source_lock.TryAcquireExclusive();

	if (IsLockUpdate)
	{
		GMixer.update_lock.AcquireExclusive();
	}

	size_t OldSize = GMixer.slots.size();
	size_t NewSize = std::max((size_t)DEFAULT_SLOT_COUNT, GMixer.slots.size() * 2);

	GMixer.slots.resize(NewSize);
	GMixer.free_slots.reserve(NewSize);

	GMixer.stats.possible_free_count += (NewSize - OldSize);
	for (size_t Iter = OldSize; Iter < NewSize; Iter++)
	{
		GMixer.free_slots.push_back(Iter + 1);
	}

	if (IsLockUpdate)
	{
		GMixer.update_lock.ReleaseExclusive();
	}

	if (!Locked)
	{
		GMixer.source_lock.ReleaseExclusive();
	}
}

ICF void Snd_AcquireHRTFSlot(u32 slot_idx)
{
	PROF_EVENT("Sound: AcquireHRTFSlot");
	if (!GMixer.hrtf_enabled || !psSoundFlags.is(ss_HRTF))
	{
		return;
	}

	auto& Slot = GMixer.slots[slot_idx - 1];
	if ((Slot.flags & (u16)Mixer::Flags::Spatial) == 0)
	{
		return;
	}

	if (Slot.hrtf_slot)
	{
		return;
	}

	if (GMixer.free_hrtf_slots.empty())
	{
		return;
	}

	Slot.hrtf_slot = GMixer.free_hrtf_slots[GMixer.free_hrtf_slots.size() - 1];
	GMixer.free_hrtf_slots.pop_back();

	if (GSpatializer)
	{
		GSpatializer->ResetSlot(Slot.hrtf_slot - 1);
	}
}

ICF void Snd_ReleaseHRTFSlot(u32 SlotIdx)
{
	if (!GMixer.hrtf_enabled || !psSoundFlags.is(ss_HRTF))
	{
		return;
	}

	auto& Slot = GMixer.slots[SlotIdx - 1];
	if ((Slot.flags & (u16)Mixer::Flags::Spatial) == 0)
	{
		return;
	}

	if (Slot.hrtf_slot)
	{
		if (GSpatializer)
		{
			GSpatializer->FreeSlot(Slot.hrtf_slot - 1);
		}
		GMixer.free_hrtf_slots.emplace_back(Slot.hrtf_slot);
		Slot.hrtf_slot = 0;
	}
}

void MixerNewState(u32 Slot, Mixer::State State)
{
	if (Slot == 0)
	{
		return;
	}

	GMixer.slots[Slot - 1].prev_state = GMixer.slots[Slot - 1].state;
	GMixer.slots[Slot - 1].state = State;
	GMixer.slots[Slot - 1].fake_state = State;
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

	if (purge_from_entry && line.name.size())
	{
		auto found_source = GMixer.snd_sources.find(line.name.c_str());
		if (found_source != GMixer.snd_sources.end())
		{
			for (u32& entry_cache_idx : found_source->second.cache_lines)
			{
				if (entry_cache_idx == cache_idx)
				{
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
	if (source.pub.ref_count == 0)
	{
		for (u32& cache_idx : source.cache_lines)
		{
			if (cache_idx != 0 && GMixer.cache_lines[cache_idx - 1].name.size())
			{
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
	if (ov_pcm_tell(&source.file) != position)
	{
		if (precise)
		{
			ov_pcm_seek(&source.file, position);
		}
		else
		{
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
	if (strext(N))
	{
		*strext(N) = 0;
	}
	source.pub.name = N;

	xr_strconcat(fn, N, ".ogg");
	if (!FS.exist("$level$", fn))
	{
		FS.update_path(fn, _game_sounds_, fn);
	}
	if (!FS.exist(fn))
	{
		FS.update_path(fn, _game_sounds_, "$no_sound.ogg");
		Msg("! Can't find sound '%s'", source.pub.name.c_str());
	}

	source.pub.path = fn;
	IReader* m_wavefile = FS.r_open(source.pub.path.c_str());
	R_ASSERT3(m_wavefile && m_wavefile->length(), "Can't open wave file:", source.pub.path.c_str());
	if (source.data != nullptr)
	{
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
	if (ovm->comments)
	{
		IReader F(ovm->user_comments[0], ovm->comment_lengths[0]);
		u32 vers = F.r_u32();
		if (vers == 0x0001)
		{
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = 1.0f;
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = 300.0f;
		}
		else if (vers == 0x0002)
		{
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = F.r_float();
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = 300.0f;
		}
		else if (vers == OGG_COMMENT_VERSION)
		{
			source.pub.min_distance = F.r_float();
			source.pub.max_distance = F.r_float();
			source.pub.volume = F.r_float();
			source.pub.game_type = F.r_u32();
			source.pub.max_ai_distance = F.r_float();
		}
		else
		{
			Msg("! Invalid ogg-comment version, file: %s", source.pub.name.c_str());
		}
	}
	else
	{
		Msg("~ Missing ogg-comment, file: %s", source.pub.name.c_str());
	}

	source.pub.volume = std::min(source.pub.volume, 1.0f);
}

ICF sound_source* Snd_FindSound(const xr_string& name)
{
	if (name.empty())
	{
		return nullptr;
	}

	xrSRWLockGuard guard(GMixer.source_lock, true);
	auto found_source = GMixer.snd_sources.find(name);
	if (found_source == GMixer.snd_sources.end())
	{
		return nullptr;
	}

	found_source->second.pub.ref_count++;
	return &found_source->second;
}

ICF sound_source* Snd_AcquireSound(const xr_string& name, bool fail_if_not_found)
{
	sound_source* source = Snd_FindSound(name);
	if (source != nullptr || name.empty())
	{
		return source;
	}

	R_ASSERT(!fail_if_not_found);

	// TODO: async file load?
	xrSRWLockGuard guard(GMixer.source_lock);
	source = &GMixer.snd_sources[name];
	if (source->reader == nullptr)
	{
		Snd_LoadSource(*source, name.c_str());
	}

	source->pub.ref_count++;
	return source;
}

ICF void Snd_ReleaseSound(const xr_string& name)
{
	if (name.empty())
	{
		return;
	}

	{
		xrSRWLockGuard guard(GMixer.source_lock, true);
		auto found_source = GMixer.snd_sources.find(name);
		if (found_source == GMixer.snd_sources.end())
		{
			return;
		}

		R_ASSERT(found_source->second.pub.ref_count);
		if (--found_source->second.pub.ref_count != 0)
		{
			return;
		}
	}

	xrSRWLockGuard guard(GMixer.source_lock);
	auto found_source = GMixer.snd_sources.find(name);
	if (found_source == GMixer.snd_sources.end() || found_source->second.pub.ref_count != 0)
	{
		return;
	}

	auto& source = found_source->second;
	ov_clear(&source.file);
	xr_delete(source.reader);
	xr_free(source.data);

	Snd_DestroySourceCache(source);
	GMixer.snd_sources.erase(found_source);
}

ICF void Snd_UpdateCache(sound_source& Source, u32 Position);

ICF void Snd_QueueDecode(const xr_string& Name, u32 Position)
{
	if (Name.empty())
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.DecodeLock);
	for (const auto& Request : GMixer.DecodeQueue)
	{
		if (Request.name == Name && Request.pos == Position)
		{
			return; // already queued
		}
	}
	GMixer.DecodeQueue.push_back({Name, Position});
}

static void Snd_DecodeThreadProc(void*)
{
	PROF_THREAD("Sound Decode Thread");

	while (!GMixer.DecodeStop)
	{
		sound_decode_request Request;
		bool IsRequested = false;
		{
			xrCriticalSectionGuard Guard(GMixer.DecodeLock);
			if (!GMixer.DecodeQueue.empty())
			{
				Request = GMixer.DecodeQueue.front();
				GMixer.DecodeQueue.erase(GMixer.DecodeQueue.begin());
				IsRequested = true;
			}
		}

		if (!IsRequested)
		{
			std::this_thread::yield();
			continue;
		}

		PROF_EVENT("Decode OGG");
		sound_source* Source = Snd_FindSound(Request.name);
		if (Source != nullptr)
		{
			Snd_UpdateCache(*Source, Request.pos);
			Snd_ReleaseSound(Request.name);
		}
	}
}

ICF u32 Snd_ReadFromSource(sound_source& source, float** buffer, u32 frames)
{
	PROF_EVENT("Sound: Decode Vorbis");

	if (source.file.datasource == nullptr)
	{
		return 0;
	}

	float** pcm;
	int section;
	u32 last_frames = frames;
	u32 offset = 0;
	do
	{
		int status = ov_read_float(&source.file, &pcm, last_frames, &section);
		if (status == 0)
		{
			break;
		}
		else
		{
			R_ASSERT2(status >= 0, "Decoding error");
			last_frames -= status;
		}

		for (size_t Channel = 0; Channel < std::min((u8)SND_CHANNEL_COUNT, source.pub.channels_count); Channel++)
		{
			for (size_t idx = 0; idx < status; idx++)
			{
				buffer[Channel][offset + idx] = std::clamp(pcm[Channel][idx], -1.0f, 1.0f);
			}
		}

		offset += status;
	} while (last_frames);

	if (source.pub.channels_count == 1)
	{
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
	PROF_EVENT("Sound: SlotOcclusion");
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
	PROF_EVENT("Sound: FindAvailableCacheLine");
	u32 needed_frames = std::min((u32)SND_BLOCKSIZE, source.pub.frames_total - position);
	u32 found_cache_idx = 0;
	for (const u32& cache_idx : source.cache_lines)
	{
		if (cache_idx != 0)
		{
			auto& line = GMixer.cache_lines[cache_idx - 1];
			if (in_range(position, line.start, line.end) && in_range(position + needed_frames, line.start, line.end))
			{
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
		if (found_cache_idx == 0 && source.file.datasource != nullptr)
		{
			GMixer.stats.cache_miss_count++;
			found_cache_idx = Snd_NewCacheLine();
			auto& line = GMixer.cache_lines[found_cache_idx - 1];

			u32 cache_size = ((SND_BLOCKSIZE + 1) * CACHE_LINE_WIDTH);

			{
				// TODO: parallel decoding for each slot
				u32 begin_pos = Snd_SeekSource(source, position, false);
				u32 end_pos = begin_pos + cache_size;
				if (end_pos < (position + SND_BLOCKSIZE))
				{
					begin_pos = Snd_SeekSource(source, position, true);
				}

				memset(line.data, 0, sizeof(line.data));

				float* ch_data[SND_CHANNEL_COUNT];
				for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
				{
					ch_data[i] = line.data[i];
				}
				end_pos = begin_pos + Snd_ReadFromSource(source, ch_data, cache_size);

				// VERIFY(in_range(slot.position, begin_pos, end_pos));
				line.name = source.pub.name;
				line.start = begin_pos;
				line.end = end_pos;
			}

			bool inserted = false;
			for (u32& cache_idx : source.cache_lines)
			{
				if (cache_idx == found_cache_idx)
				{
					inserted = true;
					break;
				}

				if (cache_idx == 0)
				{
					cache_idx = found_cache_idx;
					inserted = true;
					break;
				}
			}

			if (!inserted)
			{
				u64 least_timestamp = (u64)-1;
				u32 cache_entry_idx = 0;
				for (size_t i = 0; i < CACHE_LINE_ENTRY_COUNT; i++)
				{
					if (source.cache_lines[i] == 0 || source.cache_lines[i] == found_cache_idx)
					{
						continue;
					}

					if (GMixer.cache_lines[source.cache_lines[i] - 1].timestamp < least_timestamp)
					{
						least_timestamp = GMixer.cache_lines[source.cache_lines[i] - 1].timestamp;
						cache_entry_idx = i + 1;
						continue;
					}
				}

				if (cache_entry_idx != 0)
				{
					Snd_PurgeCacheLine(source.cache_lines[cache_entry_idx - 1], false);
					source.cache_lines[cache_entry_idx - 1] = found_cache_idx;
				}
				else
				{
					Snd_GrowCacheLines(false);
				}
			}
		}
	}
}

ICF u32 Snd_ReadSlotData(u32 SlotIdx, sound_source& Source, float** Data, u32 FramesCount)
{
	auto& Slot = GMixer.slots[SlotIdx - 1];

	u32 ReadPostion = Slot.position;
	u32 Frames2Read = FramesCount;

	while (Frames2Read && ReadPostion < Source.pub.frames_total)
	{
		u32 FoundCacheIndex = Snd_FindAvailableCacheLine(Source, ReadPostion);

		if (FoundCacheIndex == 0)
		{
			PROF_EVENT("Decode OGG Wait");
			GMixer.stats.render_cache_miss++;

			Snd_QueueDecode(Slot.sound_name, ReadPostion);
			while (FoundCacheIndex == 0)
			{
				FoundCacheIndex = Snd_FindAvailableCacheLine(Source, ReadPostion);
				std::this_thread::yield();
			}
		}

		auto& CacheLine = GMixer.cache_lines[FoundCacheIndex - 1];
		u32 BeginOffset = ReadPostion - CacheLine.start;
		u32 CacheFrames = std::min(Frames2Read, CacheLine.end - ReadPostion);
		if (CacheFrames == 0)
		{
			break;
		}

		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			memcpy(&Data[Channel][FramesCount - Frames2Read], &CacheLine.data[Channel][BeginOffset], CacheFrames * sizeof(float));
		}

		Frames2Read -= CacheFrames;
		ReadPostion += CacheFrames;
	}

	return FramesCount - Frames2Read;
}

ICF void Snd_ReadSlot(u32 slot_idx, sound_source& source, float** data, u32 frames_count)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	if (source.pub.frames_total == 0)
	{
		MixerNewState(slot_idx, Mixer::State::Stopped);
		return;
	}

	u32 last_frames = frames_count;
	while (last_frames)
	{
		float* offfseted_data[SND_CHANNEL_COUNT];
		u32 buf_offset = (frames_count - last_frames);
		for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
		{
			offfseted_data[i] = &data[i][buf_offset];
		}

		u32 read_frames = Snd_ReadSlotData(slot_idx, source, offfseted_data, last_frames);

		last_frames -= read_frames;
		slot.position = std::min(slot.position + read_frames, source.pub.frames_total);

		if (slot.position < source.pub.frames_total)
		{
			if (read_frames == 0)
			{
				MixerNewState(slot_idx, Mixer::State::Stopped);
				break;
			}

			continue;
		}

		slot.position = 0;
		if ((slot.flags & (u32)Mixer::Flags::Looped) == 0)
		{
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
	float ratio = std::clamp(pitch * slot.doppler * GMixer.time_factor, 0.0f, (float)SND_MAX_PITCH);
	u32 input_frames = std::max((u32)((float)output_frames * ratio), 1u);

	bool is_music = (slot.flags & (u16)Mixer::Flags::Intro);

	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memset(GMixer.read_buffer[Channel], 0, (input_frames + 1) * sizeof(float));
	}

	if (is_music || fis_zero(1.0f - ratio))
	{
		Snd_ReadSlot(slot_idx, source, data, SND_BLOCKSIZE);
	}
	else
	{
		float* offfseted_data[SND_CHANNEL_COUNT];
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			offfseted_data[Channel] = GMixer.read_buffer[Channel];
		}

		Snd_ReadSlot(slot_idx, source, offfseted_data, input_frames + 1);
		if (slot.position > 0)
		{
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

	{
		xrSRWLockGuard g1(GMixer.manage_lock);
		float dt = (float)((double)(Snd_GetTimestamp() - timestamp) / 1000000000.0);

		GMixer.stats.frame_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
		timestamp = Snd_GetTimestamp();

		if (counter % 100 == 0)
		{
			GMixer.stats.cache_hit_count = 0;
			GMixer.stats.cache_miss_count = 0;
		}

		for (size_t i = 0; i < GMixer.slots.size(); i++)
		{
			PROF_EVENT("Sound: GMixer.slot");
			auto& slot = GMixer.slots[i];

			sound_source* source = Snd_FindSound(slot.sound_name);
			if (source != nullptr && Snd_SlotOcclusion(i + 1, *source, dt, nullptr) != ESlotOcclusionResult::False)
			{
				Snd_AcquireHRTFSlot(i + 1);
				// Hand the decode off to the decode thread; only enqueue if the cache
				// line for the current position isn't already filled.
				if (Snd_FindAvailableCacheLine(*source, slot.position) == 0)
				{
					PROF_EVENT("Sound: QueueDecode");
					Snd_QueueDecode(slot.sound_name, slot.position);
				}
			}
			else
			{
				Snd_ReleaseHRTFSlot(i + 1);
			}

			if (source != nullptr)
			{
				PROF_EVENT("Sound: ReleaseSound");
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
}

ICF Fvector Snd_Velocity(const Fvector& From, const Fvector& To)
{
	Fvector Out;
	Out.set(0.0f, 0.0f, 0.0f);

	if (GMixer.dt > EPS_S)
	{
		Out.sub(To, From).mul(1.0f / GMixer.dt);
	}

	if (Out.square_magnitude() > SND_MAX_VELOCITY * SND_MAX_VELOCITY)
	{
		Out.set(0.0f, 0.0f, 0.0f);
	}

	return Out;
}

ICF void Snd_PhononSpatialProcess(float** Data, u32 slot_idx)
{
	auto& Slot = GMixer.slots[slot_idx - 1];
	if ((Slot.flags & (u32)Mixer::Flags::Spatial) == 0)
	{
		return;
	}

	Fvector& Pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
	Fvector& Distances = Slot.parameters[(u32)Mixer::ParameterId::DistanceRange];

	dsp_stuff Stuff =
		{
			.Dt = GMixer.dt,
			.Panning = Slot.panning,
			.CameraPosition = &GMixer.P,
			.CameraDirection = &GMixer.D,
			.CameraNormal = &GMixer.N,
			.CameraVelocity = &GMixer.listener_velocity,
			.ObjPosition = &Pos,
			.ObjVelocity = &Slot.velocity,
			.Doppler = &Slot.doppler
		};

	if (Slot.hrtf_slot == 0)
	{
		DSP_SpatialProcess(Data, Slot.parameters[(u32)Mixer::ParameterId::DistanceRange], Stuff, false /*slot.flags& (u32)Mixer::Flags::NoOCC */);
		return;
	}

	float Distance;
	Fvector RelativePos;
	DSP_CalculateRelativePosition(Stuff, RelativePos, Distance);
	DSP_Doppler(Stuff, Distance);
	Distance = std::max(Distance, 0.1f);

	if (GSpatializer)
	{
		GSpatializer->ProcessHrtf(Slot.hrtf_slot - 1, Data, Pos, GMixer.P, RelativePos);
	}

	// Attenuation
	float MinDistance = std::max(Distances.x, EPS_S);
	float MaxDistance = std::max(Distances.y, MinDistance + EPS_S);

	Distance = std::clamp(Distance, MinDistance, MaxDistance);
	float Attent = MinDistance / (psSoundRolloff * Distance);
	Attent *= Attent;
	Attent *= 1.0f - std::clamp(std::max(Distance - MinDistance, 0.0f) / (MaxDistance - MinDistance), 0.0f, 1.0f);
	Attent = std::clamp(Attent, 0.f, 1.f);
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (size_t Key = 0; Key < SND_BLOCKSIZE; Key++)
		{
			Data[Channel][Key] *= Attent;
		}
	}
}

static float Snd_HemiIndoorFactor(const Fvector& Pos)
{
	PROF_EVENT("Sound: Indoor Hemi");

	CDB::MODEL* EnvModel = ::Sound->get_geometry_env();
	CDB::COLLIDER* Collider = ::Sound->get_geometry_db();
	if (EnvModel == nullptr || Collider == nullptr)
	{
		return 0.0f;
	}

	constexpr u32 kRayCount = 12;
	constexpr float kGoldenAngle = 2.39996322972865332f;
	constexpr float kMaxRange = 1000.0f;

	float blocked = 0.0f;
	for (u32 i = 0; i < kRayCount; i++)
	{
		const float h = (float)(i + 1) / (float)(kRayCount + 1);
		const float r = std::sqrt(std::max(1.0f - h * h, 0.0f));
		const float a = (float)i * kGoldenAngle;

		Fvector dir = {std::cos(a) * r, h, std::sin(a) * r};
		Collider->ray_options(CDB::OPT_ONLYNEAREST);
		Collider->ray_query(EnvModel, Pos, dir, kMaxRange);
		if (Collider->r_count())
		{
			blocked += 1.0f;
		}
	}

	blocked /= (float)kRayCount;

	// Smoothstep over [0.1, 0.7]: partial cover (trees, wire fences) should not
	// flip the sound to the indoor reverb.
	const float t = std::clamp((blocked - 0.1f) / 0.6f, 0.0f, 1.0f);
	return t * t * (3.0f - 2.0f * t);
}

// Updates the per-slot indoor factor from the SOUND's own position (not the listener's) and smooths it to avoid abrupt IR switches.
static void Snd_UpdateSlotIndoorFactor(sound_slot_state& Slot, const Fvector& Pos)
{
	const float target = Snd_HemiIndoorFactor(Pos);
	const float k = std::clamp(GMixer.dt * 3.0f, 0.0f, 1.0f);
	Slot.IndoorFactor += (target - Slot.IndoorFactor) * k;
}

ICF void Snd_RenderSlot(u32 SlotIdx, sound_source& Source, float** process_buffer, float dt)
{
	auto& Slot = GMixer.slots[SlotIdx - 1];

	float OCCVolume = 1.0f;
	ESlotOcclusionResult OCCResult = Snd_SlotOcclusion(SlotIdx, Source, dt, &OCCVolume);
	if (OCCResult == ESlotOcclusionResult::False)
	{
		// TODO: hack for simulated sounds
		Slot.position = std::min(Slot.position + SND_BLOCKSIZE, Source.pub.frames_total);
		if (Slot.position == Source.pub.frames_total && (Slot.flags & (u16)Mixer::Flags::Looped) == 0)
		{
			MixerNewState(SlotIdx, Mixer::State::Stopped);
		}

		return;
	}

	// Clear process buffer and read data from source
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memset(process_buffer[Channel], 0, SND_BLOCKSIZE * sizeof(float));
	}

	Snd_ProcessSlot(SlotIdx, Source, process_buffer);

	Fvector& Pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
	Fvector& Volume = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel];
	float BeginFactor = 1.0f, EndFactor = 1.0f;

	// Deferred stopping
	bool IsMusic = (Slot.flags & (u16)Mixer::Flags::Intro);
	if ((Slot.flags & (u16)Mixer::Flags::NoOCC) == 0 || OCCResult != ESlotOcclusionResult::SOM)
	{
		OCCVolume = 1.f;
	}

	Slot.fade_volume = 1.f;

	if (Slot.stopping_position != (u32)-1)
	{
		u32 stopping_total = Source.pub.frames_total - Slot.stopping_position;
		if (stopping_total > 1 && Slot.position >= Slot.stopping_position)
		{
			u32 begin_offset = Slot.position - Slot.stopping_position;
			u32 write_count = IsMusic ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * GMixer.time_factor);
			u32 end_offset = std::min(begin_offset + write_count, Source.pub.frames_total - 1);
			BeginFactor = 1.0f - ((float)begin_offset / (float)(stopping_total - 1));
			EndFactor = 1.0f - ((float)end_offset / (float)(stopping_total - 1));
			BeginFactor = std::clamp(BeginFactor, 0.0f, 1.0f);
			EndFactor = std::clamp(EndFactor, 0.0f, 1.0f);
		}
	}

	// Apply final volumes
	float slot_volume = Volume.x * Volume.y * Volume.z;

	float VolumeMixer = GMixer.effect_volume;
	if (Slot.flags & (u16)Mixer::Flags::Music)
	{
		VolumeMixer = GMixer.music_volume;
	}
	else if (Slot.flags & (u16)Mixer::Flags::Shooting)
	{
		VolumeMixer = GMixer.shooting_volume;
	}

	float VolumeFinal = OCCVolume * slot_volume * VolumeMixer * Slot.fade_volume;
	BeginFactor *= VolumeFinal;
	EndFactor *= VolumeFinal;

	float left_panning = Slot.parameters[(u32)Mixer::ParameterId::Panning].x;
	float right_panning = Slot.parameters[(u32)Mixer::ParameterId::Panning].y;

	// Convolution reverb send. The indoor set uses its own near IR; the
	// outdoor set uses ONLY the FAR IR (the outdoor near field is removed).
	// The send follows the dry signal's own distance attenuation (matching
	// DSP_SpatialProcess), so the tail can never be louder than the gunshot
	// and fades to zero at extreme range.
	constexpr float IndoorNearEndDistance = 5.0f; // indoor near IR owns [0, 5] m
	constexpr float OutdoorGateDistance = 10.0f; // outdoor far IR gate opens at 10 m
	constexpr float NearWetScale = 0.5f; // near tail is too loud: -6 dB on its wet send
	bool ReverbSendActive = false;
	float Attent = 0.0f;
	float WetFar = 0.0f;
	float WetIndoorNear = 0.0f;

	if ((Slot.flags & (u16)Mixer::Flags::Shooting))
	{
		Fvector& ReverbPos = Slot.parameters[(u32)Mixer::ParameterId::Position];
		Fvector& ReverbDist = Slot.parameters[(u32)Mixer::ParameterId::DistanceRange];

		Fvector ReverbDelta;
		ReverbDelta.set(ReverbPos.x - GMixer.P.x, ReverbPos.y - GMixer.P.y, ReverbPos.z - GMixer.P.z);
		float ReverbDistance = ReverbDelta.magnitude();
		float MinD = std::max(ReverbDist.x, EPS_S);
		float MaxD = std::max(ReverbDist.y, MinD + EPS_S);
		float D = std::clamp(ReverbDistance, MinD, MaxD);

		// Wet level: dry signal distance attenuation. MUST match
		// DSP_SpatialProcess (power 1.3, NOT squared) so the wet send sits at the same level as the dry path; a squared falloff here makes the tail inaudible.
		float Base = MinD / (psSoundRolloff * D);
		Attent = std::pow(Base, 1.3f);
		Attent *= 1.0f - std::clamp(std::max(D - MinD, 0.0f) / (MaxD - MinD), 0.0f, 1.0f);
		Attent = std::clamp(Attent, 0.0f, 1.0f);

		// Indoor near IR plays from 0 to 5 m (no indoor far IR).
		float AlphaIndoor = std::clamp(ReverbDistance / IndoorNearEndDistance, 0.0f, 1.0f);
		AlphaIndoor = AlphaIndoor * AlphaIndoor * (3.0f - 2.0f * AlphaIndoor);

		// Outdoor has a soft gate at OutdoorGateDistance (no room tail for point-blank shots in the open field).
		constexpr float WetGateWidth = 2.0f;
		float WetGateOutdoor = std::clamp((ReverbDistance - OutdoorGateDistance) / WetGateWidth, 0.0f, 1.0f);
		WetGateOutdoor = WetGateOutdoor * WetGateOutdoor * (3.0f - 2.0f * WetGateOutdoor);

		// Indoor factor of the SOUND's own position (hemi) splits the send
		// between the recorded (outdoor) far IR and the synthesized indoor IR.
		const float WetIndoor = std::clamp(Slot.IndoorFactor, 0.0f, 1.0f);
		WetFar = 1.0f - WetIndoor;
		WetIndoorNear = AlphaIndoor * WetIndoor;

		Attent *= WetGateOutdoor;
		ReverbSendActive = true;
	}

	if (ReverbSendActive)
	{
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			for (u32 i = 0; i < SND_BLOCKSIZE; i++)
			{
				float s = process_buffer[Channel][i] * VolumeFinal;
				if (WetFar > 0.0f)
				{
					GMixer.ShootingSendFar[Channel][i] += s * Attent * WetFar;
					GMixer.IsOutdoorSend = true;
				}
				if (WetIndoorNear > 0.0f)
				{
					GMixer.ShootingSendIndoor[Channel][i] += s * Attent * WetIndoorNear * NearWetScale;
					GMixer.IsIndoorSend = true;
				}
			}
		}
	}

	// Spatial processing
	if (!(Slot.flags & (u32)Mixer::Flags::Intro) && Source.pub.channels_count == 1)
	{
		PROF_EVENT("Slot Spatial");

		if (Slot.flags & (u32)Mixer::Flags::Spatial)
		{
			if (psSoundFlags.is(ss_HRTF) && GMixer.hrtf_enabled && !(Slot.flags & (u32)Mixer::Flags::Shooting))
			{
				Snd_PhononSpatialProcess(process_buffer, SlotIdx);
			}
			else
			{
				dsp_stuff Stuff =
					{
						.Dt = GMixer.dt,
						.Panning = Slot.panning,
						.CameraPosition = &GMixer.P,
						.CameraDirection = &GMixer.D,
						.CameraNormal = &GMixer.N,
						.CameraVelocity = &GMixer.listener_velocity,
						.ObjPosition = &Pos,
						.ObjVelocity = &Slot.velocity,
						.Doppler = &Slot.doppler
					};

				DSP_SpatialProcess(process_buffer, Slot.parameters[(u32)Mixer::ParameterId::DistanceRange], Stuff, false /* slot.flags& (u32)Mixer::Flags::NoOCC */);
			}
		}

		if (GMixer.editor_zone)
		{
			Slot.zone_idx = 1;
		}

		u32 ZoneIdx = Slot.zone_idx;
		if (ZoneIdx && ZoneIdx <= GMixer.zones.size())
		{
			sound_zone_params& zone = GMixer.zones[ZoneIdx - 1];
			zone.use_count++;
			zone.last_use_ms = Snd_Milliseconds();

			float* reverb_buffer[SND_CHANNEL_COUNT] = {};
			for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				reverb_buffer[Channel] = zone.data[Channel];
			}

			float ZoneFade = std::clamp(Slot.IndoorFactor, 0.0f, 1.0f);
			DSP_MixBufferPanning(reverb_buffer, process_buffer, BeginFactor * ZoneFade, EndFactor * ZoneFade, left_panning, right_panning, SND_BLOCKSIZE);
		}
	}

	// TODO(vertver): push data to buses instead of main
	int bus_idx = IsMusic ? SND_BUS_MUSIC : SND_BUS_EFFECTS;
	float* bus_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		bus_buffer[Channel] = GMixer.buses[bus_idx].data[Channel];
	}

	// Bus mixing
	DSP_MixBufferPanning(bus_buffer, process_buffer, BeginFactor, EndFactor, left_panning, right_panning, SND_BLOCKSIZE);
}

void Snd_MixerRenderCallback(float* buffer)
{
	PROF_EVENT("Sound: Render Stage");

	xrSRWLockGuard Guard(GMixer.render_lock, true);

	GMixer.stats.render_cache_miss = 0;

	static u64 TimeStamp = Snd_GetTimestamp();
	float dt = (float)((double)(Snd_GetTimestamp() - TimeStamp) / 1000000000.0);
	TimeStamp = Snd_GetTimestamp();

	memset(buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
	static float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};

	float* process_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		process_buffer[i] = _process_buffer[i];
	}

	for (size_t i = 0; i < SND_BUS_COUNT; i++)
	{
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			memset(GMixer.buses[i].data[Channel], 0, SND_BLOCKSIZE * sizeof(float));
		}
	}

	for (auto& Zone : GMixer.zones)
	{
		Zone.use_count = 0;
		memset(Zone.data, 0, sizeof(Zone.data));
	}

	for (size_t i = 0; i < GMixer.slots.size(); i++)
	{
		PROF_EVENT("Slot Render");
		if (GMixer.slots[i].state != Mixer::State::Playing)
		{
			continue;
		}

		sound_source* Source = Snd_FindSound(GMixer.slots[i].sound_name);
		if (Source == nullptr)
		{
			MixerNewState(i + 1, Mixer::State::Stopped);
			continue;
		}

		Snd_RenderSlot(i + 1, *Source, process_buffer, dt);
		Snd_ReleaseSound(GMixer.slots[i].sound_name);
	}

	for (size_t Iter = 0; Iter < SND_CHANNEL_COUNT; Iter++)
	{
		process_buffer[Iter] = _process_buffer[Iter];
	}

	// Reverb mixing
	if (psSoundFlags.is(ss_EFX))
	{
		for (auto& Zone : GMixer.zones)
		{
			if (Zone.use_count == 0 && (Zone.last_use_ms + 3000) < Snd_Milliseconds())
			{
				continue;
			}

			PROF_EVENT("Reverb rendering");
			float* reverb_buffer[SND_CHANNEL_COUNT] = {};
			float* bus_buffer[SND_CHANNEL_COUNT] = {};

			for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				reverb_buffer[Channel] = Zone.data[Channel];
				bus_buffer[Channel] = GMixer.buses[SND_BUS_REVERB].data[Channel];
			}

			if (GReverInterface)
			{
				GReverInterface->ProcessReverb(Zone, reverb_buffer, process_buffer, bus_buffer);
			}

			// Algorithmic (Resonance / Steam Audio) reverb is attenuated by
			// -6 dB (x0.5) relative to its configured level.
			float reverb_gain = std::clamp(Zone.settings.reverb, 0.0f, 1.0f) * 0.010f * 0.5f;
			DSP_MixBuffer(bus_buffer, process_buffer, reverb_gain, reverb_gain, SND_BLOCKSIZE);
		}
	}

	// Convolution reverb tail for shooting sounds (impulse-response based).
	// The outdoor far and indoor near IR sets are convolved separately; each is
	// skipped when its send was silent this block. Indoor-ness is a property of
	// the SOUND's position, so both sets can be active at once.
	if (GMixer.IsOutdoorSend || GMixer.IsIndoorSend)
	{
		float* bus_buffer[SND_CHANNEL_COUNT] = {};
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			bus_buffer[Channel] = GMixer.buses[SND_BUS_REVERB].data[Channel];
		}

		GMixer.ShootingReverbFar.SetWetGain(psSoundShootingReverb);
		GMixer.ShootingReverbIndoor.SetWetGain(psSoundShootingReverb);

		if (GMixer.IsOutdoorSend)
		{
			GMixer.ShootingReverbFar.Process(GMixer.ShootingSendFar, bus_buffer, SND_BLOCKSIZE);
		}

		if (GMixer.IsIndoorSend)
		{
			GMixer.ShootingReverbIndoor.Process(GMixer.ShootingSendIndoor, bus_buffer, SND_BLOCKSIZE);
		}

		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			memset(GMixer.ShootingSendFar[Channel], 0, SND_BLOCKSIZE * sizeof(float));
			memset(GMixer.ShootingSendIndoor[Channel], 0, SND_BLOCKSIZE * sizeof(float));
		}

		GMixer.IsOutdoorSend = false;
		GMixer.IsIndoorSend = false;
	}

	{
		PROF_EVENT("Sound Mixing");
		float* MasterBuffer[SND_CHANNEL_COUNT] = {};
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			MasterBuffer[Channel] = GMixer.buses[SND_BUS_MASTER].data[Channel];
		}

		// Master mixing
		for (size_t Iter = 0; Iter < SND_BUS_COUNT; Iter++)
		{
			float* BusBuffer[SND_CHANNEL_COUNT] = {};
			for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				BusBuffer[Channel] = GMixer.buses[Iter].data[Channel];
			}

			DSP_MixBuffer(MasterBuffer, BusBuffer, 1.0f, 1.0f, SND_BLOCKSIZE);
		}

		DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, MasterBuffer, GMixer.compression, SND_BLOCKSIZE, GMixer.compressor_envelope);

		// Clipping and master volume adjust
		for (size_t Iter = 0; Iter < SND_BLOCKSIZE; Iter++)
		{
			for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				float Sample = MasterBuffer[Channel][Iter];
				Sample = std::clamp(Sample, -1.0f, 1.0f) * GMixer.master_volume;
				buffer[Iter * SND_CHANNEL_COUNT + Channel] = Sample;
			}
		}
	}

#ifdef DEBUG_DRAW
	for (size_t Iter = 0; Iter < SND_BLOCKSIZE; Iter++)
	{
		float Sample = 0.0f;
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			Sample += buffer[Iter * SND_CHANNEL_COUNT + Channel];
		}

		Sample /= float(SND_CHANNEL_COUNT);
		GMixer.aligned_input_fft[Iter] = GMixer.fft_window[Iter] * Sample;
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

	float Volumes[SND_CHANNEL_COUNT] = {};
	for (size_t Block = 0; Block < SND_BLOCKSIZE; Block++)
	{
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			Volumes[Channel] = (Volumes[Channel] + fabs(buffer[Block * SND_CHANNEL_COUNT + Channel])) * 0.5f;
		}
	}

	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		Volumes[i] = lin2dB(Volumes[i]);
	}

	memcpy(GMixer.stats.channel_volumes, Volumes, sizeof(Volumes));
#endif

	GMixer.stats.render_time_micros = (Snd_GetTimestamp() - TimeStamp) / 1000;
}

void Mixer::Initialize()
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

	if (GSpatializer)
	{
		GSpatializer->Initialize();
		GMixer.hrtf_enabled = true;
	}

	GMixer.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++)
	{
		GMixer.free_hrtf_slots[i] = i + 1;
	}

	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		GMixer.ShootingSendFar[Channel] = new float[SND_BLOCKSIZE]();
		GMixer.ShootingSendIndoor[Channel] = new float[SND_BLOCKSIZE]();
	}

	GMixer.ShootingReverbFar.Initialize("ir\\ir_default_far.ogg", psSoundShootingReverb, ReverbField::Far);
	GMixer.ShootingReverbIndoor.InitializeProcedural(psSoundShootingReverb, ReverbField::IndoorNear);

#ifdef DEBUG_DRAW
#pragma todo(replace with aligned allocators)
	// Blackman-Harris window
	for (int i = 0; i < SND_BLOCKSIZE; ++i)
	{
		GMixer.fft_window[i] = .5 * (1. - cosf(2. * 3.1415926535897932384 * (f64)i / (f64)(SND_BLOCKSIZE - 1)));
	}

	GMixer.aligned_input_fft = (float*)aligned_alloc(16, SND_BLOCKSIZE * sizeof(float));
	GMixer.aligned_output_fft = (float*)aligned_alloc(16, SND_BLOCKSIZE * 2 * sizeof(float));
	GMixer.fft_setup = pffft_new_setup(SND_BLOCKSIZE, PFFFT_REAL);
#endif

	GMixer.DecodeStop = false;
	GMixer.DecodeThread = thread_spawn(Snd_DecodeThreadProc, "Sound Decode Thread", 0, NULL);

	Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void Mixer::Shutdown()
{
	GMixer.DecodeStop = true;
	if (GMixer.DecodeThread)
	{
		Platform::WaitForSingleObject(GMixer.DecodeThread);
		GMixer.DecodeThread = 0;
	}

	Backend::Shutdown();

	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		delete[] GMixer.ShootingSendFar[Channel];
		GMixer.ShootingSendFar[Channel] = nullptr;
		delete[] GMixer.ShootingSendIndoor[Channel];
		GMixer.ShootingSendIndoor[Channel] = nullptr;
	}
	GMixer.ShootingReverbFar.Free();
	GMixer.ShootingReverbIndoor.Free();

#ifdef DEBUG_DRAW
	if (GMixer.fft_setup)
	{
		pffft_destroy_setup(GMixer.fft_setup);
		GMixer.fft_setup = nullptr;
	}
#endif

	if (GSpatializer)
	{
		GSpatializer->Shutdown();
		delete GSpatializer;
		GSpatializer = nullptr;
	}

	GMixer.free_hrtf_slots.clear();

	if (GReverInterface)
	{
		delete GReverInterface;
		GReverInterface = nullptr;
	}

	GMixer.slots.clear();
	GMixer.free_slots.clear();
	GMixer.free_cachelines.clear();
	GMixer.cache_lines.clear();
	GMixer.sounds.clear();
	GMixer.cmd.clear();
}

ICF void DestroyInternal(int slot)
{
	if (slot == 0)
	{
		return;
	}

	if (!GMixer.slots[slot - 1].sound_name.empty())
	{
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

void Mixer::Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float shooting_volume, float compression, const Fmatrix& mtx, Fvector P, Fvector D, Fvector N)
{
	PROF_EVENT("Sound: Update Stage");
	sound_event* Handler = (sound_event*)event_handler;

	static u64 TimeStamp = Snd_GetTimestamp();
	GMixer.dt = (float)((Snd_GetTimestamp() - TimeStamp) / 1000000) * 0.001f;
	TimeStamp = Snd_GetTimestamp();

	GMixer.time_factor = std::clamp(time_factor, 0.1f, 10.0f);
	GMixer.compression = compression;
	GMixer.master_volume = volume;
	GMixer.effect_volume = eff_volume;
	GMixer.music_volume = mus_volume;
	GMixer.shooting_volume = shooting_volume;
	GMixer.m_V = mtx;

	GMixer.listener_velocity = Snd_Velocity(GMixer.P, P);
	GMixer.P = P;
	GMixer.D = D;
	GMixer.N = N;

	GMixer.render_lock.AcquireExclusive();
	GMixer.manage_lock.AcquireExclusive();
	GMixer.update_lock.AcquireExclusive();

	// Listener hemi -> indoor factor (kept for reference / global use).
	GMixer.IndoorFactor += (Snd_HemiIndoorFactor(GMixer.P) - GMixer.IndoorFactor) *
		std::clamp(GMixer.dt * 3.0f, 0.0f, 1.0f);

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
				if (Handler != nullptr)
				{
					const Fvector& Dist = Slot.parameters[(u32)Mixer::ParameterId::DistanceRange];
					const float SndVolume = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].y;
					float Clip = Dist.z * SndVolume;
					float Range = std::min(Dist.z, Clip);
					if (Range >= 0.1f)
					{
						Handler(RefSound->_p, Range);
					}
				}
			}
		}
	}

	for (size_t Iter = 0; Iter < GMixer.slots.size(); Iter++)
	{
		auto& Slot = GMixer.slots[Iter];
		if (Slot.flags & (u16)Flags::NoFeedback && Slot.state == State::Stopped)
		{
			Destroy(Iter + 1);
		}
		else
		{
			if (Slot.flags & (u16)Flags::Spatial)
			{
				const Fvector& Pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
				Slot.velocity = Snd_Velocity(Slot.prev_position, Pos);
				Slot.prev_position = Pos;
			}

			bool IsOCCEnabled = ((Slot.flags & ((u16)Flags::Intro | (u16)Flags::NoOCC)) == 0) && Slot.state == State::Playing;
			if (IsOCCEnabled)
			{
				Fvector Pos = (Slot.flags & (u16)Flags::Spatial) ? Slot.parameters[(u32)Mixer::ParameterId::Position] : GMixer.P;
				float Dist = GMixer.P.distance_to(Pos);

				// Indoor factor of the SOUND's position: a shot fired inside a
				// room gets the indoor tail even if the listener stands outside.
				if (Slot.flags & (u16)Flags::Shooting)
				{
					Snd_UpdateSlotIndoorFactor(Slot, Pos);
				}

				if (Dist <= Slot.parameters[(u32)Mixer::ParameterId::DistanceRange].y)
				{
					float OutOCC = ::Sound->get_occlusion(Pos, 0.2f, &GMixer.occ);
					float& OldOCC = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
					volume_lerp(OldOCC, OutOCC, 1.0f, GMixer.dt);

					CDB::MODEL* EnvModel = ::Sound->get_geometry_env();
					CDB::COLLIDER* Collider = ::Sound->get_geometry_db();
					if (EnvModel != nullptr)
					{
						Fvector Dir = {0, -1, 0};
						Collider->ray_options(CDB::OPT_ONLYNEAREST);
						Collider->ray_query(EnvModel, Pos, Dir, 1000.f);
						if (Collider->r_count())
						{
							auto& r = Collider->r_any();
							auto& Verts = r.model->verts;
							auto& Tris = r.model->tris;
							auto& T = Tris[r.tris_id];
							auto& verts = T.verts;

							Fvector tri_norm;
							tri_norm.mknormal(Verts[verts[0]], Verts[verts[1]], Verts[verts[2]]);
							r.ModelWorldTransform.transform_tiny(tri_norm);

							R_ASSERT(T.dummy < GMixer.zones.size());
							Slot.zone_idx = T.dummy + 1;
						} 
						else
						{
							Slot.zone_idx = 0;
						}
					} 
					else
					{
						Slot.zone_idx = 0;
					}
				}
			}
		}
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	for (const auto& Command : GMixer.cmd)
	{
		switch (Command.id)
		{
			case ESoundMixerCommands::play:
			{
				bool IsSoundExists = (Command.param1 && GMixer.sounds.contains((ref_sound*)Command.param1));
				ref_sound* RefSound = IsSoundExists ? (ref_sound*)Command.param1 : nullptr;
				u16 Flags = Command.param0;

				auto& ActualSlot = GMixer.slots[Command.slot - 1];
				bool IsSameFile = ActualSlot.sound_name == Command.string_storage.c_str();
				if (!IsSameFile && !ActualSlot.sound_name.empty())
				{
					Snd_ReleaseSound(ActualSlot.sound_name);
					ActualSlot.sound_name.clear();
				}

				sound_source* SourcePtr = IsSameFile ? Snd_FindSound(ActualSlot.sound_name) : Snd_AcquireSound(Command.string_storage.c_str(), false);

				if (SourcePtr == nullptr)
				{
					MixerNewState(Command.slot, State::Stopped);
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
				ActualSlot.sound_name = Command.string_storage.c_str();
				ActualSlot.flags = Flags;
				ActualSlot.fade_volume = 0.0f;

				// Start decoding the first cache line immediately (off the audio thread) so the sound is ready by the time it is rendered.
				Snd_QueueDecode(ActualSlot.sound_name, 0);

				if (ActualSlot.flags & (u16)Flags::Spatial && RefSound != nullptr && RefSound->_g_object() != nullptr)
				{
					ActualSlot.parameters[(u32)Mixer::ParameterId::Position] = ((IRenderable*)RefSound->_g_object())->renderable.xform.c;
				}

				ActualSlot.doppler = 1.0f;
				ActualSlot.velocity.set(0.0f, 0.0f, 0.0f);
				ActualSlot.prev_position = ActualSlot.parameters[(u32)Mixer::ParameterId::Position];

				if (Handler != nullptr)
				{
					float Clip = Source.pub.max_ai_distance * Source.pub.volume;
					float Range = std::min(Source.pub.max_ai_distance, Clip);

					if (Range >= 0.1f && RefSound != nullptr && RefSound->_p != nullptr)
					{
						if (CObject* Object = RefSound->_g_object())
						{
							if (Flags & (u16)Flags::NoFeedback)
							{
								ref_sound_data_ptr DataPtr = new ref_sound_data();
								DataPtr->slot = Command.slot;
								DataPtr->g_type = 0;
								DataPtr->g_object = Object;
								DataPtr->dont_destroy_slot = true;
								DataPtr->fn_attached[0] = Source.pub.path;
								Handler(DataPtr, Range);
							}
							else
							{
								Handler(RefSound->_p, Range);
							}
						}
					}
				}

				if (!fis_zero(Command.param2))
				{
					ActualSlot.delay = (float)*(double*)&Command.param2;
					MixerNewState(Command.slot, State::Delay);
				}
				else
				{
					MixerNewState(Command.slot, State::Playing);
				}
			}
			break;
			case ESoundMixerCommands::pause:
			{
				MixerNewState(Command.slot, State::Paused);
			}
			break;
			case ESoundMixerCommands::stop:
			{
				if (Command.param0)
				{
					GMixer.slots[Command.slot - 1].flags &= ~((u8)Flags::Looped);
					GMixer.slots[Command.slot - 1].stopping_position = GMixer.slots[Command.slot - 1].position;
				}
				else
				{
					MixerNewState(Command.slot, State::Stopped);
					GMixer.slots[Command.slot - 1].position = 0;
					GMixer.slots[Command.slot - 1].stopping_position = (u32)-1;
				}
			}
			break;
			case ESoundMixerCommands::destroy:
			{
				if (GMixer.slots[Command.slot - 1].state != State::Delay)
				{
					DestroyInternal(Command.slot);
				}
			}
			break;
			case ESoundMixerCommands::stop_all:
			{
				for (size_t Iter = 0; Iter < GMixer.slots.size(); Iter++)
				{
					if (GMixer.slots[Iter].sound_name.size() && GMixer.slots[Iter].state != State::Stopped)
					{
						GMixer.slots[Iter].position = 0;
						GMixer.slots[Iter].stopping_position = (u32)-1;
						GMixer.slots[Iter].prev_state = GMixer.slots[Iter].state;
						GMixer.slots[Iter].state = State::Stopped;
						GMixer.slots[Iter].fake_state = State::Stopped;
					}
				}
			}
			break;
			case ESoundMixerCommands::pause_all:
			{
				for (size_t Iter = 0; Iter < GMixer.slots.size(); Iter++)
				{
					if (GMixer.slots[Iter].sound_name.size() && GMixer.slots[Iter].state != State::Stopped && GMixer.slots[Iter].state != State::Paused)
					{
						GMixer.slots[Iter].prev_state = GMixer.slots[Iter].state;
						GMixer.slots[Iter].state = State::Paused;
					}
				}
			}
			break;
			case ESoundMixerCommands::resume_all:
			{
				for (size_t Iter = 0; Iter < GMixer.slots.size(); Iter++)
				{
					if (GMixer.slots[Iter].state == State::Paused && GMixer.slots[Iter].prev_state != State::Paused)
					{
						GMixer.slots[Iter].state = GMixer.slots[Iter].prev_state;
						GMixer.slots[Iter].prev_state = State::Paused;
					}
				}
			}
			break;
			case ESoundMixerCommands::update_parameter:
			{
				GMixer.slots[Command.slot - 1].parameters[(u32)Command.param0] = Fvector{(float)*(double*)&Command.param1, (float)*(double*)&Command.param2, (float)*(double*)&Command.param3};

				auto& MixerSlot = GMixer.slots[Command.slot - 1];
				if (MixerSlot.flags & (u16)Flags::Spatial && Command.param0 == (u16)ParameterId::Position)
				{
					auto& Pos = MixerSlot.parameters[(u32)Mixer::ParameterId::Position];
					float OutOCC = ::Sound->get_occlusion(Pos, 0.2f, &GMixer.occ);
					float& OldOCC = MixerSlot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
					volume_lerp(OldOCC, OutOCC, 1.0f, GMixer.dt);
				}
			}
			break;
			case ESoundMixerCommands::set_volume:
			{
				GMixer.slots[Command.slot - 1].parameters[(u32)ParameterId::VolumePerChannel].y = *(double*)&Command.param1;
			}
			break;
			case ESoundMixerCommands::set_panning:
			{
				GMixer.slots[Command.slot - 1].parameters[(u32)ParameterId::Panning].x = *(double*)&Command.param1;
				GMixer.slots[Command.slot - 1].parameters[(u32)ParameterId::Panning].y = *(double*)&Command.param2;
			}
			break;
		}
	}

	GMixer.cmd.resize(0);
	GMixer.stats.update_time_micros = (Snd_GetTimestamp() - TimeStamp) / 1000;

	GMixer.update_lock.ReleaseExclusive();
	GMixer.manage_lock.ReleaseExclusive();
	GMixer.render_lock.ReleaseExclusive();
}

void Mixer::StopAll()
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.id = ESoundMixerCommands::stop_all});
}

void Mixer::PauseAll()
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.id = ESoundMixerCommands::pause_all});
}

void Mixer::ResumeAll()
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.id = ESoundMixerCommands::resume_all});
}

void Mixer::DereferenceObjects(CObject** object, int count)
{
	xrSRWLockGuard Guard0(GMixer.update_lock);
	xrSRWLockGuard Guard1(GMixer.manage_lock);

	for (auto& SoundRef : GMixer.sounds)
	{
		if (SoundRef == nullptr || SoundRef->_p == nullptr)
		{
			continue;
		}

		for (size_t Iter = 0; Iter < count; Iter++)
		{
			if (object[Iter] == SoundRef->_g_object())
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

	xrSRWLockGuard Guard0(GMixer.update_lock);

	u32 SlotIdx = GMixer.free_slots[GMixer.free_slots.size() - 1];
	GMixer.free_slots.pop_back();
	GMixer.stats.possible_free_count--;

	return SlotIdx;
}

void Mixer::Destroy(u32 SlotID)
{
	if (SlotID == 0 || GMixer.slots[SlotID - 1].state == State::Delay)
	{
		return;
	}

	GMixer.slots[SlotID - 1].fake_state = State::Stopped;

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = SlotID, .id = ESoundMixerCommands::destroy});
	GMixer.stats.possible_free_count++;
}

void Mixer::Play(u32 SlotID, u16 flags, ref_sound* SoundRef, double Delay)
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);

	if (SlotID == 0 || SoundRef == nullptr || SoundRef->_p == nullptr || SoundRef->_p->fn_attached[0] == nullptr)
	{
		return;
	}

	GMixer.slots[SlotID - 1].fake_state = State::Playing;

	GMixer.cmd.emplace_back(SoundCommand{
		.slot = SlotID, .id = ESoundMixerCommands::play, .param0 = flags, .param1 = (u64)SoundRef, .param2 = *(u64*)&Delay, .param3 = (u64)SoundRef->_g_object(), .string_storage = SoundRef->_p->fn_attached[0]
	});
}

void Mixer::PlayNoFeedback(u16 Flags, ref_sound* SoundRef, CObject* Obj, double Delay, float* Pitch, float* Volume, Fvector* Distance, Fvector* Pos)
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);

	u32 SlotIdx = Create();
	if (SlotIdx == 0)
	{
		return;
	}

	auto& Slot = GMixer.slots[SlotIdx - 1];
	Slot.state = State::Paused;

	Slot.fake_state = State::Playing;

	GMixer.cmd.emplace_back(SoundCommand{
		.slot = SlotIdx, .id = ESoundMixerCommands::play, .param0 = Flags, .param1 = (u64)SoundRef, .param2 = *(u64*)&Delay, .param3 = (u64)Obj, .string_storage = SoundRef->_p->fn_attached[0]
	});

	auto Params = SoundRef->_p->get_params();
	Fvector Distances = {Params.min_distance, Params.max_distance, Params.max_ai_distance};

	if (SoundRef->slot())
	{
		Pitch = (Pitch ? Pitch : &Params.freq);
		Distance = (Distance ? Distance : &Distances);
		Pos = (Pos ? Pos : &Params.position);
		Volume = (Volume ? Volume : &Params.volume);
	}

	if (Pitch)
	{
		Mixer::UpdateParameter(SlotIdx, ParameterId::Pitch, Fvector{*Pitch, 1.f, 1.f});
	}
	if (Distance)
	{
		Mixer::UpdateParameter(SlotIdx, ParameterId::DistanceRange, *Distance);
	}
	if (Pos)
	{
		Mixer::UpdateParameter(SlotIdx, ParameterId::Position, *Pos);
	}
	if (Volume)
	{
		Mixer::SetVolume(SlotIdx, *Volume);
	}
}

void Mixer::Pause(u32 slot)
{
	if (slot == 0)
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.slots[slot - 1].fake_state = State::Paused;
	GMixer.cmd.emplace_back(SoundCommand{.slot = slot, .id = ESoundMixerCommands::pause});
}

void Mixer::Stop(u32 SlotID, bool IsDeferred)
{
	if (SlotID == 0)
	{
		return;
	}

	auto& Slot = GMixer.slots[SlotID - 1];
	if (Slot.state == State::Delay)
	{
		return;
	}

	if (!IsDeferred)
	{
		Slot.fake_state = State::Stopped;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = SlotID, .id = ESoundMixerCommands::stop, .param0 = IsDeferred});
}

void Mixer::UpdateParameter(u32 SlotID, ParameterId Parameter, Fvector Value)
{
	if (SlotID == 0)
	{
		return;
	}

	double P0 = Value.x, P1 = Value.y, P2 = Value.z;
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = SlotID, .id = ESoundMixerCommands::update_parameter, .param0 = (u16)Parameter, .param1 = *(u64*)&P0, .param2 = *(u64*)&P1, .param3 = *(u64*)&P2});
}

void Mixer::SetVolume(u32 Slot, double Volume)
{
	if (Slot == 0)
	{
		return;
	}

	Volume = std::clamp(Volume, 0.0, 1.0);
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = Slot, .id = ESoundMixerCommands::set_volume, .param1 = *(u64*)&Volume});
}

void Mixer::SetPanning(u32 slot, double Left, double Right)
{
	if (slot == 0)
	{
		return;
	}

	Left = std::clamp(Left, 0.0, 1.0);
	Right = std::clamp(Right, 0.0, 1.0);

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = slot, .id = ESoundMixerCommands::set_panning, .param1 = *(u64*)&Left, .param2 = *(u64*)&Right});
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

float Mixer::GetPlaytime(u32 SlotID)
{
	if (SlotID == 0)
	{
		return 0.0f;
	}

	return (((float)GMixer.slots[SlotID - 1].position) / (float)SND_SAMPLERATE);
}

float Mixer::GetDuration(u32 SlotID)
{
	xrSRWLockGuard Guard(GMixer.source_lock, true);

	if (SlotID == 0)
	{
		return 0.0f;
	}

	if (!GMixer.slots[SlotID - 1].sound_name.size() || !GMixer.snd_sources.contains(GMixer.slots[SlotID - 1].sound_name))
	{
		return 0.0f;
	}

	auto& Source = GMixer.snd_sources.at(GMixer.slots[SlotID - 1].sound_name);
	return (float)Source.pub.frames_total / (float)SND_SAMPLERATE;
}

bool Mixer::SlotIsRelated(u32 slot)
{
	if (slot == 0)
	{
		return false;
	}

	const xr_string& Name = GMixer.slots[slot - 1].sound_name;
	sound_source* Source = Snd_FindSound(Name);
	if (Source == nullptr)
	{
		return false;
	}

	bool Result = Snd_SlotOcclusion(slot, *Source, 0.0f, nullptr) != ESlotOcclusionResult::False;
	Snd_ReleaseSound(Name);
	return Result;
}

u32 Mixer::GetGameType(u32 Slot)
{
	xrSRWLockGuard Guard(GMixer.source_lock, true);

	if (Slot == 0)
	{
		return 0.0f;
	}

	if (!GMixer.slots[Slot - 1].sound_name.size() || !GMixer.snd_sources.contains(GMixer.slots[Slot - 1].sound_name))
	{
		return 0.0f;
	}

	const auto& Source = GMixer.snd_sources.at(GMixer.slots[Slot - 1].sound_name);
	return Source.pub.game_type;
}

u32 Mixer::GetFlags(u32 Slot)
{
	if (Slot == 0)
	{
		return 0;
	}

	return GMixer.slots[Slot - 1].flags;
}

Mixer::State Mixer::GetState(u32 Slot)
{
	if (Slot == 0)
	{
		return State::Stopped;
	}

	return GMixer.slots[Slot - 1].fake_state;
}

u32 Mixer::GetSourceCount()
{
	xrSRWLockGuard Guard(GMixer.source_lock, true);
	return GMixer.snd_sources.size();
}

const sound_source_public* Mixer::GetSource(u32 index)
{
	xrSRWLockGuard Guard(GMixer.source_lock, true);

	u32 Counter = 0;
	for (const auto& [key, source] : GMixer.snd_sources)
	{
		if (Counter == index)
		{
			return &source.pub;
		}

		Counter++;
	}

	return nullptr;
}

Fvector* Mixer::GetParameters(u32 SlotID)
{
	if (SlotID == 0)
	{
		return nullptr;
	}

	return GMixer.slots[SlotID - 1].parameters;
}

void Mixer::LoadImpulseResponse(const char* name, xr_vector<xr_vector<float>>& ch_audio, u32& sample_rate, u16& num_channels)
{
	sound_source source{};
	Snd_LoadSource(source, name);
	if (source.file.datasource == nullptr)
	{
		return;
	}

	num_channels = (u16)source.pub.channels_count;
	sample_rate = SND_SAMPLERATE;
	const u32 total = source.pub.frames_total;

	ch_audio.resize(SND_CHANNEL_COUNT);
	float* read_buf[SND_CHANNEL_COUNT] = {};
	for (u32 c = 0; c < SND_CHANNEL_COUNT; c++)
	{
		read_buf[c] = xr_alloc<float>(SND_BLOCKSIZE);
	}

	u32 pos = 0;
	while (pos < total)
	{
		u32 to_read = std::min((u32)SND_BLOCKSIZE, total - pos);
		u32 got = Snd_ReadFromSource(source, read_buf, to_read);
		if (got == 0)
		{
			break;
		}
		for (u32 c = 0; c < SND_CHANNEL_COUNT; c++)
		{
			if (ch_audio[c].size() < pos + got)
			{
				ch_audio[c].resize(pos + got);
			}
			memcpy(ch_audio[c].data() + pos, read_buf[c], got * sizeof(float));
		}
		pos += got;
	}

	for (u32 c = 0; c < SND_CHANNEL_COUNT; c++)
	{
		xr_free(read_buf[c]);
	}

	ov_clear(&source.file);
	xr_delete(source.reader);
	xr_free(source.data);
}

void Mixer::AddEditorZone(sound_zone_params& params)
{
	GMixer.editor_zone = true;
	ResetZones();
	AddZone(params);
}

void Mixer::AddZone(sound_zone_params& params)
{
	if (GReverInterface)
	{
		GReverInterface->InitZone(params);
	}

	GMixer.zones.emplace_back(std::move(params));
}

void Mixer::ResetZones()
{
	xrSRWLockGuard Guard(GMixer.render_lock);

	for (auto& Zone : GMixer.zones)
	{
		if (GReverInterface)
		{
			GReverInterface->ReleaseZone(Zone);
		}
	}

	GMixer.zones.clear();
}

const xr_vector<sound_zone_params>& Mixer::GetZones()
{
	return GMixer.zones;
}

ref_sound::ref_sound()
{
	xrSRWLockGuard Guard1(GMixer.manage_lock);

	if (!GMixer.sounds.contains(this))
	{
		GMixer.sounds.emplace(this);
	}
}

ref_sound::~ref_sound()
{
	xrSRWLockGuard Guard1(GMixer.manage_lock);

	if (GMixer.sounds.contains(this))
	{
		GMixer.sounds.erase(this);
	}
}