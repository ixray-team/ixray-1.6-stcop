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
#include "SoundSource.h"
#include "SoundBackend.h"
#include "SoundDSP.h"
#include "SoundAlgoritmicReverb.h"

#include "../Sound.h"
#include "../SoundRender.h"
#include "../ai_sounds.h"
#include "SoundConvolutionReverb.h"

#include <pffft.h>

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#include "SoundSpatializer.h"

#ifndef DISABLE_STEAM_AUDIO
#include "../Plugins/SteamAudio.h"
#endif
#ifndef DISABLE_RESONANCE_AUDIO
#include "../Plugins/ResonanceAudio.h"
#endif

ISoundSpatializer* GSpatializer = nullptr;

#define DEFAULT_SLOT_COUNT (512)
#define SND_MAX_PITCH (4)
#define SND_MAX_VELOCITY (100.0f)

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

struct sound_bus_state
{
	float data[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
};

struct sound_mixer_state
{
	xrSRWLock render_lock;
	xrSRWLock update_lock;
	xrSRWLock manage_lock;
	xrCriticalSection play_lock;

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
	Fvector occ[3]; // occluder triangle cache for get_occlusion()

	xr_vector<u32> free_slots;
	xr_vector<SoundCommand> cmd;
	xr_vector<sound_slot_state> slots;
	xr_hash_set<ref_sound*> sounds;

	// HRTF slot management (index pool; backend state lives in the spatializer plugin)
	xr_vector<u32> free_hrtf_slots;

	sound_bus_state buses[SND_BUS_COUNT];

	float IndoorFactor = 0.0f;

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

static void Snd_GrowSlots(bool IsLockUpdate)
{
	xrSRWLockGuard Guard0(GMixer.render_lock, false);
	xrSRWLockGuard Guard1(GMixer.manage_lock, false);
	bool Locked = !g_SoundSourceLock.TryAcquireExclusive();

	if (IsLockUpdate)
	{
		GMixer.update_lock.AcquireExclusive();
	}

	size_t OldSize = GMixer.slots.size();
	size_t NewSize = std::max((size_t)DEFAULT_SLOT_COUNT, GMixer.slots.size() * 2);

	GMixer.slots.resize(NewSize);
	GMixer.free_slots.reserve(NewSize);

	g_SoundStats.possible_free_count += (NewSize - OldSize);
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
		g_SoundSourceLock.ReleaseExclusive();
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

enum class ESlotOcclusionResult
{
	False,
	True,
	SOM
};

ICF ESlotOcclusionResult Snd_SlotOcclusion(u32 slot_idx, const SoundSourceState& source, float dt, float* occ_volume)
{
	PROF_EVENT("Sound: SlotOcclusion");
	auto& slot = GMixer.slots[slot_idx - 1];
	if (slot.state != Mixer::State::Playing)
	{
		return ESlotOcclusionResult::False;
	}

	ESlotOcclusionResult Result = ESlotOcclusionResult::True;

	if (source.Desc.channels_count == 1 && slot.flags & (u32)Mixer::Flags::Spatial)
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

ICF u32 Snd_ReadSlotData(u32 SlotIdx, SoundSourceState& Source, float** Data, u32 FramesCount)
{
	auto& Slot = GMixer.slots[SlotIdx - 1];

	u32 ReadPostion = Slot.position;
	u32 Frames2Read = FramesCount;
	u32 WaitSpins = 0;

	while (Frames2Read && ReadPostion < Source.Desc.frames_total)
	{
		float* Dest[SND_CHANNEL_COUNT];
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			Dest[Channel] = &Data[Channel][FramesCount - Frames2Read];
		}

		u32 CacheFrames = Snd_CopyCached(&Source, ReadPostion, Dest, Frames2Read);
		if (CacheFrames != 0)
		{
			Frames2Read -= CacheFrames;
			ReadPostion += CacheFrames;
			WaitSpins = 0;
			continue;
		}

		PROF_EVENT("Decode OGG Wait");
		SND_STAT_ADD(g_SoundStats.render_cache_miss, 1u);
		Snd_QueueDecode(&Slot.sound_name, ReadPostion);
		if (++WaitSpins > 4096)
		{
			break;
		}

		std::this_thread::yield();
	}

	return FramesCount - Frames2Read;
}


ICF void Snd_ReadSlot(u32 slot_idx, SoundSourceState& source, float** data, u32 frames_count)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	if (source.Desc.frames_total == 0)
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
		slot.position = std::min(slot.position + read_frames, source.Desc.frames_total);

		if (slot.position < source.Desc.frames_total)
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

ICF void Snd_ProcessSlot(u32 slot_idx, SoundSourceState& source, float** data)
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

		g_SoundStats.frame_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
		timestamp = Snd_GetTimestamp();

		if (counter % 100 == 0)
		{
			g_SoundStats.cache_hit_count = 0;
			g_SoundStats.cache_miss_count = 0;
		}

		for (size_t i = 0; i < GMixer.slots.size(); i++)
		{
			PROF_EVENT("Sound: GMixer.slot");
			auto& slot = GMixer.slots[i];

			SoundSourceState* source = Snd_FindSource(&slot.sound_name);
			if (source != nullptr && Snd_SlotOcclusion(i + 1, *source, dt, nullptr) != ESlotOcclusionResult::False)
			{
				Snd_AcquireHRTFSlot(i + 1);
				// Hand the decode off to the decode thread; only enqueue if the cache
				// line for the current position isn't already filled.
				if (!Snd_HasCacheLine(source, slot.position))
				{
					PROF_EVENT("Sound: QueueDecode");
					Snd_QueueDecode(&slot.sound_name, slot.position);
				}
			}
			else
			{
				Snd_ReleaseHRTFSlot(i + 1);
			}

			if (source != nullptr)
			{
				PROF_EVENT("Sound: ReleaseSound");
				Snd_ReleaseSource(&slot.sound_name);
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

		g_SoundStats.precache_time_micros = (Snd_GetTimestamp() - timestamp) / 1000;
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
	if (!Slot.IndoorFactorValid)
	{
		Slot.IndoorFactor = target;
		Slot.IndoorFactorValid = true;
		return;
	}

	const float k = std::clamp(GMixer.dt * 3.0f, 0.0f, 1.0f);
	Slot.IndoorFactor += (target - Slot.IndoorFactor) * k;
}

ICF void Snd_RenderSlot(u32 SlotIdx, SoundSourceState& Source, float** process_buffer, float dt)
{
	auto& Slot = GMixer.slots[SlotIdx - 1];

	float OCCVolume = 1.0f;
	ESlotOcclusionResult OCCResult = Snd_SlotOcclusion(SlotIdx, Source, dt, &OCCVolume);
	if (OCCResult == ESlotOcclusionResult::False)
	{
		// TODO: hack for simulated sounds
		Slot.position = std::min(Slot.position + SND_BLOCKSIZE, Source.Desc.frames_total);
		if (Slot.position == Source.Desc.frames_total && (Slot.flags & (u16)Mixer::Flags::Looped) == 0)
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
		u32 stopping_total = Source.Desc.frames_total - Slot.stopping_position;
		if (stopping_total > 1 && Slot.position >= Slot.stopping_position)
		{
			u32 begin_offset = Slot.position - Slot.stopping_position;
			u32 write_count = IsMusic ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * GMixer.time_factor);
			u32 end_offset = std::min(begin_offset + write_count, Source.Desc.frames_total - 1);
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

	if (Slot.flags & (u16)Mixer::Flags::Shooting)
	{
		Snd_ShootingReverbSend(Slot, GMixer.P, process_buffer, BeginFactor, EndFactor);
	}

	// Spatial processing
	if (!(Slot.flags & (u32)Mixer::Flags::Intro) && Source.Desc.channels_count == 1)
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

		float ZoneFade = std::clamp(Slot.IndoorFactor, 0.0f, 1.0f);
		Snd_SendToReverbZone(Slot.zone_idx, process_buffer, BeginFactor * ZoneFade, EndFactor * ZoneFade, left_panning, right_panning);
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

	g_SoundStats.render_cache_miss = 0;

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

	Snd_BeginReverbBlock();

	for (size_t i = 0; i < GMixer.slots.size(); i++)
	{
		PROF_EVENT("Slot Render");
		if (GMixer.slots[i].state != Mixer::State::Playing)
		{
			continue;
		}

		SoundSourceState* Source = Snd_FindSource(&GMixer.slots[i].sound_name);
		if (Source == nullptr)
		{
			MixerNewState(i + 1, Mixer::State::Stopped);
			continue;
		}

		Snd_RenderSlot(i + 1, *Source, process_buffer, dt);
		Snd_ReleaseSource(&GMixer.slots[i].sound_name);
	}

	for (size_t Iter = 0; Iter < SND_CHANNEL_COUNT; Iter++)
	{
		process_buffer[Iter] = _process_buffer[Iter];
	}

	{
		float* ReverbBus[SND_CHANNEL_COUNT] = {};
		for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			ReverbBus[Channel] = GMixer.buses[SND_BUS_REVERB].data[Channel];
		}

		Snd_RenderReverbZones(process_buffer, ReverbBus);
		Snd_RenderShootingReverb(ReverbBus);
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
	g_SoundStats.spectral_data[0] = lin2dB(fabs(GMixer.aligned_output_fft[0]) / float(SND_BLOCKSIZE));

	for (size_t k = 1; k < SND_BLOCKSIZE / 2; k++)
	{
		float re = GMixer.aligned_output_fft[k];
		float im = GMixer.aligned_output_fft[SND_BLOCKSIZE + k - 1];
		float mag = sqrtf(re * re + im * im) / float(SND_BLOCKSIZE);
		g_SoundStats.spectral_data[k] = lin2dB(mag);
	}

	g_SoundStats.spectral_data[SND_BLOCKSIZE / 2] = lin2dB(fabs(GMixer.aligned_output_fft[SND_BLOCKSIZE / 2]) / float(SND_BLOCKSIZE));

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

	memcpy(g_SoundStats.channel_volumes, Volumes, sizeof(Volumes));
#endif

	g_SoundStats.render_time_micros = (Snd_GetTimestamp() - TimeStamp) / 1000;
}

void Mixer::Initialize()
{
	GMixer.slots.clear();
	GMixer.free_slots.clear();
	GMixer.sounds.clear();
	GMixer.cmd.clear();
	GMixer.cmd.reserve(256);
	Snd_GrowSlots(true);

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

	Snd_InitShootingReverb();

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

	Snd_InitSources();

	Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void Mixer::Shutdown()
{
	Backend::Shutdown();

	Snd_ShutdownSources();

	Snd_ShutdownShootingReverb();

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

	Snd_ShutdownReverb();

	GMixer.slots.clear();
	GMixer.free_slots.clear();
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
		Snd_ReleaseSource(&GMixer.slots[slot - 1].sound_name);
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

void Mixer::Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float shooting_volume, float compression, Fvector P, Fvector D, Fvector N)
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

				if (Dist <= Slot.parameters[(u32)Mixer::ParameterId::DistanceRange].y)
				{
					if (Slot.flags & (u16)Flags::Spatial)
					{
						Snd_UpdateSlotIndoorFactor(Slot, Pos);
					}
					else
					{
						Slot.IndoorFactor = GMixer.IndoorFactor;
						Slot.IndoorFactorValid = true;
					}

					float OutOCC = ::Sound->get_occlusion(Pos, 0.2f, GMixer.occ);
					float& OldOCC = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel].z;
					volume_lerp(OldOCC, OutOCC, 1.0f, GMixer.dt);

					Slot.zone_idx = Snd_FindReverbZone(Pos);
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
				const xr_string NewName = Command.string_storage.size() ? Command.string_storage.c_str() : "";
				bool IsSameFile = ActualSlot.sound_name == NewName;
				if (!IsSameFile && !ActualSlot.sound_name.empty())
				{
					Snd_ReleaseSource(&ActualSlot.sound_name);
					ActualSlot.sound_name.clear();
				}

				SoundSourceState* SourcePtr = IsSameFile ? Snd_FindSource(&ActualSlot.sound_name) : Snd_AcquireSource(&NewName);

				if (SourcePtr == nullptr)
				{
					MixerNewState(Command.slot, State::Stopped);
					break;
				}

				if (IsSameFile)
				{
					// The slot already holds a reference on it
					Snd_ReleaseSource(&ActualSlot.sound_name);
				}

				auto& Source = *SourcePtr;
				memset(ActualSlot.parameters, 0, sizeof(ActualSlot.parameters));
				memset(ActualSlot.history, 0, sizeof(ActualSlot.history));
				ActualSlot.parameters[(u32)Mixer::ParameterId::VolumePerChannel] = Fvector(Source.Desc.volume, 1.0f, 1.0f);
				ActualSlot.parameters[(u32)Mixer::ParameterId::DistanceRange] = Fvector(Source.Desc.min_distance, Source.Desc.max_distance, Source.Desc.max_ai_distance);
				ActualSlot.parameters[(u32)Mixer::ParameterId::Pitch] = Fvector{1.0f, 1.0f, 1.0f};
				ActualSlot.parameters[(u32)Mixer::ParameterId::Panning] = Fvector{1.0f, 1.0f, 1.0f};
				ActualSlot.position = 0;
				ActualSlot.stopping_position = (u32)-1;
				ActualSlot.sound_name = NewName;
				ActualSlot.flags = Flags;
				ActualSlot.fade_volume = 0.0f;
				ActualSlot.ReverbDryGain = -1.0f;
				ActualSlot.IndoorFactor = GMixer.IndoorFactor;
				ActualSlot.IndoorFactorValid = false;
				ActualSlot.zone_idx = 0;

				// Start decoding the first cache line immediately (off the audio thread) so the sound is ready by the time it is rendered.
				Snd_QueueDecode(&ActualSlot.sound_name, 0);

				if (ActualSlot.flags & (u16)Flags::Spatial && RefSound != nullptr && RefSound->_g_object() != nullptr)
				{
					ActualSlot.parameters[(u32)Mixer::ParameterId::Position] = ((IRenderable*)RefSound->_g_object())->renderable.xform.c;
				}

				ActualSlot.doppler = 1.0f;
				ActualSlot.velocity.set(0.0f, 0.0f, 0.0f);
				ActualSlot.prev_position = ActualSlot.parameters[(u32)Mixer::ParameterId::Position];

				if (Handler != nullptr)
				{
					float Clip = Source.Desc.max_ai_distance * Source.Desc.volume;
					float Range = std::min(Source.Desc.max_ai_distance, Clip);

					if (Range >= 0.1f && RefSound != nullptr && RefSound->_p != nullptr)
					{
						if (CObject* Object = RefSound->_g_object())
						{
							int GameType = RefSound->_p->g_type;
							if (GameType == (int)sg_SourceType)
							{
								GameType = (int)Source.Desc.game_type;
								RefSound->_p->g_type = GameType;
							}

							if (Flags & (u16)Flags::NoFeedback)
							{
								ref_sound_data_ptr DataPtr = new ref_sound_data();
								DataPtr->slot = Command.slot;
								DataPtr->g_type = GameType;
								DataPtr->g_object = Object;
								DataPtr->dont_destroy_slot = true;
								DataPtr->fn_attached[0] = Source.Desc.path;
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
					float OutOCC = ::Sound->get_occlusion(Pos, 0.2f, GMixer.occ);
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
	g_SoundStats.update_time_micros = (Snd_GetTimestamp() - TimeStamp) / 1000;

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
	g_SoundStats.possible_free_count--;

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
	g_SoundStats.possible_free_count++;
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
	return &g_SoundStats;
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
	xrSRWLockGuard Guard(g_SoundSourceLock, true);

	if (SlotID == 0)
	{
		return 0.0f;
	}

	const SoundSourceState* Source = Snd_LookupSource(&GMixer.slots[SlotID - 1].sound_name);
	if (Source == nullptr)
	{
		return 0.0f;
	}

	return (float)Source->Desc.frames_total / (float)SND_SAMPLERATE;
}

bool Mixer::SlotIsRelated(u32 slot)
{
	if (slot == 0)
	{
		return false;
	}

	const xr_string& Name = GMixer.slots[slot - 1].sound_name;
	SoundSourceState* Source = Snd_FindSource(&Name);
	if (Source == nullptr)
	{
		return false;
	}

	bool Result = Snd_SlotOcclusion(slot, *Source, 0.0f, nullptr) != ESlotOcclusionResult::False;
	Snd_ReleaseSource(&Name);
	return Result;
}

u32 Mixer::GetGameType(u32 Slot)
{
	xrSRWLockGuard Guard(g_SoundSourceLock, true);

	if (Slot == 0)
	{
		return 0.0f;
	}

	const SoundSourceState* Source = Snd_LookupSource(&GMixer.slots[Slot - 1].sound_name);
	if (Source == nullptr)
	{
		return 0;
	}

	return Source->Desc.game_type;
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

Fvector* Mixer::GetParameters(u32 SlotID)
{
	if (SlotID == 0)
	{
		return nullptr;
	}

	return GMixer.slots[SlotID - 1].parameters;
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