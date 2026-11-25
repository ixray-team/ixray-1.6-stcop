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
#include "SoundBus.h"
#include "SoundConfig.h"
#include "SoundEffect.h"
#include "SoundShaders.h"
#include "SoundAnalyzer.h"

#include "../Sound.h"
#include "../SoundRender.h"
#include "../ai_sounds.h"
#include "SoundHrtf.h"

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#define DEFAULT_SLOT_COUNT (1024)
#define SND_MAX_PITCH (4)
// A position jump faster than this is a teleport, not motion
#define SND_MAX_VELOCITY (100.0f)
// Seconds of velocity smoothing: positions that move every few frames (physics steps, scripts) average out
#define SND_VELOCITY_SMOOTH (0.08f)

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
	set_panning,
	slot_shader,
	bus_shader,
	slot_route,
	bus_route,
	bus_release
};

enum class ESoundShaderOp : u8
{
	Add,
	Remove,
	Clear,
	SetParam,
	// Copies the chain of the slot in param3
	Copy,
	// param2: target, param3: seconds (float bits) | curve << 32
	Ramp,
	// param2: rate, param3: depth (float bits) | shape << 32
	Lfo,
	StopAutomation
};

static u64 Snd_PackFloat(float Value, u8 Extra)
{
	u32 Bits = 0;
	memcpy(&Bits, &Value, sizeof(Bits));
	return (u64)Bits | ((u64)Extra << 32);
}

static float Snd_UnpackFloat(u64 Packed)
{
	const u32 Bits = (u32)Packed;
	float Value = 0.0f;
	memcpy(&Value, &Bits, sizeof(Value));
	return Value;
}

struct SoundCommand
{
	u32 slot;
	ESoundMixerCommands id;
	u16 param0;
	u64 param1;
	u64 param2;
	u64 param3;
	shared_str string_storage;
	u32 bus = 0;
	u32 route = 0;
};

struct sound_mixer_state
{
	xrSRWLock render_lock;
	xrSRWLock update_lock;
	xrSRWLock manage_lock;
	xrCriticalSection play_lock;

	float dt;
	float time_factor = 1.0f;
	float mute_volume = 1.0f;
	float compression = -1.0f;
	float shooting_reverb = -1.0f;
	u32 default_bus = 0;
	SoundEffectListener listener = {};
	Fvector occ[3]; // occluder triangle cache for get_occlusion()

	xr_vector<u32> free_slots;
	xr_vector<SoundCommand> cmd;
	xr_vector<sound_slot_state> slots;
	xr_hash_set<ref_sound*> sounds;

	float IndoorFactor = 0.0f;

	float read_buffer[SND_CHANNEL_COUNT][(SND_BLOCKSIZE + 1) * 10];
};

static sound_mixer_state GMixer = {};

static void Snd_ApplyShaderCommand(SoundShaderChain& Chain, const SoundCommand& Command)
{
	const SoundShaderType Type = (SoundShaderType)Command.param0;
	switch ((ESoundShaderOp)Command.param1)
	{
	case ESoundShaderOp::Add:
		Chain.Add(Type, Command.param3 != 0);
		break;
	case ESoundShaderOp::Remove:
		Chain.Remove(Type, Command.param3 != 0);
		break;
	case ESoundShaderOp::Clear:
		Chain.Clear();
		break;
	case ESoundShaderOp::SetParam:
		Chain.SetParam(Type, Command.string_storage.c_str(), (float)*(double*)&Command.param2);
		break;
	case ESoundShaderOp::Copy:
		if (Command.param3 != 0 && Command.param3 <= GMixer.slots.size())
		{
			Chain.CopyFrom(GMixer.slots[Command.param3 - 1].Shaders);
		}
		break;
	case ESoundShaderOp::Ramp:
		Chain.RampParam(Type, Command.string_storage.c_str(), (float)*(double*)&Command.param2, Snd_UnpackFloat(Command.param3), (Shader::Curve)(Command.param3 >> 32));
		break;
	case ESoundShaderOp::Lfo:
		Chain.SetParamLfo(Type, Command.string_storage.c_str(), (float)*(double*)&Command.param2, Snd_UnpackFloat(Command.param3), (Shader::LfoShape)(Command.param3 >> 32));
		break;
	case ESoundShaderOp::StopAutomation:
		Chain.StopAutomation(Type, Command.string_storage.c_str());
		break;
	}
}

// Extra: source slot for Copy, SpatialOnly for Add / Remove, packed seconds / depth for Ramp / Lfo
static void Snd_QueueShaderCommand(ESoundMixerCommands Id, u32 Target, SoundShaderType Type, ESoundShaderOp Op, double Value = 0.0, const char* Param = nullptr, u64 Extra = 0)
{
	if (Target == 0 || (Op != ESoundShaderOp::Clear && Op != ESoundShaderOp::Copy && (Type == SoundShaderType::None || Type >= SoundShaderType::Count)))
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{
		.slot = Target, .id = Id, .param0 = (u16)Type, .param1 = (u64)Op, .param2 = *(u64*)&Value, .param3 = Extra, .string_storage = Param
	});
}

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

static void Snd_ReleaseVoiceEffects(sound_slot_state& Slot, SoundEffectOp Op)
{
	for (u32 EffectIdx = 0; EffectIdx < SND_VOICE_EFFECT_COUNT; EffectIdx++)
	{
		Snd_CallEffect(Slot.VoiceEffects[EffectIdx], Slot.VoiceEffectState[EffectIdx], Op, nullptr);
		if (Op == SoundEffectOp::Destroy)
		{
			Slot.VoiceEffects[EffectIdx] = 0;
		}
	}
}

static void Snd_ProcessVoiceEffects(sound_slot_state& Slot, const SoundBus* Bus, SoundEffectProcess* Process)
{
	for (u32 EffectIdx = 0; EffectIdx < SND_VOICE_EFFECT_COUNT; EffectIdx++)
	{
		const SoundBusEffect* Effect = EffectIdx < Bus->VoiceEffectCount ? &Bus->VoiceEffects[EffectIdx] : nullptr;
		u8 Id = Effect != nullptr ? Effect->Effect : 0;
		void* State = Slot.VoiceEffectState[EffectIdx];
		if (Slot.VoiceEffects[EffectIdx] != Id)
		{
			Snd_CallEffect(Slot.VoiceEffects[EffectIdx], State, SoundEffectOp::Destroy, nullptr);
			memset(State, 0, SND_VOICE_EFFECT_STATE_SIZE);

			SoundEffectCreate Create = {Effect != nullptr ? Effect->Params : nullptr, nullptr};
			Slot.VoiceEffects[EffectIdx] = Snd_CallEffect(Id, State, SoundEffectOp::Create, &Create) ? Id : 0;
		}

		if (Slot.VoiceEffects[EffectIdx] != 0)
		{
			Process->Params = Effect->Params;
			Snd_CallEffect(Id, State, SoundEffectOp::Process, Process);
		}
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
		float dist = GMixer.listener.Position.distance_to(pos);
		if (dist > distances.y)
		{
			if (occ_volume)
			{
				*occ_volume = 0.0f;
			}
			return ESlotOcclusionResult::False;
		}

		// Sounds attached to the car (engine, doors, horn, etc.) must not be
		// muffled by vegetation or other occluders around the listener.
		if ((slot.flags & (u32)Mixer::Flags::NoOCC) == 0 && occ_volume != nullptr)
		{
			float occ = Sound->get_occlusion_to(GMixer.listener.Position, pos);
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
	u32 DecodeAttempts = 0;

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
			DecodeAttempts = 0;
			continue;
		}

		// Cache miss: the decode thread didn't make it in time (or never got the request).
		// Don't wait for it, decode the line right here on the render thread.
		SND_STAT_ADD(g_SoundStats.render_cache_miss, 1u);
		if (++DecodeAttempts > 2 || !Snd_DecodeNow(&Source, ReadPostion))
		{
			break;
		}
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
			if (read_frames != 0)
			{
				continue;
			}

			// Nothing decodable from here on: treat it as the end of the stream (loop or stop)
			if (slot.position == 0)
			{
				MixerNewState(slot_idx, Mixer::State::Stopped);
				break;
			}
		}

		slot.position = 0;
		if ((slot.flags & (u32)Mixer::Flags::Looped) == 0)
		{
			MixerNewState(slot_idx, Mixer::State::Stopped);
			break;
		}
	}
}

// Returns the source frames read per output frame
ICF float Snd_ProcessSlot(u32 slot_idx, SoundSourceState& source, float** data)
{
	auto& slot = GMixer.slots[slot_idx - 1];
	float pitch = slot.parameters[(u32)Mixer::ParameterId::Pitch].x;

	u32 output_frames = SND_BLOCKSIZE;
	float ratio = std::clamp(pitch * slot.doppler * GMixer.time_factor, 0.0f, (float)SND_MAX_PITCH);
	// Glide from the ratio of the last block, so doppler and pitch changes are not stepped
	const float begin_ratio = slot.LastRate > 0.0f ? slot.LastRate : ratio;
	slot.LastRate = ratio;
	u32 input_frames = std::max((u32)((float)output_frames * (begin_ratio + ratio) * 0.5f + 0.5f), 1u);

	bool is_music = (slot.flags & (u16)Mixer::Flags::Intro);

	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memset(GMixer.read_buffer[Channel], 0, (input_frames + 1) * sizeof(float));
	}

	if (is_music || (fis_zero(1.0f - ratio) && fis_zero(1.0f - begin_ratio)))
	{
		Snd_ReadSlot(slot_idx, source, data, SND_BLOCKSIZE);
		return 1.0f;
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

		if (fis_zero(begin_ratio - ratio))
		{
			DSP_ResampleBuffer(offfseted_data, data, slot.history, input_frames, output_frames);
		}
		else
		{
			DSP_ResampleBufferRamp(offfseted_data, data, slot.history, input_frames, output_frames, begin_ratio, ratio);
		}
	}

	return (float)input_frames / (float)output_frames;
}

struct SoundShaderSourceUser
{
	u32 SlotIdx;
	SoundSourceState* Source;
};

static u32 Snd_ShaderReadSource(void* User, u32 Position, float** Data, u32 Frames)
{
	const SoundShaderSourceUser* Info = (const SoundShaderSourceUser*)User;
	SoundSourceState* Source = Info->Source;
	u32 Read = 0;
	u32 DecodeAttempts = 0;
	while (Read < Frames && Position + Read < Source->Desc.frames_total)
	{
		float* Dest[SND_CHANNEL_COUNT];
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			Dest[Channel] = Data[Channel] + Read;
		}

		const u32 CacheFrames = Snd_CopyCached(Source, Position + Read, Dest, Frames - Read);
		if (CacheFrames != 0)
		{
			Read += CacheFrames;
			DecodeAttempts = 0;
			continue;
		}

		SND_STAT_ADD(g_SoundStats.render_cache_miss, 1u);
		if (++DecodeAttempts > 2 || !Snd_DecodeNow(Source, Position + Read))
		{
			break;
		}
	}
	return Read;
}

static void Snd_ShaderPrefetchSource(void* User, u32 Position)
{
	const SoundShaderSourceUser* Info = (const SoundShaderSourceUser*)User;
	GMixer.slots[Info->SlotIdx - 1].ShaderPrefetch = Position;
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
				// Async decode of the current position and the read-ahead window.
				// Whatever doesn't make it in time is decoded on the fly by the render thread.
				PROF_EVENT("Sound: Prefetch");
				Snd_PrefetchSource(source, &slot.sound_name, slot.position, (slot.flags & (u8)Mixer::Flags::Looped) != 0);

				// A shader reads elsewhere in the source (reverse)
				const u32 ShaderPrefetch = slot.ShaderPrefetch;
				if (ShaderPrefetch != (u32)-1)
				{
					Snd_PrefetchSource(source, &slot.sound_name, ShaderPrefetch, (slot.flags & (u8)Mixer::Flags::Looped) != 0);
				}
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

// Smoothed velocity of a point that moved From -> To during the last frame. A teleport drops the history
ICF void Snd_UpdateVelocity(Fvector& Velocity, const Fvector& From, const Fvector& To)
{
	if (GMixer.dt <= 0.0f)
	{
		return;
	}

	Fvector Frame;
	Frame.sub(To, From).mul(1.0f / GMixer.dt);
	if (Frame.square_magnitude() > SND_MAX_VELOCITY * SND_MAX_VELOCITY)
	{
		Velocity.set(0.0f, 0.0f, 0.0f);
		return;
	}

	const float Alpha = 1.0f - std::exp(-GMixer.dt / SND_VELOCITY_SMOOTH);
	Velocity.x += (Frame.x - Velocity.x) * Alpha;
	Velocity.y += (Frame.y - Velocity.y) * Alpha;
	Velocity.z += (Frame.z - Velocity.z) * Alpha;
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
		Collider->ray_query(*EnvModel, Pos, dir, kMaxRange);
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

static void Snd_DirectSend(sound_slot_state& Slot, const SoundBus* Bus, float** Data, float BusGain, float& BeginFactor, float& EndFactor, float Left, float Right)
{
	constexpr float OutdoorCriticalDistance = 25.0f;
	constexpr float IndoorCriticalDistance = 4.0f;
	constexpr float MinReverbDistance = 5.0f;

	bool IsSpatial = (Slot.flags & (u16)Mixer::Flags::Spatial) != 0;
	// Measured past min_distance: distance-band layers already carry the distance they were recorded at
	float Distance = IsSpatial ? GMixer.listener.Position.distance_to(Slot.parameters[(u32)Mixer::ParameterId::Position]) - Slot.parameters[(u32)Mixer::ParameterId::DistanceRange].x : 0.0f;
	float Indoor = std::clamp(Slot.IndoorFactor, 0.0f, 1.0f);
	float CriticalDistance = lerp(OutdoorCriticalDistance, IndoorCriticalDistance, Indoor);
	float Ratio = Bus->DirectRatio * std::max(Distance, MinReverbDistance) / CriticalDistance;
	float Dry = 1.0f / std::sqrt(1.0f + Ratio * Ratio);
	float Wet = Ratio * Dry * BusGain;

	float FarGain = Wet * std::sqrt(1.0f - Indoor);
	float IndoorGain = Wet * std::sqrt(Indoor);
	Snd_SendToBus(Bus->FarSend, Data, BeginFactor * FarGain, EndFactor * FarGain, Left, Right, Slot.flags);
	Snd_SendToBus(Bus->IndoorSend, Data, BeginFactor * IndoorGain, EndFactor * IndoorGain, Left, Right, Slot.flags);

	float PrevDry = Slot.ReverbDryGain < 0.0f ? Dry : Slot.ReverbDryGain;
	Slot.ReverbDryGain = Dry;
	BeginFactor *= PrevDry;
	EndFactor *= Dry;
}

ICF void Snd_RenderSlot(u32 SlotIdx, SoundSourceState& Source, float** process_buffer, float dt)
{
	auto& Slot = GMixer.slots[SlotIdx - 1];

	if (Slot.stopping_position != (u32)-1 && Slot.position >= std::min(Slot.stopping_end, Source.Desc.frames_total))
	{
		MixerNewState(SlotIdx, Mixer::State::Stopped);
		Slot.stopping_position = (u32)-1;
		Slot.stopping_end = (u32)-1;
		return;
	}

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

		Snd_ReleaseVoiceEffects(Slot, SoundEffectOp::Reset);
		Slot.SendGain = 0.0f;
		return;
	}

	// Clear process buffer and read data from source
	for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memset(process_buffer[Channel], 0, SND_BLOCKSIZE * sizeof(float));
	}

	const u32 StartPosition = Slot.position;
	const float Rate = Snd_ProcessSlot(SlotIdx, Source, process_buffer);

	// 3D voices get doppler by default; a "doppler" shader in a chain replaces it (amount = 0 turns it off)
	const bool IsSpatialVoice = (Slot.flags & (u16)Mixer::Flags::Spatial) != 0;
	const bool UseDefaultDoppler = IsSpatialVoice && Slot.SourceShaders.Find(SoundShaderType::Doppler) == nullptr && Slot.Shaders.Find(SoundShaderType::Doppler) == nullptr;
	if (UseDefaultDoppler && !Slot.DefaultDoppler)
	{
		Slot.DefaultDoppler.reset(Shader::Create(SoundShaderType::Doppler));
	}

	if (!Slot.SourceShaders.IsEmpty() || !Slot.Shaders.IsEmpty() || (UseDefaultDoppler && Slot.DefaultDoppler))
	{
		PROF_EVENT("Slot Shaders");
		SoundShaderSourceUser SourceUser = {SlotIdx, &Source};
		SoundShaderSource SourceAccess;
		SourceAccess.User = &SourceUser;
		SourceAccess.Read = Snd_ShaderReadSource;
		SourceAccess.Prefetch = Snd_ShaderPrefetchSource;
		SourceAccess.TotalFrames = Source.Desc.frames_total;
		SourceAccess.Position = StartPosition;
		SourceAccess.Rate = Rate;
		SourceAccess.IsLooped = (Slot.flags & (u16)Mixer::Flags::Looped) != 0;
		SoundShaderSpace Space;
		Space.ListenerPosition = GMixer.listener.Position;
		Space.ListenerVelocity = GMixer.listener.Velocity;
		Space.VoicePosition = Slot.parameters[(u32)Mixer::ParameterId::Position];
		Space.VoiceVelocity = Slot.velocity;

		float ShaderPitch = 1.0f;
		SoundShaderContext Context = {false, true, &SourceAccess, IsSpatialVoice ? &Space : nullptr, &ShaderPitch};
		if (UseDefaultDoppler && Slot.DefaultDoppler)
		{
			Slot.DefaultDoppler->Process(process_buffer, SND_BLOCKSIZE, Context);
		}
		Slot.SourceShaders.Process(process_buffer, SND_BLOCKSIZE, Context);
		Slot.Shaders.Process(process_buffer, SND_BLOCKSIZE, Context);
		Slot.doppler = ShaderPitch;
	}
	else
	{
		Slot.doppler = 1.0f;
	}

	const Fvector& Pos = Slot.parameters[(u32)Mixer::ParameterId::Position];
	const Fvector& Volume = Slot.parameters[(u32)Mixer::ParameterId::VolumePerChannel];
	float BeginFactor = 1.0f, EndFactor = 1.0f;

	// Deferred stopping
	bool IsIntro = (Slot.flags & (u16)Mixer::Flags::Intro);
	if ((Slot.flags & (u16)Mixer::Flags::NoOCC) == 0 || OCCResult != ESlotOcclusionResult::SOM)
	{
		OCCVolume = 1.f;
	}

	Slot.fade_volume = 1.f;

	if (Slot.stopping_position != (u32)-1)
	{
		const u32 stopping_end = std::min(Slot.stopping_end, Source.Desc.frames_total);
		const u32 stopping_total = stopping_end > Slot.stopping_position ? stopping_end - Slot.stopping_position : 1;
		if (stopping_total > 1 && Slot.position >= Slot.stopping_position)
		{
			u32 begin_offset = Slot.position - Slot.stopping_position;
			u32 write_count = IsIntro ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * GMixer.time_factor);
			u32 end_offset = std::min(begin_offset + write_count, stopping_total - 1);
			BeginFactor = 1.0f - ((float)begin_offset / (float)(stopping_total - 1));
			EndFactor = 1.0f - ((float)end_offset / (float)(stopping_total - 1));
			BeginFactor = std::clamp(BeginFactor, 0.0f, 1.0f);
			EndFactor = std::clamp(EndFactor, 0.0f, 1.0f);
		}
	}

	float LeftPanning = Slot.parameters[(u32)Mixer::ParameterId::Panning].x;
	float RightPanning = Slot.parameters[(u32)Mixer::ParameterId::Panning].y;

	SoundBus* Bus = Snd_GetBus(Slot.bus);
	if (Bus == nullptr)
	{
		Slot.bus = Snd_GetMasterBus();
		Bus = Snd_GetBus(Slot.bus);
	}

	bool IsSpatial = (Slot.flags & (u32)Mixer::Flags::Spatial) != 0;
	bool IsMono = !IsIntro && Source.Desc.channels_count == 1;
	float Gain = Volume.x * Volume.y;
	if (IsMono && IsSpatial)
	{
		PROF_EVENT("Slot Spatial");
		SoundEffectVoice Voice = {Pos, Slot.velocity, Slot.parameters[(u32)Mixer::ParameterId::DistanceRange], Gain};
		SoundEffectProcess Process = {process_buffer, nullptr, &GMixer.listener, &Voice, true};
		Snd_ProcessVoiceEffects(Slot, Bus, &Process);
		Gain = Voice.Gain;
	}

	float VolumeFinal = OCCVolume * std::min(Gain, 1.0f) * Volume.z * Slot.fade_volume;
	const bool UseCarFactor = IsSpatial && (Slot.flags & (u16)Mixer::Flags::NoCarFactor) == 0;
	if (UseCarFactor)
	{
		VolumeFinal *= psSoundCarFactor;
	}
	BeginFactor *= VolumeFinal;
	EndFactor *= VolumeFinal;

	float BusGain = Bus->Volume * Bus->UserVolume;
	if (Bus->DirectRatio > 0.0f)
	{
		Snd_DirectSend(Slot, Bus, process_buffer, BusGain, BeginFactor, EndFactor, LeftPanning, RightPanning);
	}

	u32 ZoneBus = (IsMono && psSoundFlags.is(ss_EFX)) ? Snd_GetZoneBus(Slot.zone_idx) : 0;
	float ZoneGain = ZoneBus != 0 ? Bus->ZoneSend * std::clamp(Slot.IndoorFactor, 0.0f, 1.0f) * BusGain : 0.0f;
	if (ZoneBus != Slot.SendBus)
	{
		Snd_SendToBus(Slot.SendBus, process_buffer, BeginFactor * Slot.SendGain, 0.0f, LeftPanning, RightPanning, Slot.flags);
		Slot.SendBus = ZoneBus;
		Slot.SendGain = 0.0f;
	}

	Snd_SendToBus(ZoneBus, process_buffer, BeginFactor * Slot.SendGain, EndFactor * ZoneGain, LeftPanning, RightPanning, Slot.flags);
	Slot.SendGain = ZoneGain;

	const u32 MainBus = (Slot.RouteBus != 0 && Snd_GetBus(Slot.RouteBus) != nullptr) ? Slot.RouteBus : Slot.bus;
	Snd_SendToBus(MainBus, process_buffer, BeginFactor, EndFactor, LeftPanning, RightPanning, Slot.flags);
}

void Snd_MixerRenderCallback(float* buffer)
{
	PROF_EVENT("Sound: Render Stage");

	xrSRWLockGuard Guard(GMixer.render_lock, true);

	g_SoundStats.render_cache_miss = 0;

	static u64 TimeStamp = Snd_GetTimestamp();
	float dt = (float)((double)(Snd_GetTimestamp() - TimeStamp) / 1000000000.0);
	TimeStamp = Snd_GetTimestamp();

	static float _process_buffer[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};

	float* process_buffer[SND_CHANNEL_COUNT] = {};
	for (size_t i = 0; i < SND_CHANNEL_COUNT; i++)
	{
		process_buffer[i] = _process_buffer[i];
	}

	Snd_BeginBuses();

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

	{
		PROF_EVENT("Sound Mixing");
		Snd_RenderBuses(buffer, GMixer.mute_volume);
	}

#ifdef DEBUG_DRAW
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

	Snd_InitEffects();
	Snd_LoadConfig();
	Snd_InitBuses();
	GMixer.compression = -1.0f;
	GMixer.shooting_reverb = -1.0f;
	GMixer.default_bus = Snd_FindBus("effects");
	GMixer.default_bus = GMixer.default_bus != 0 ? GMixer.default_bus : Snd_GetMasterBus();

	Snd_InitSources();

	Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void Mixer::Shutdown()
{
	Backend::Shutdown();

	for (sound_slot_state& Slot : GMixer.slots)
	{
		Snd_ReleaseVoiceEffects(Slot, SoundEffectOp::Destroy);
		Slot.SourceShaders.Clear();
		Slot.Shaders.Clear();
	}

	Snd_ShutdownBuses();
	Snd_ShutdownHrtf();
	Snd_ShutdownSources();
	Snd_UnloadConfig();

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
	GMixer.slots[slot - 1].stopping_end = (u32)-1;
	GMixer.slots[slot - 1].flags = 0;
	Snd_ReleaseVoiceEffects(GMixer.slots[slot - 1], SoundEffectOp::Destroy);
	GMixer.slots[slot - 1].SourceShaders.Clear();
	GMixer.slots[slot - 1].Shaders.Clear();
	GMixer.slots[slot - 1].bus = 0;
	GMixer.slots[slot - 1].SendBus = 0;
	GMixer.slots[slot - 1].SendGain = 0.0f;
	GMixer.slots[slot - 1].RouteBus = 0;
	GMixer.slots[slot - 1].state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].prev_state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].fake_state = Mixer::State::Stopped;
	GMixer.slots[slot - 1].fade_volume = 1.0f;
	GMixer.free_slots.push_back(slot);
}

void Mixer::Update(void* event_handler, float time_factor, float mute_volume, float compression, Fvector P, Fvector D, Fvector N)
{
	PROF_EVENT("Sound: Update Stage");
	sound_event* Handler = (sound_event*)event_handler;

	static u64 TimeStamp = Snd_GetTimestamp();
	// Nanoseconds to seconds without rounding to milliseconds: velocities divide by it
	const u64 Now = Snd_GetTimestamp();
	GMixer.dt = (float)((double)(Now - TimeStamp) * 1e-9);
	TimeStamp = Now;

	GMixer.time_factor = std::clamp(time_factor, 0.1f, 10.0f);
	GMixer.mute_volume = mute_volume;

	GMixer.render_lock.AcquireExclusive();
	GMixer.manage_lock.AcquireExclusive();
	GMixer.update_lock.AcquireExclusive();

	Snd_UpdateVelocity(GMixer.listener.Velocity, GMixer.listener.Position, P);
	GMixer.listener.Position = P;
	GMixer.listener.Direction = D;
	GMixer.listener.Normal = N;

	if (GMixer.compression != compression)
	{
		GMixer.compression = compression;
		Snd_SetBusParam(Snd_GetMasterBus(), "compressor", "mix", compression);
	}

	SoundBus* ShootingBus = Snd_GetBus(Snd_FindBus("shooting"));
	if (ShootingBus != nullptr && GMixer.shooting_reverb != psSoundShootingReverb)
	{
		GMixer.shooting_reverb = psSoundShootingReverb;
		ShootingBus->DirectRatio = psSoundShootingReverb;
	}

	// Listener hemi -> indoor factor (kept for reference / global use).
	GMixer.IndoorFactor += (Snd_HemiIndoorFactor(GMixer.listener.Position) - GMixer.IndoorFactor) *
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
				Snd_UpdateVelocity(Slot.velocity, Slot.prev_position, Pos);
				Slot.prev_position = Pos;
			}

			bool IsOCCEnabled = ((Slot.flags & ((u16)Flags::Intro | (u16)Flags::NoOCC)) == 0) && Slot.state == State::Playing;
			if (IsOCCEnabled)
			{
				Fvector Pos = (Slot.flags & (u16)Flags::Spatial) ? Slot.parameters[(u32)Mixer::ParameterId::Position] : GMixer.listener.Position;
				float Dist = GMixer.listener.Position.distance_to(Pos);

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

					Slot.zone_idx = Snd_FindZone(Pos);
				}
			}
		}
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	xr_vector<ref_sound_data_ptr> PendingEvents;
	// A play whose source is still loading stays queued, along with the later commands of its slot
	size_t Deferred = 0;
	for (size_t CommandIdx = 0; CommandIdx < GMixer.cmd.size(); CommandIdx++)
	{
		const SoundCommand& Command = GMixer.cmd[CommandIdx];
		bool IsDeferred = false;
		for (size_t DeferredIdx = 0; DeferredIdx < Deferred && !IsDeferred; DeferredIdx++)
		{
			IsDeferred = Command.slot != 0 && GMixer.cmd[DeferredIdx].slot == Command.slot;
		}

		if (IsDeferred)
		{
			if (Deferred != CommandIdx)
			{
				GMixer.cmd[Deferred] = Command;
			}

			Deferred++;
			continue;
		}

		switch (Command.id)
		{
			case ESoundMixerCommands::play:
			{
				bool IsSoundExists = (Command.param1 && GMixer.sounds.contains((ref_sound*)Command.param1));
				ref_sound* RefSound = IsSoundExists ? (ref_sound*)Command.param1 : nullptr;
				u16 Flags = Command.param0;
				// Overlapping playback passes its emitter in the command, without changing the reusable sound reference.
				CObject* PlaybackObject = (Flags & (u16)Flags::NoFeedback) ?
					reinterpret_cast<CObject*>(Command.param3) : (RefSound ? RefSound->_g_object() : nullptr);

				auto& ActualSlot = GMixer.slots[Command.slot - 1];
				const xr_string NewName = Command.string_storage.size() ? Command.string_storage.c_str() : "";
				SoundSourceState* SourcePtr = Snd_FindSource(&NewName);
				if (SourcePtr == nullptr && !NewName.empty())
				{
					Snd_QueueDecode(&NewName, 0, true);
					if (Deferred != CommandIdx)
					{
						GMixer.cmd[Deferred] = Command;
					}

					Deferred++;
					break;
				}

				Snd_ReleaseVoiceEffects(ActualSlot, SoundEffectOp::Destroy);
				bool IsSameFile = ActualSlot.sound_name == NewName;
				if (!IsSameFile && !ActualSlot.sound_name.empty())
				{
					Snd_ReleaseSource(&ActualSlot.sound_name);
					ActualSlot.sound_name.clear();
				}

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
				ActualSlot.stopping_end = (u32)-1;
				ActualSlot.sound_name = NewName;
				ActualSlot.flags = Flags;
				ActualSlot.bus = Snd_GetBus(Command.bus) != nullptr ? Command.bus : Snd_GetBus(Source.Desc.bus) != nullptr ? Source.Desc.bus : GMixer.default_bus;
				ActualSlot.SendBus = 0;
				ActualSlot.SendGain = 0.0f;
				ActualSlot.RouteBus = Snd_GetBus(Command.route) != nullptr ? Command.route : 0;
				ActualSlot.ShaderPrefetch = (u32)-1;
				ActualSlot.fade_volume = 0.0f;
				ActualSlot.ReverbDryGain = -1.0f;
				Snd_SyncShaderChain(ActualSlot.SourceShaders, Source.Desc.shaders);
				ActualSlot.SourceShaders.Reset();
				ActualSlot.Shaders.Reset();
				ActualSlot.IndoorFactor = GMixer.IndoorFactor;
				ActualSlot.IndoorFactorValid = false;
				ActualSlot.zone_idx = 0;

				// Start decoding the first cache line immediately (off the audio thread) so the sound is ready by the time it is rendered.
				Snd_QueueDecode(&ActualSlot.sound_name, 0, false);

				if (ActualSlot.flags & (u16)Flags::Spatial && RefSound != nullptr && PlaybackObject != nullptr)
				{
					ActualSlot.parameters[(u32)Mixer::ParameterId::Position] = ((IRenderable*)PlaybackObject)->renderable.xform.c;
				}

				ActualSlot.doppler = 1.0f;
				ActualSlot.LastRate = 0.0f;
				if (ActualSlot.DefaultDoppler)
				{
					ActualSlot.DefaultDoppler->Reset();
				}
				ActualSlot.velocity.set(0.0f, 0.0f, 0.0f);
				ActualSlot.prev_position = ActualSlot.parameters[(u32)Mixer::ParameterId::Position];

				if (Handler && RefSound && RefSound->_p && PlaybackObject)
				{
					int GameType = RefSound->_p->g_type;
					if (GameType == int(sg_SourceType))
					{
						GameType = int(Source.Desc.game_type);
						RefSound->_p->g_type = GameType;
					}
					// Preserve each playback's emitter and type until all queued parameters are applied.
					ref_sound_data_ptr Data = new ref_sound_data();
					Data->slot = Command.slot;
					Data->g_type = GameType;
					Data->g_object = PlaybackObject;
					Data->g_userdata = RefSound->_p->g_userdata;
					Data->dont_destroy_slot = true;
					Data->fn_attached[0] = Source.Desc.path;
					PendingEvents.push_back(Data);
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
					auto& Slot = GMixer.slots[Command.slot - 1];
					Slot.flags &= ~((u8)Flags::Looped);
					// Without an explicit fade keep the legacy behavior: fade over
					// the remaining file. Car sounds request a short fade instead.
					const double Fade = *(double*)&Command.param2;
					Slot.stopping_position = Slot.position;
					Slot.stopping_end = Fade > 0.0 ? Slot.position + (u32)(Fade * (double)SND_SAMPLERATE) : (u32)-1;
				}
				else
				{
					MixerNewState(Command.slot, State::Stopped);
					GMixer.slots[Command.slot - 1].position = 0;
					GMixer.slots[Command.slot - 1].stopping_position = (u32)-1;
					GMixer.slots[Command.slot - 1].stopping_end = (u32)-1;
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
				size_t Pending = Deferred;
				Deferred = 0;
				for (size_t DeferredIdx = 0; DeferredIdx < Pending; DeferredIdx++)
				{
					const SoundCommand& Queued = GMixer.cmd[DeferredIdx];
					if (Queued.id != ESoundMixerCommands::play)
					{
						if (Deferred != DeferredIdx)
						{
							GMixer.cmd[Deferred] = Queued;
						}

						Deferred++;
					}
					else if (Queued.param0 & (u16)Flags::NoFeedback)
					{
						DestroyInternal(Queued.slot);
					}
					else
					{
						MixerNewState(Queued.slot, State::Stopped);
					}
				}

				for (size_t Iter = 0; Iter < GMixer.slots.size(); Iter++)
				{
					if (GMixer.slots[Iter].sound_name.size() && GMixer.slots[Iter].state != State::Stopped)
					{
						GMixer.slots[Iter].position = 0;
						GMixer.slots[Iter].stopping_position = (u32)-1;
						GMixer.slots[Iter].stopping_end = (u32)-1;
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
				if (MixerSlot.flags & (u16)Flags::Spatial && (MixerSlot.flags & (u16)Flags::NoOCC) == 0 && Command.param0 == (u16)ParameterId::Position)
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
			case ESoundMixerCommands::slot_shader:
			{
				Snd_ApplyShaderCommand(GMixer.slots[Command.slot - 1].Shaders, Command);
			}
			break;
			case ESoundMixerCommands::bus_shader:
			{
				if (SoundShaderChain* Chain = Snd_GetBusShaders(Command.slot))
				{
					Snd_ApplyShaderCommand(*Chain, Command);
				}
			}
			break;
			case ESoundMixerCommands::slot_route:
			{
				const u32 Route = (u32)Command.param1;
				GMixer.slots[Command.slot - 1].RouteBus = Snd_GetBus(Route) != nullptr ? Route : 0;
			}
			break;
			case ESoundMixerCommands::bus_route:
			{
				Snd_RouteBus(Command.slot, (u32)Command.param1);
			}
			break;
			case ESoundMixerCommands::bus_release:
			{
				if (Snd_ReleaseDynamicBus(Command.slot))
				{
					for (sound_slot_state& Slot : GMixer.slots)
					{
						Slot.RouteBus = Slot.RouteBus == Command.slot ? 0 : Slot.RouteBus;
					}
				}
			}
			break;
		}
	}

	// PlayNoFeedback queues position/range/volume after play. AI must see their final values.
	for (const auto& Event : PendingEvents)
	{
		const auto& Slot = GMixer.slots[Event->slot - 1];
		if (Slot.state != State::Playing && Slot.state != State::Delay) continue;
		const CSound_params Params = Event->get_params();
		const float Range = std::min(Params.max_ai_distance, Params.max_ai_distance * Params.volume);
		if (Range >= .1f) Handler(Event, Range);
	}
	GMixer.cmd.resize(Deferred);
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
	// A queued play can wait several frames for its source and outlive its emitter
	{
		xrCriticalSectionGuard Guard(GMixer.play_lock);
		for (SoundCommand& Command : GMixer.cmd)
		{
			for (int Iter = 0; Iter < count && Command.id == ESoundMixerCommands::play; Iter++)
			{
				if (Command.param3 == (u64)object[Iter])
				{
					Command.param3 = 0;
				}
			}
		}
	}

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

void Mixer::Play(u32 SlotID, u16 flags, ref_sound* SoundRef, double Delay, u32 Bus, u32 Route)
{
	xrCriticalSectionGuard Guard(GMixer.play_lock);

	if (SlotID == 0 || SoundRef == nullptr || SoundRef->_p == nullptr || SoundRef->_p->fn_attached[0] == nullptr)
	{
		return;
	}

	GMixer.slots[SlotID - 1].fake_state = State::Playing;

	GMixer.cmd.emplace_back(SoundCommand{
		.slot = SlotID, .id = ESoundMixerCommands::play, .param0 = flags, .param1 = (u64)SoundRef, .param2 = *(u64*)&Delay, .param3 = (u64)SoundRef->_g_object(), .string_storage = SoundRef->_p->fn_attached[0], .bus = Bus, .route = Route
	});
}

void Mixer::PlayNoFeedback(u16 Flags, ref_sound* SoundRef, CObject* Obj, double Delay, float* Pitch, float* Volume, Fvector* Distance, Fvector* Pos, u32 Bus, u32 Route)
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
		.slot = SlotIdx, .id = ESoundMixerCommands::play, .param0 = Flags, .param1 = (u64)SoundRef, .param2 = *(u64*)&Delay, .param3 = (u64)Obj, .string_storage = SoundRef->_p->fn_attached[0], .bus = Bus, .route = Route
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

	// Overlapping copies sound like the reference they were started from
	if (SoundRef->slot())
	{
		Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, SlotIdx, SoundShaderType::None, ESoundShaderOp::Copy, 0.0, nullptr, SoundRef->slot());
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
	Stop(SlotID, IsDeferred, -1.0f);
}

void Mixer::Stop(u32 SlotID, bool IsDeferred, float FadeSeconds)
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

	const double Fade = FadeSeconds;
	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = SlotID, .id = ESoundMixerCommands::stop, .param0 = IsDeferred, .param2 = *(u64*)&Fade});
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

float* Mixer::GetMasterVolume()
{
	return GetBusVolume("master");
}

float* Mixer::GetBusVolume(const char* Name)
{
	u32 BusIdx = Snd_ReserveBus(Name);
	R_ASSERT(BusIdx != 0);
	return &Snd_GetBuses()[BusIdx - 1].UserVolume;
}

u32 Mixer::FindBus(const char* Name)
{
	return Snd_FindBus(Name);
}

const char* Mixer::GetBusName(u32 Bus)
{
	const SoundBus* Data = Snd_GetBus(Bus);
	return Data != nullptr ? Data->Name.c_str() : "";
}

void Mixer::GetDynamicBuses(xr_vector<SoundDynamicBusInfo>& Buses)
{
	// Acquire and the queued release write it under the update lock
	xrSRWLockGuard Guard(GMixer.update_lock, true);
	Snd_GetDynamicBuses(Buses);
}

void Mixer::ForgetDynamicBuses()
{
	xrSRWLockGuard RenderGuard(GMixer.render_lock);
	xrSRWLockGuard UpdateGuard(GMixer.update_lock);
	Snd_ForgetDynamicBuses();
}

bool Mixer::GetBusSpectrum(u32 Bus, u32 Channel, float* BandsDb, u32 BandCount, float MinHz, float MaxHz, u32 FftSize)
{
	float Samples[SoundSpectrumAnalyzer::MaxFftSize];
	FftSize = std::clamp(FftSize, SoundSpectrumAnalyzer::MinFftSize, SoundSpectrumAnalyzer::MaxFftSize);
	if (BandsDb == nullptr || BandCount == 0 || !Snd_ReadBusScope(Bus, Channel, Samples, FftSize))
	{
		return false;
	}

	static xrCriticalSection Lock;
	static SoundSpectrumAnalyzer Analyzer;
	xrCriticalSectionGuard Guard(Lock);
	return Analyzer.Analyze(Samples, FftSize, BandsDb, BandCount, MinHz, MaxHz);
}

u32 Mixer::RegisterEffect(const char* Name, SoundEffectProc Proc)
{
	return Snd_RegisterEffect(Name, Proc);
}

SoundBus* Mixer::GetBuses()
{
	return Snd_GetBuses();
}

const SoundEffectEntry* Mixer::GetEffect(u8 Id)
{
	return Snd_GetEffect(Id);
}

u32 Mixer::GetEffectCount()
{
	return Snd_GetEffectCount();
}

u32 Mixer::CreateBus(const char* Name)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	return Snd_CreateBus(Name);
}

void Mixer::DeleteBus(u32 Bus)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_DeleteBus(Bus);
}

void Mixer::SetBusValue(u32 Bus, const char* Key, const char* Value)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_SetBusValue(Bus, Key, Value);
	GMixer.shooting_reverb = -1.0f;
	GMixer.compression = -1.0f;
}

void Mixer::SetBusEffectParam(u32 Bus, bool IsVoice, u32 Effect, u32 Param, float Value)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_SetBusEffectParam(Bus, IsVoice, Effect, Param, Value);
}

const char* Mixer::GetBusValue(u32 Bus, const char* Key)
{
	return Snd_GetBusValue(Bus, Key);
}

bool Mixer::SaveBus(u32 Bus)
{
	return Snd_SaveBus(Bus);
}

void Mixer::AddZone(sound_zone_params& Params)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_AddZone(&Params, false);
}

void Mixer::AddEditorZone(sound_zone_params& Params)
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_AddZone(&Params, true);
}

void Mixer::ResetZones()
{
	xrSRWLockGuard Guard(GMixer.render_lock);
	Snd_ResetZones();
}

void Mixer::ExportConfig()
{
	Snd_ExportConfig();
}

void Mixer::ReloadSources()
{
	// Voices keep playing: looped ambients continue with the new data
	xrSRWLockGuard RenderGuard(GMixer.render_lock);
	xrSRWLockGuard UpdateGuard(GMixer.update_lock);
	Snd_ReloadSources();

	xrSRWLockGuard SourceGuard(g_SoundSourceLock, true);
	for (sound_slot_state& Slot : GMixer.slots)
	{
		if (Slot.state == State::Stopped)
		{
			continue;
		}

		const SoundSourceState* Source = Snd_LookupSource(&Slot.sound_name);
		if (Source == nullptr)
		{
			continue;
		}

		// The new file may be shorter than the old one
		const u32 LastFrame = Source->Desc.frames_total > 0 ? Source->Desc.frames_total - 1 : 0;
		Slot.position = std::min(Slot.position, LastFrame);
		if (Slot.stopping_position != (u32)-1)
		{
			Slot.stopping_position = std::min(Slot.stopping_position, LastFrame);
		}

		Slot.parameters[(u32)ParameterId::VolumePerChannel].x = Source->Desc.volume;
		Slot.parameters[(u32)ParameterId::DistanceRange] = Fvector(Source->Desc.min_distance, Source->Desc.max_distance, Source->Desc.max_ai_distance);
		Snd_SyncShaderChain(Slot.SourceShaders, Source->Desc.shaders);
	}
}

bool Mixer::GetSoundConfig(const char* Name, sound_config* Config)
{
	return Snd_GetSoundConfig(Name, Config);
}

void Mixer::SetSoundConfig(const char* Name, sound_config* Config)
{
	xrSRWLockGuard RenderGuard(GMixer.render_lock);
	xrSRWLockGuard UpdateGuard(GMixer.update_lock);
	xrSRWLockGuard SourceGuard(g_SoundSourceLock, true);
	SoundSourceState* Source = Snd_SetSoundConfig(Name, Config);
	if (Source == nullptr)
	{
		return;
	}

	for (sound_slot_state& Slot : GMixer.slots)
	{
		if (Slot.state == State::Stopped || Snd_LookupSource(&Slot.sound_name) != Source)
		{
			continue;
		}

		Slot.parameters[(u32)ParameterId::VolumePerChannel].x = Source->Desc.volume;
		Slot.parameters[(u32)ParameterId::DistanceRange] = Fvector(Source->Desc.min_distance, Source->Desc.max_distance, Source->Desc.max_ai_distance);
		Snd_SyncShaderChain(Slot.SourceShaders, Source->Desc.shaders);
	}
}

bool Mixer::SaveSoundConfig(const char* Name)
{
	return Snd_SaveConfig(Name);
}

void Mixer::GetLoadedSources(xr_vector<shared_str>& Names)
{
	Snd_GetLoadedSources(Names);
}

const xr_vector<sound_zone_params>& Mixer::GetZones()
{
	return Snd_GetZones();
}

void Mixer::AddShader(u32 Slot, SoundShaderType Type)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::Add);
}

void Mixer::RemoveShader(u32 Slot, SoundShaderType Type)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::Remove);
}

void Mixer::ClearShaders(u32 Slot)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, SoundShaderType::None, ESoundShaderOp::Clear);
}

void Mixer::SetShaderParam(u32 Slot, SoundShaderType Type, const char* Param, float Value)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::SetParam, Value, Param);
}

void Mixer::AddBusShader(u32 Bus, SoundShaderType Type, bool SpatialOnly)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::Add, 0.0, nullptr, SpatialOnly ? 1 : 0);
}

void Mixer::RemoveBusShader(u32 Bus, SoundShaderType Type, bool SpatialOnly)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::Remove, 0.0, nullptr, SpatialOnly ? 1 : 0);
}

void Mixer::ClearBusShaders(u32 Bus)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, SoundShaderType::None, ESoundShaderOp::Clear);
}

void Mixer::SetBusShaderParam(u32 Bus, SoundShaderType Type, const char* Param, float Value)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::SetParam, Value, Param);
}

void Mixer::RampShaderParam(u32 Slot, SoundShaderType Type, const char* Param, float Value, float Seconds, Shader::Curve Curve)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::Ramp, Value, Param, Snd_PackFloat(Seconds, (u8)Curve));
}

void Mixer::SetShaderLfo(u32 Slot, SoundShaderType Type, const char* Param, float RateHz, float Depth, Shader::LfoShape Shape)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::Lfo, RateHz, Param, Snd_PackFloat(Depth, (u8)Shape));
}

void Mixer::StopShaderAutomation(u32 Slot, SoundShaderType Type, const char* Param)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::slot_shader, Slot, Type, ESoundShaderOp::StopAutomation, 0.0, Param != nullptr ? Param : "");
}

void Mixer::RampBusShaderParam(u32 Bus, SoundShaderType Type, const char* Param, float Value, float Seconds, Shader::Curve Curve)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::Ramp, Value, Param, Snd_PackFloat(Seconds, (u8)Curve));
}

void Mixer::SetBusShaderLfo(u32 Bus, SoundShaderType Type, const char* Param, float RateHz, float Depth, Shader::LfoShape Shape)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::Lfo, RateHz, Param, Snd_PackFloat(Depth, (u8)Shape));
}

void Mixer::StopBusShaderAutomation(u32 Bus, SoundShaderType Type, const char* Param)
{
	Snd_QueueShaderCommand(ESoundMixerCommands::bus_shader, Bus, Type, ESoundShaderOp::StopAutomation, 0.0, Param != nullptr ? Param : "");
}

const SoundShaderChain* Mixer::GetBusShaders(u32 Bus)
{
	return Snd_GetBusShaders(Bus);
}

u32 Mixer::AcquireDynamicBus(const char* Name, u32 Output)
{
	xrSRWLockGuard RenderGuard(GMixer.render_lock);
	xrSRWLockGuard UpdateGuard(GMixer.update_lock);
	return Snd_AcquireDynamicBus(Name, Output);
}

u32 Mixer::AcquireShaderBus(SoundShaderType Type, u32 Output)
{
	const Shader::ClassInfo* Info = Shader::GetClass(Type);
	if (Info == nullptr)
	{
		return 0;
	}

	string128 Name;
	xr_strconcat(Name, "fx:", Info->Name);

	xrSRWLockGuard RenderGuard(GMixer.render_lock);
	xrSRWLockGuard UpdateGuard(GMixer.update_lock);
	const u32 BusIdx = Snd_AcquireDynamicBus(Name, Output);
	const SoundBus* Bus = Snd_GetBus(BusIdx);
	SoundShaderChain* Chain = Snd_GetBusShaders(BusIdx);
	if (Bus != nullptr && Bus->IsDynamic && Chain != nullptr && Chain->Find(Type) == nullptr)
	{
		Chain->Add(Type);
	}

	return BusIdx;
}

void Mixer::ReleaseDynamicBus(u32 Bus)
{
	if (Bus == 0)
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = Bus, .id = ESoundMixerCommands::bus_release});
}

void Mixer::RouteBus(u32 Bus, u32 Target)
{
	if (Bus == 0)
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = Bus, .id = ESoundMixerCommands::bus_route, .param1 = Target});
}

void Mixer::RouteVoice(u32 Slot, u32 Bus)
{
	if (Slot == 0)
	{
		return;
	}

	xrCriticalSectionGuard Guard(GMixer.play_lock);
	GMixer.cmd.emplace_back(SoundCommand{.slot = Slot, .id = ESoundMixerCommands::slot_route, .param1 = Bus});
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

float Mixer::GetDuration(const char* Name)
{
	if (Name == nullptr || *Name == '\0')
	{
		return 0.0f;
	}

	xr_string Key(Name);
	SoundSourceState* Source = Snd_AcquireSource(&Key);
	if (Source == nullptr)
	{
		return 0.0f;
	}

	const float Duration = (float)Source->Desc.frames_total / (float)SND_SAMPLERATE;
	Snd_ReleaseSource(&Key);
	return Duration;
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

ref_sound::ref_sound() : _unique_id(0)
{
	xrSRWLockGuard Guard1(GMixer.manage_lock);

	if (!GMixer.sounds.contains(this))
	{
		GMixer.sounds.emplace(this);
	}
}

// Sound pools copy references during insertion and reallocation; every live address must be registered.
ref_sound::ref_sound(const ref_sound& Other) : ref_sound()
{
	_unique_id = Other._unique_id;
	_p = Other._p;
	TimeToPropagade = Other.TimeToPropagade;
}

ref_sound::~ref_sound()
{
	xrSRWLockGuard Guard1(GMixer.manage_lock);

	if (GMixer.sounds.contains(this))
	{
		GMixer.sounds.erase(this);
	}
}
