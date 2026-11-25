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
#include "SoundEffect.h"
#include "SoundDSP.h"
#include "SoundHrtf.h"
#include "SoundRoomReverb.h"

#define SND_PANNING_SMOOTH (10.0f)

struct SoundEffectRegistry
{
	SoundEffectEntry Entries[SND_EFFECT_COUNT];
	u32 Count = 0;
};

enum SoundSpatialParam : u32
{
	SpatialRolloff,
	SpatialBackAttenuation
};

static const SoundEffectParam SpatialParams[] =
{
	{"rolloff", 1.3f, 0.1f, 4.0f},
	{"back_attenuation", 0.3f, 0.0f, 1.0f}
};

static SoundEffectRegistry GEffects = {};

void Snd_InitEffects()
{
	Snd_RegisterEffect("spatial", Snd_SpatialProc);
	Snd_RegisterEffect("convolution", Snd_ConvolutionProc);
	// Shaders are bus effects too: "compressor" and the rest come from here
	Snd_RegisterShaderEffects();
	Snd_RegisterEffect("spatialize", Snd_HrtfProc);
	Snd_RegisterEffect("reverb", Snd_RoomReverbProc);
}

u8 Snd_FindEffect(const char* Name)
{
	for (u32 EffectIdx = 0; EffectIdx < GEffects.Count; EffectIdx++)
	{
		if (xr_strcmp(GEffects.Entries[EffectIdx].Name.c_str(), Name) == 0)
		{
			return (u8)(EffectIdx + 1);
		}
	}

	return 0;
}

u8 Snd_RegisterEffect(const char* Name, SoundEffectProc Proc)
{
	u8 Id = Snd_FindEffect(Name);
	if (Id != 0)
	{
		return Id;
	}

	if (GEffects.Count >= SND_EFFECT_COUNT || Proc == nullptr)
	{
		Msg("! [Sound] Can't register effect '%s'", Name);
		return 0;
	}

	SoundEffectEntry& Entry = GEffects.Entries[GEffects.Count];
	Entry.Desc = {};
	if (!Proc(nullptr, SoundEffectOp::Describe, &Entry.Desc) || Entry.Desc.ParamCount > SND_EFFECT_PARAM_COUNT || (Entry.Desc.Scope == SoundEffectScope::Voice && Entry.Desc.StateSize > SND_VOICE_EFFECT_STATE_SIZE))
	{
		Msg("! [Sound] Invalid effect description '%s'", Name);
		return 0;
	}

	Entry.Name = Name;
	Entry.Proc = Proc;
	return (u8)++GEffects.Count;
}

const SoundEffectEntry* Snd_GetEffect(u8 Id)
{
	return (Id == 0 || Id > GEffects.Count) ? nullptr : &GEffects.Entries[Id - 1];
}

u32 Snd_GetEffectCount()
{
	return GEffects.Count;
}

bool Snd_CallEffect(u8 Id, void* State, SoundEffectOp Op, void* Arg)
{
	const SoundEffectEntry* Entry = Snd_GetEffect(Id);
	return Entry != nullptr && State != nullptr && Entry->Proc(State, Op, Arg);
}

void Snd_ResetEffect(SoundBusEffect* Effect, u8 Id)
{
	const SoundEffectEntry* Entry = Snd_GetEffect(Id);
	Effect->Effect = Entry != nullptr ? Id : 0;
	Effect->State = nullptr;
	Effect->Resource = nullptr;
	memset(Effect->Params, 0, sizeof(Effect->Params));
	for (u32 ParamIdx = 0; Entry != nullptr && ParamIdx < Entry->Desc.ParamCount; ParamIdx++)
	{
		Effect->Params[ParamIdx] = Entry->Desc.Params[ParamIdx].Default;
	}
}

bool Snd_SetEffectParam(SoundBusEffect* Effect, const char* Name, float Value)
{
	const SoundEffectEntry* Entry = Snd_GetEffect(Effect->Effect);
	for (u32 ParamIdx = 0; Entry != nullptr && ParamIdx < Entry->Desc.ParamCount; ParamIdx++)
	{
		const SoundEffectParam& Param = Entry->Desc.Params[ParamIdx];
		if (xr_strcmp(Param.Name, Name) == 0)
		{
			Effect->Params[ParamIdx] = std::clamp(Value, Param.Min, Param.Max);
			Snd_CallEffect(Effect->Effect, Effect->State, SoundEffectOp::Update, Effect->Params);
			return true;
		}
	}

	return false;
}

bool Snd_CreateEffect(SoundBusEffect* Effect)
{
	const SoundEffectEntry* Entry = Snd_GetEffect(Effect->Effect);
	if (Entry == nullptr || Effect->State != nullptr || Entry->Desc.Scope != SoundEffectScope::Bus)
	{
		return false;
	}

	Effect->State = xr_alloc<u8>(std::max(Entry->Desc.StateSize, 1u));
	memset(Effect->State, 0, std::max(Entry->Desc.StateSize, 1u));

	SoundEffectCreate Create = {Effect->Params, Effect->Resource.c_str()};
	if (!Entry->Proc(Effect->State, SoundEffectOp::Create, &Create))
	{
		Msg("! [Sound] Can't create effect '%s'", Entry->Name.c_str());
		xr_free(Effect->State);
		return false;
	}

	return true;
}

void Snd_DestroyEffect(SoundBusEffect* Effect)
{
	if (Effect->State == nullptr)
	{
		return;
	}

	Snd_CallEffect(Effect->Effect, Effect->State, SoundEffectOp::Destroy, nullptr);
	xr_free(Effect->State);
}

void Snd_SpatialLocate(SoundEffectProcess* Process, SoundSpatialPosition* OutPosition)
{
	const SoundEffectListener* Listener = Process->Listener;
	SoundEffectVoice* Voice = Process->Voice;

	Fvector Direction;
	Direction.sub(Voice->Position, Listener->Position);
	float Distance = Direction.magnitude();

	Fmatrix Camera;
	Camera.build_camera_dir(Listener->Position, Listener->Direction, Listener->Normal);
	Camera.transform_tiny_noadd(OutPosition->Local, Direction);
	OutPosition->Local.normalize_safe();

	OutPosition->MinDistance = std::max(Voice->Distances.x, EPS_S);
	OutPosition->MaxDistance = std::max(Voice->Distances.y, OutPosition->MinDistance + EPS_S);
	OutPosition->Distance = std::clamp(Distance, OutPosition->MinDistance, OutPosition->MaxDistance);

	// The voice gain is consumed here so it is clamped after attenuation, not before
	OutPosition->Gain = Voice->Gain;
	Voice->Gain = 1.0f;
}

float Snd_DistanceAttenuation(const SoundSpatialPosition* Position, float Power)
{
	float Attenuation = powf(Position->MinDistance / (psSoundRolloff * Position->Distance), Power);
	Attenuation *= 1.0f - (Position->Distance - Position->MinDistance) / (Position->MaxDistance - Position->MinDistance);
	return std::clamp(std::min(Attenuation, 1.0f) * Position->Gain, 0.0f, 1.0f);
}

void Snd_SpatialPan(SoundSpatialState* State, SoundEffectProcess* Process, const SoundSpatialPosition* Position, float Rolloff, float BackAttenuation)
{
	float Attenuation = Snd_DistanceAttenuation(Position, Rolloff);
	float PanAngle = (std::clamp(Position->Local.x, -1.0f, 1.0f) + 1.0f) * PI_DIV_4;
	float BackGain = 1.0f - BackAttenuation * std::clamp(-Position->Local.z, 0.0f, 1.0f);
	float Blend = std::min(Position->Distance, 1.0f);
	float Target[SND_CHANNEL_COUNT] = {lerp(1.0f, cosf(PanAngle) * BackGain, Blend), lerp(1.0f, sinf(PanAngle) * BackGain, Blend)};

	if (!State->IsPanned)
	{
		State->Panning[0] = Target[0];
		State->Panning[1] = Target[1];
		State->IsPanned = true;
	}

	float SampleDt = 1.0f / (float)SND_SAMPLERATE;
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float* Data = Process->Data[Channel];
		for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
		{
			Data[Frame] *= Attenuation * State->Panning[Channel];
			volume_lerp(State->Panning[Channel], Target[Channel], SND_PANNING_SMOOTH, SampleDt);
		}
	}
}

bool Snd_SpatialProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	SoundSpatialState* State = (SoundSpatialState*)StatePtr;
	switch (Op)
	{
	case SoundEffectOp::Describe:
		*(SoundEffectDesc*)Arg = {SoundEffectScope::Voice, sizeof(SoundSpatialState), (u32)std::size(SpatialParams), SpatialParams};
		return true;
	case SoundEffectOp::Create:
		return true;
	case SoundEffectOp::Reset:
	case SoundEffectOp::Destroy:
		State->IsPanned = false;
		return true;
	case SoundEffectOp::Process:
	{
		SoundEffectProcess* Process = (SoundEffectProcess*)Arg;
		SoundSpatialPosition Position;
		Snd_SpatialLocate(Process, &Position);
		Snd_SpatialPan(State, Process, &Position, Process->Params[SpatialRolloff], Process->Params[SpatialBackAttenuation]);
		return true;
	}
	default:
		return false;
	}
}

