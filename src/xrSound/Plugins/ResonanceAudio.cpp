#include "stdafx.h"
#include "ResonanceAudio.h"
#include "../New/SoundEffect.h"
#include "../../3rd-party/resonance-audio/resonance_audio/api/resonance_audio_api.h"

#define RESONANCE_HRTF_COUNT (512)

enum ResonanceSpatializeParam : u32
{
	SpatializeRolloff,
	SpatializeFallbackRolloff,
	SpatializeBackAttenuation,
	SpatializeDoppler
};

enum ResonanceReverbParam : u32
{
	ReverbGain,
	ReverbSize,
	ReverbCutoff,
	ReverbDiffusion,
	ReverbReflections,
	ReverbDecay,
	ReverbDecayHf,
	ReverbAirHf
};

struct ResonanceInstance
{
	vraudio::ResonanceAudioApi* Api = nullptr;
	vraudio::ResonanceAudioApi::SourceId Source = vraudio::ResonanceAudioApi::kInvalidSourceId;
};

struct ResonanceState
{
	xr_vector<ResonanceInstance> Hrtfs;
	xr_vector<u32> FreeHrtfs;
};

struct ResonanceSpatializeState
{
	SoundSpatialState Pan;
	u32 Hrtf;
};

struct ResonanceReverbState
{
	ResonanceInstance Instance;
	u32 TailFrames;
	float Mono[SND_BLOCKSIZE];
};

static const SoundEffectParam SpatializeParams[] =
{
	{"rolloff", 2.0f, 0.1f, 4.0f},
	{"fallback_rolloff", 1.3f, 0.1f, 4.0f},
	{"back_attenuation", 0.3f, 0.0f, 1.0f},
	{"doppler", 1.0f, 0.0f, 10.0f}
};

static const SoundEffectParam ReverbParams[] =
{
	{"gain", 0.005f, 0.0f, 1.0f},
	{"size", 10.0f, 1.0f, 100.0f},
	{"cutoff", 5000.0f, 200.0f, 20000.0f},
	{"diffusion", 1.0f, 0.0f, 1.0f},
	{"reflections", 0.05f, 0.0f, 1.0f},
	{"decay", 1.49f, 0.1f, 20.0f},
	{"decay_hf", 0.83f, 0.1f, 2.0f},
	{"air_hf", 0.994f, 0.0f, 1.0f}
};

static ResonanceState GResonance = {};

static void Resonance_DestroyInstance(ResonanceInstance* Instance)
{
	if (Instance->Api == nullptr)
	{
		return;
	}

	if (Instance->Source != vraudio::ResonanceAudioApi::kInvalidSourceId)
	{
		Instance->Api->DestroySource(Instance->Source);
	}

	delete Instance->Api;
	Instance->Api = nullptr;
	Instance->Source = vraudio::ResonanceAudioApi::kInvalidSourceId;
}

static bool Resonance_CreateInstance(ResonanceInstance* Instance, vraudio::RenderingMode Mode, bool IsRoom)
{
	Instance->Api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	if (Instance->Api == nullptr)
	{
		return false;
	}

	Instance->Source = Instance->Api->CreateSoundObjectSource(Mode);
	if (Instance->Source == vraudio::ResonanceAudioApi::kInvalidSourceId)
	{
		Resonance_DestroyInstance(Instance);
		return false;
	}

	Instance->Api->EnableRoomEffects(IsRoom);
	Instance->Api->SetHeadPosition(0.0f, 0.0f, 0.0f);
	Instance->Api->SetHeadRotation(0.0f, 0.0f, 0.0f, 1.0f);
	Instance->Api->SetSourceDistanceModel(Instance->Source, vraudio::DistanceRolloffModel::kNone, 0.0f, 0.0f);
	Instance->Api->SetSourceDistanceAttenuation(Instance->Source, 1.0f);
	Instance->Api->SetSourceRoomEffectsGain(Instance->Source, IsRoom ? 1.0f : 0.0f);
	return true;
}

static u32 Resonance_AcquireHrtf()
{
	if (!GResonance.FreeHrtfs.empty())
	{
		u32 Hrtf = GResonance.FreeHrtfs.back();
		GResonance.FreeHrtfs.pop_back();
		return Hrtf;
	}

	if (GResonance.Hrtfs.size() >= RESONANCE_HRTF_COUNT)
	{
		return 0;
	}

	ResonanceInstance Instance;
	if (!Resonance_CreateInstance(&Instance, vraudio::RenderingMode::kBinauralHighQuality, false))
	{
		return 0;
	}

	GResonance.Hrtfs.reserve(RESONANCE_HRTF_COUNT);
	GResonance.FreeHrtfs.reserve(RESONANCE_HRTF_COUNT);
	GResonance.Hrtfs.push_back(Instance);
	return (u32)GResonance.Hrtfs.size();
}

static void Resonance_ReleaseHrtf(u32* Hrtf)
{
	if (*Hrtf != 0)
	{
		GResonance.FreeHrtfs.push_back(*Hrtf);
		*Hrtf = 0;
	}
}

static void Resonance_Spatialize(ResonanceSpatializeState* State, SoundEffectProcess* Process)
{
	const float* Params = Process->Params;
	SoundSpatialPosition Position;
	Snd_SpatialLocate(Process, Params[SpatializeDoppler], &Position);

	if (!psSoundFlags.is(ss_HRTF))
	{
		Resonance_ReleaseHrtf(&State->Hrtf);
	}
	else if (State->Hrtf == 0)
	{
		State->Hrtf = Resonance_AcquireHrtf();
	}

	if (State->Hrtf == 0)
	{
		Snd_SpatialPan(&State->Pan, Process, &Position, Params[SpatializeFallbackRolloff], Params[SpatializeBackAttenuation]);
		return;
	}

	State->Pan.IsPanned = false;

	Fvector Direction = Position.Local;
	if (Direction.square_magnitude() < EPS)
	{
		Direction.set(0.0f, 0.0f, 1.0f);
	}

	ResonanceInstance& Instance = GResonance.Hrtfs[State->Hrtf - 1];
	Instance.Api->SetSourcePosition(Instance.Source, Direction.x, Direction.y, Direction.z);

	const float* MonoInput[1] = {Process->Data[0]};
	Instance.Api->SetPlanarBuffer(Instance.Source, MonoInput, 1, SND_BLOCKSIZE);
	Instance.Api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, Process->Data);

	float Attenuation = Snd_DistanceAttenuation(&Position, Params[SpatializeRolloff]);
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
		{
			Process->Data[Channel][Frame] *= Attenuation;
		}
	}
}

static bool Resonance_SpatializeProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	ResonanceSpatializeState* State = (ResonanceSpatializeState*)StatePtr;
	switch (Op)
	{
	case SoundEffectOp::Describe:
		*(SoundEffectDesc*)Arg = {SoundEffectScope::Voice, sizeof(ResonanceSpatializeState), (u32)std::size(SpatializeParams), SpatializeParams};
		return true;
	case SoundEffectOp::Create:
		return true;
	case SoundEffectOp::Reset:
	case SoundEffectOp::Destroy:
		Resonance_ReleaseHrtf(&State->Hrtf);
		State->Pan.IsPanned = false;
		return true;
	case SoundEffectOp::Process:
		Resonance_Spatialize(State, (SoundEffectProcess*)Arg);
		return true;
	default:
		return false;
	}
}

static void Resonance_ApplyReverb(ResonanceReverbState* State, const float* Params)
{
	vraudio::ReflectionProperties Reflection = {};
	Reflection.room_dimensions[0] = Params[ReverbSize];
	Reflection.room_dimensions[1] = Params[ReverbSize];
	Reflection.room_dimensions[2] = Params[ReverbSize];
	Reflection.room_rotation[3] = 1.0f;
	Reflection.cutoff_frequency = Params[ReverbCutoff];
	Reflection.gain = Params[ReverbReflections];
	for (float& Coefficient : Reflection.coefficients)
	{
		Coefficient = Params[ReverbDiffusion];
	}

	vraudio::ReverbProperties Reverb = {};
	Reverb.gain = 1.0f;
	for (u32 Band = 0; Band < std::size(Reverb.rt60_values); Band++)
	{
		float Decay = Band >= 6 ? Params[ReverbDecay] * Params[ReverbDecayHf] * Params[ReverbAirHf] : Params[ReverbDecay];
		Reverb.rt60_values[Band] = std::clamp(Decay, 0.05f, 120.0f);
	}

	State->TailFrames = (u32)(Params[ReverbDecay] * SND_SAMPLERATE);
	State->Instance.Api->SetReflectionProperties(Reflection);
	State->Instance.Api->SetReverbProperties(Reverb);
}

static bool Resonance_ReverbProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	ResonanceReverbState* State = (ResonanceReverbState*)StatePtr;
	switch (Op)
	{
	case SoundEffectOp::Describe:
		*(SoundEffectDesc*)Arg = {SoundEffectScope::Bus, sizeof(ResonanceReverbState), (u32)std::size(ReverbParams), ReverbParams};
		return true;
	case SoundEffectOp::Create:
		if (Resonance_CreateInstance(&State->Instance, vraudio::RenderingMode::kRoomEffectsOnly, true))
		{
			Resonance_ApplyReverb(State, ((SoundEffectCreate*)Arg)->Params);
		}
		return true;
	case SoundEffectOp::Update:
		if (State->Instance.Api != nullptr)
		{
			Resonance_ApplyReverb(State, (const float*)Arg);
		}
		return true;
	case SoundEffectOp::Destroy:
		Resonance_DestroyInstance(&State->Instance);
		return true;
	case SoundEffectOp::Tail:
		*(u32*)Arg = State->TailFrames;
		return true;
	case SoundEffectOp::Process:
	{
		SoundEffectProcess* Process = (SoundEffectProcess*)Arg;
		if (State->Instance.Api == nullptr)
		{
			memset(Process->Data[0], 0, SND_BLOCKSIZE * sizeof(float));
			memset(Process->Data[1], 0, SND_BLOCKSIZE * sizeof(float));
			return true;
		}

		for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
		{
			State->Mono[Frame] = (Process->Data[0][Frame] + Process->Data[1][Frame]) * 0.5f;
		}

		const float* MonoInput[1] = {State->Mono};
		State->Instance.Api->SetPlanarBuffer(State->Instance.Source, MonoInput, 1, SND_BLOCKSIZE);
		State->Instance.Api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, Process->Data);

		float Gain = Process->Params[ReverbGain];
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
			{
				Process->Data[Channel][Frame] *= Gain;
			}
		}
		return true;
	}
	default:
		return false;
	}
}

void Resonance_RegisterEffects()
{
	Snd_RegisterEffect("spatialize", Resonance_SpatializeProc);
	Snd_RegisterEffect("reverb", Resonance_ReverbProc);
}

void Resonance_Shutdown()
{
	for (ResonanceInstance& Instance : GResonance.Hrtfs)
	{
		Resonance_DestroyInstance(&Instance);
	}

	GResonance.Hrtfs.clear();
	GResonance.FreeHrtfs.clear();
}
