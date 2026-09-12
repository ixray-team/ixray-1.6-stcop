#include "stdafx.h"
#include "ResonanceAudio.h"
#include "../New/SoundDSP.h"

struct AutoRegResonanceAudio
{
	AutoRegResonanceAudio()
	{
		GSpatializer = new CResonanceAudioSpatializer;
		GReverInterface = new CResonanceAudioReverb;
	}

	~AutoRegResonanceAudio()
	{
		xr_delete(GReverInterface);
		xr_delete(GSpatializer);
	}
};
static AutoRegResonanceAudio RegCall;


float CResonanceAudioReverb::DbToLinear(float Db)
{
	return powf(10.0f, Db / 20.0f);
}

void CResonanceAudioReverb::EngineToResonanceParams(const sound_zone_params& Zone, vraudio::ReflectionProperties& OutReflection, vraudio::ReverbProperties& OutReverb)
{
	OutReflection = vraudio::ReflectionProperties();
	OutReverb = vraudio::ReverbProperties();

	constexpr float HfRef = 5000.0f;

	OutReflection.room_dimensions[0] = Zone.size.x;
	OutReflection.room_dimensions[1] = Zone.size.y;
	OutReflection.room_dimensions[2] = Zone.size.z;

	OutReflection.room_position[0] = Zone.center.x;
	OutReflection.room_position[1] = Zone.center.y;
	OutReflection.room_position[2] = Zone.center.z;

	OutReflection.room_rotation[0] = OutReflection.room_rotation[1] = OutReflection.room_rotation[2] = 0.0f;
	OutReflection.room_rotation[3] = 1.0f;

	float HfDb = std::clamp(Zone.settings.room_hf, -10000.0f, 0.0f);
	float Cutoff = HfRef * DbToLinear(HfDb);
	OutReflection.cutoff_frequency = std::clamp(Cutoff, 200.0f, 20000.0f);

	float Diffusion = std::clamp(Zone.settings.environment_diffusion, 0.0f, 1.0f);
	for (int Iter = 0; Iter < std::size(OutReflection.coefficients); Iter++)
	{
		OutReflection.coefficients[Iter] = Diffusion;
	}

	OutReflection.gain = DbToLinear(std::clamp(Zone.settings.reflections, -10000.0f, 0.0f));
	OutReverb.gain = DbToLinear(std::clamp(Zone.settings.reverb, -10000.0f, 0.0f));

	const float BaseRt60 = std::clamp(Zone.settings.decay_time, 0.1f, 20.0f);
	const float HfRatio = std::clamp(Zone.settings.decay_hf_ratio, 0.1f, 4.0f);

	float AirHf = std::max(0.0f, Zone.settings.air_absorption_hf);
	float HfAbsScale = powf(10.0f, -(AirHf) / 20.0f);

	for (int i = 0; i < 9; i++)
	{
		bool IsHf = (i >= 6);

		float Ratio = BaseRt60;
		if (IsHf)
		{
			Ratio *= HfRatio * HfAbsScale;
		}

		OutReverb.rt60_values[i] = std::clamp(Ratio, 0.05f, 120.0f);
	}
}

void CResonanceAudioReverb::InitZone(sound_zone_params& Params)
{
    ReverbZoneState& State = ReverbStates[NextReverbId];
    Params.reverb_id = NextReverbId;
    NextReverbId++;

    vraudio::ReflectionProperties ReflectionProperties = {};
    vraudio::ReverbProperties ReverbProperties = {};

    EngineToResonanceParams(Params, ReflectionProperties, ReverbProperties);
    State.ra_context = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
    State.ra_context->EnableRoomEffects(true);
    State.ra_context->SetReverbProperties(ReverbProperties);
    State.ra_context->SetReflectionProperties(ReflectionProperties);
    State.buffer = State.ra_context->CreateSoundObjectSource(vraudio::RenderingMode::kStereoPanning);
}

void CResonanceAudioReverb::ReleaseZone(sound_zone_params& Zone)
{
    auto It = ReverbStates.find(Zone.reverb_id);
    if (It != ReverbStates.end())
    {
        delete It->second.ra_context;
        ReverbStates.erase(It);
    }
    Zone.reverb_id = 0;
}

void CResonanceAudioReverb::ProcessReverb(sound_zone_params& Zone, float** ReverbBuffer, float** ProcessBuffer, float** BusBuffer)
{
    if (Zone.reverb_id == 0)
    {
        return;
    }

    auto It = ReverbStates.find(Zone.reverb_id);
    if (It == ReverbStates.end())
    {
        return;
    }
    ReverbZoneState& State = It->second;

    DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, ReverbBuffer, 1.0f, SND_BLOCKSIZE, State.compressor_envelope[0]);
    State.ra_context->SetPlanarBuffer(State.buffer, ReverbBuffer, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
    State.ra_context->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, ProcessBuffer);
    DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, BusBuffer, 1.0f, SND_BLOCKSIZE, State.compressor_envelope[1]);
}

void CResonanceAudioSpatializer::Initialize()
{
	Slots.resize(SND_HRTF_SLOT_COUNT);
}

void CResonanceAudioSpatializer::Shutdown()
{
	auto DestroyAll = [](xr_vector<HrtfSlot>& List)
	{
		for (HrtfSlot& Slot : List)
		{
			if (Slot.Api != nullptr)
			{
				if (Slot.SourceId != vraudio::ResonanceAudioApi::kInvalidSourceId)
				{
					Slot.Api->DestroySource(Slot.SourceId);
					Slot.SourceId = vraudio::ResonanceAudioApi::kInvalidSourceId;
				}
				delete Slot.Api;
				Slot.Api = nullptr;
			}
		}
		List.clear();
	};

	DestroyAll(Slots);
	DestroyAll(FreeSlots);
}

void CResonanceAudioSpatializer::ResetSlot(u32 SlotIndex)
{
	if (SlotIndex >= Slots.size())
	{
		return;
	}

	HrtfSlot& Slot = Slots[SlotIndex];
	if (Slot.Api != nullptr)
	{
		return;
	}

	if (!FreeSlots.empty())
	{
		Slot = std::move(FreeSlots.back());
		FreeSlots.pop_back();
		return;
	}

	Slot.Api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	R_ASSERT(Slot.Api != nullptr);

	Slot.SourceId = Slot.Api->CreateSoundObjectSource(vraudio::RenderingMode::kBinauralHighQuality);
	R_ASSERT(Slot.SourceId != vraudio::ResonanceAudioApi::kInvalidSourceId);

	Slot.Api->EnableRoomEffects(false);

	Slot.Api->SetSourceDistanceModel(Slot.SourceId, vraudio::DistanceRolloffModel::kNone, 0.0f, 0.0f);
	Slot.Api->SetSourceDistanceAttenuation(Slot.SourceId, 1.0f);

	Slot.Api->SetHeadPosition(0.0f, 0.0f, 0.0f);
	Slot.Api->SetHeadRotation(0.0f, 0.0f, 0.0f, 1.0f);
}

void CResonanceAudioSpatializer::FreeSlot(u32 SlotIndex)
{
	if (SlotIndex >= Slots.size())
	{
		return;
	}

	HrtfSlot& Slot = Slots[SlotIndex];
	if (Slot.Api == nullptr)
	{
		return;
	}

	FreeSlots.push_back(std::move(Slot));
	Slot.Api = nullptr;
	Slot.SourceId = vraudio::ResonanceAudioApi::kInvalidSourceId;
}

void CResonanceAudioSpatializer::ProcessHrtf(u32 SlotIndex, float** Data, const Fvector& SourcePosition, const Fvector& HeadPosition, const Fvector& RelativeDirection)
{
	if (SlotIndex >= Slots.size())
	{
		return;
	}

	HrtfSlot& Slot = Slots[SlotIndex];
	if (Slot.Api == nullptr || Slot.SourceId == vraudio::ResonanceAudioApi::kInvalidSourceId)
	{
		return;
	}

	Fvector Dir = RelativeDirection;
	const float Len = std::sqrt(Dir.x * Dir.x + Dir.y * Dir.y + Dir.z * Dir.z);
	if (Len < EPS)
	{
		Dir.x = 0.0f;
		Dir.y = 0.0f;
		Dir.z = 1.0f;
	}
	else
	{
		const float Inv = 1.0f / Len;
		Dir.x *= Inv;
		Dir.y *= Inv;
		Dir.z *= Inv;
	}

	Slot.Api->SetSourcePosition(Slot.SourceId, Dir.x, Dir.y, Dir.z);

	float MonoBuffer[SND_BLOCKSIZE];
	for (size_t i = 0; i < SND_BLOCKSIZE; i++)
	{
		MonoBuffer[i] = Data[0][i];
	}
	const float* MonoInput[1] = { MonoBuffer };
	Slot.Api->SetPlanarBuffer(Slot.SourceId, MonoInput, 1, SND_BLOCKSIZE);
	Slot.Api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, Data);
}
