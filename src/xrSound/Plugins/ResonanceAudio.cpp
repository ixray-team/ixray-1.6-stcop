#include "stdafx.h"
#include "ResonanceAudio.h"
#include "../New/SoundDSP.h"

struct AutoRegReverbResonance
{
	AutoRegReverbResonance()
	{
		GReverInterface = new CResonanceAudioReverb;
	}

	~AutoRegReverbResonance()
	{
		xr_delete(GReverInterface);
	}
};
static AutoRegReverbResonance RegCall;


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
	Api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
	R_ASSERT(Api != nullptr);

	Slots.resize(SND_HRTF_SLOT_COUNT);
	for (size_t i = 0; i < SND_HRTF_SLOT_COUNT; i++)
	{
		Slots[i].SourceId = Api->CreateSoundObjectSource(vraudio::RenderingMode::kBinauralHighQuality);
	}
}

void CResonanceAudioSpatializer::Shutdown()
{
	if (Api)
	{
		for (size_t i = 0; i < Slots.size(); i++)
		{
			auto Id = Slots[i].SourceId;
			if (Id != vraudio::ResonanceAudioApi::kInvalidSourceId)
			{
				Api->DestroySource(Id);
			}
		}
		delete Api;
		Api = nullptr;
	}

	Slots.clear();
}

void CResonanceAudioSpatializer::ResetSlot([[maybe_unused]] u32 SlotIndex)
{
}

void CResonanceAudioSpatializer::ProcessHrtf(u32 SlotIndex, float** Data, const Fvector& SourcePosition, const Fvector& HeadPosition, const Fvector& RelativeDirection)
{
	Api->SetHeadPosition(HeadPosition.x, HeadPosition.y, HeadPosition.z);

	auto Id = Slots[SlotIndex].SourceId;
	if (Id != vraudio::ResonanceAudioApi::kInvalidSourceId)
	{
		Api->SetSourcePosition(Id, SourcePosition.x, SourcePosition.y, SourcePosition.z);
		Api->SetPlanarBuffer(Id, (const float* const*)Data, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
		Api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, Data);
	}
}
