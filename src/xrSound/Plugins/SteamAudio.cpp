#include "stdafx.h"
#include "SteamAudio.h"

struct AutoRegSteamAudio
{
	AutoRegSteamAudio()
	{
		GSpatializer = new CSteamAudioSpatializer;
		GReverInterface = new CSteamAudioReverb;
	}

	~AutoRegSteamAudio()
	{
		xr_delete(GReverInterface);
		xr_delete(GSpatializer);
	}
};
static AutoRegSteamAudio RegCall;

void CSteamAudioReverb::ConvertReverbSettings(const sound_reverb_settings* ReverbIn, IPLReflectionEffectParams* ReverbOut, IPLint32 SampleRate)
{
    ReverbOut->type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC;

    ReverbOut->reverbTimes[1] = ReverbIn->decay_time;
    ReverbOut->reverbTimes[2] = ReverbIn->decay_time * ReverbIn->decay_hf_ratio;
    ReverbOut->reverbTimes[0] = ReverbIn->decay_time;

    ReverbOut->delay = (IPLint32)(ReverbIn->reverb_delay * SampleRate);
}

IPLReflectionEffectParams CSteamAudioReverb::EngineToIPLParams(const sound_reverb_settings& Settings)
{
    constexpr float Duration = 4.0f;

    IPLReflectionEffectParams Params = { .type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC };

    float DecayTime = std::max(Settings.decay_time, 0.1f);
    float HfRatio = std::clamp(Settings.decay_hf_ratio, 0.1f, 2.0f);

    Params.reverbTimes[1] = DecayTime;
    Params.reverbTimes[2] = DecayTime * HfRatio;
    Params.reverbTimes[0] = DecayTime * (2.0f - HfRatio);

    Params.delay = static_cast<IPLint32>(std::round(std::clamp(Settings.reverb_delay, 0.0f, 10.0f) * SND_SAMPLERATE));

    float RoomHfClamped = std::clamp(Settings.room_hf, -10000.0f, 0.0f);
    float HfGain = powf(10.0f, RoomHfClamped / 2000.0f);

    Params.eq[0] = 1.0f;
    Params.eq[1] = 1.0f;
    Params.eq[2] = HfGain;

    Params.irSize = SND_SAMPLERATE * Duration;
    Params.numChannels = SND_CHANNEL_COUNT * SND_CHANNEL_COUNT;

    return Params;
}

void CSteamAudioReverb::InitZone(sound_zone_params& Params)
{
    ReverbZoneState& State = ReverbStates[NextReverbId];
    Params.reverb_id = NextReverbId;
    NextReverbId++;

    IPLAudioSettings Settings = { .samplingRate = SND_SAMPLERATE, .frameSize = SND_BLOCKSIZE };
    IPLReflectionEffectSettings ReflectSettings =
    {
        .type = IPL_REFLECTIONEFFECTTYPE_PARAMETRIC, .irSize = SND_BLOCKSIZE * 4, .numChannels = 1
    };

    for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
    {
        R_ASSERT(iplReflectionEffectCreate(Context, &Settings, &ReflectSettings, &State.effect[Channel]) == IPL_STATUS_SUCCESS);
    }
}

void CSteamAudioReverb::ReleaseZone(sound_zone_params& Zone)
{
    auto It = ReverbStates.find(Zone.reverb_id);
    if (It != ReverbStates.end())
    {
        for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
        {
            if (It->second.effect[Channel] != nullptr)
            {
                iplReflectionEffectRelease(&It->second.effect[Channel]);
            }
        }
        ReverbStates.erase(It);
    }
    Zone.reverb_id = 0;
}

void CSteamAudioReverb::ProcessReverb(sound_zone_params& Zone, float** ReverbBuffer, float** ProcessBuffer, [[maybe_unused]]float** BusBuffer)
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

    for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
    {
        IPLAudioBuffer ReverbBuf = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &ReverbBuffer[Channel] };
        IPLAudioBuffer OutBuf = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = &ProcessBuffer[Channel] };
        IPLReflectionEffectParams Params = {};
        ConvertReverbSettings(&Zone.settings, &Params, SND_SAMPLERATE);
        iplReflectionEffectApply(State.effect[Channel], &Params, &ReverbBuf, &OutBuf, nullptr);
    }
}

void CSteamAudioSpatializer::Initialize()
{
    IPLContextSettings ContextSettings = { .version = STEAMAUDIO_VERSION, .simdLevel = IPL_SIMDLEVEL_SSE2 };
    IPLAudioSettings Settings = { .samplingRate = SND_SAMPLERATE, .frameSize = SND_BLOCKSIZE };
    IPLerror Error = iplContextCreate(&ContextSettings, &Context);
    R_ASSERT(Error == IPL_STATUS_SUCCESS);

    IPLHRTFSettings HrtfSettings = { .type = IPL_HRTFTYPE_DEFAULT, .volume = 1.0f };
    R_ASSERT(iplHRTFCreate(Context, &Settings, &HrtfSettings, &Hrtf) == IPL_STATUS_SUCCESS);

    Slots.resize(SND_HRTF_SLOT_COUNT);
    for (size_t Key = 0; Key < SND_HRTF_SLOT_COUNT; Key++)
    {
        for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
        {
            Slots[Key].ProcessBuffer[Channel] = Slots[Key].Storage[Channel];
        }
        Slots[Key].BufferDescriptor = { .numChannels = 1, .numSamples = SND_BLOCKSIZE, .data = Slots[Key].ProcessBuffer };

        IPLBinauralEffectSettings Binaural = { .hrtf = Hrtf };
        R_ASSERT(iplBinauralEffectCreate(Context, &Settings, &Binaural, &Slots[Key].Effect) == IPL_STATUS_SUCCESS);
	}


	((CSteamAudioReverb*)GReverInterface)->SetContext(GetContext());
}

void CSteamAudioSpatializer::Shutdown()
{
	for (size_t Key = 0; Key < Slots.size(); Key++)
	{
		if (Slots[Key].Effect != nullptr)
		{
			iplBinauralEffectRelease(&Slots[Key].Effect);
		}
	}
	Slots.clear();

	if (Hrtf)
	{
		iplHRTFRelease(&Hrtf);
	}
	if (Context)
	{
		iplContextRelease(&Context);
	}

	Hrtf = nullptr;
	Context = nullptr;
}

void CSteamAudioSpatializer::ResetSlot(u32 SlotIndex)
{
    if (Hrtf != nullptr && SlotIndex < Slots.size())
    {
        iplBinauralEffectReset(Slots[SlotIndex].Effect);
    }
}

void CSteamAudioSpatializer::FreeSlot(u32 SlotIndex)
{
    if (Hrtf != nullptr && SlotIndex < Slots.size())
    {
        iplBinauralEffectReset(Slots[SlotIndex].Effect);
    }
}

void CSteamAudioSpatializer::ProcessHrtf(u32 SlotIndex, float** Data, const Fvector& SourcePosition, const Fvector& HeadPosition, const Fvector& RelativeDirection)
{
    auto& Slot = Slots[SlotIndex];

    for (size_t Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
    {
        memcpy(Slot.ProcessBuffer[Channel], Data[Channel], SND_BLOCKSIZE * sizeof(float));
    }

    IPLBinauralEffectParams BinauralParams =
    {
        .direction = { RelativeDirection.x, RelativeDirection.y, RelativeDirection.z },
        .spatialBlend = 1.0f,
        .hrtf = Hrtf
    };

    IPLAudioBuffer OutBuffer = { .numChannels = SND_CHANNEL_COUNT, .numSamples = SND_BLOCKSIZE, .data = Data };
    R_ASSERT(iplBinauralEffectApply(Slot.Effect, &BinauralParams, &Slot.BufferDescriptor, &OutBuffer) == IPL_STATUS_SUCCESS);
}
