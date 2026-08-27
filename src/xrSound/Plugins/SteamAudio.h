#pragma once

#include "New/ReverbInterface.h"
#include "New/SoundSpatializer.h"
#include "New/SoundMeta.h"

#include <SteamAudio/phonon.h>

class CSteamAudioReverb final :
    public IReverInterface
{
public:
    void SetContext(IPLContext AudioContext)
    {
        Context = AudioContext;
    }

    void InitZone(sound_zone_params& Params) override;
    void ReleaseZone(sound_zone_params& Zone) override;
    void ProcessReverb(sound_zone_params& Zone, float** ReverbBuffer, float** ProcessBuffer, float** BusBuffer) override;

private:
    struct ReverbZoneState
    {
        IPLReflectionEffect effect[SND_CHANNEL_COUNT] = { 0 };
    };

    void ConvertReverbSettings(const sound_reverb_settings* ReverbIn, IPLReflectionEffectParams* ReverbOut, IPLint32 SampleRate);
    IPLReflectionEffectParams EngineToIPLParams(const sound_reverb_settings& Settings);

    IPLContext Context = nullptr;
    xr_map<u32, ReverbZoneState> ReverbStates;
    u32 NextReverbId = 1;
};

class CSteamAudioSpatializer final :
    public ISoundSpatializer
{
public:
    void Initialize() override;
    void Shutdown() override;

    void ResetSlot(u32 SlotIndex) override;
    void ProcessHrtf(u32 SlotIndex, float** Data, const Fvector& SourcePosition, const Fvector& HeadPosition, const Fvector& RelativeDirection) override;

    IPLContext GetContext() { return Context; }

private:
    struct HrtfSlot
    {
        IPLBinauralEffect Effect = nullptr;
        float Storage[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
        float* ProcessBuffer[SND_CHANNEL_COUNT];
        IPLAudioBuffer BufferDescriptor = {};
    };

    IPLContext Context = nullptr;
    IPLHRTF Hrtf = nullptr;
    xr_vector<HrtfSlot> Slots;
};
