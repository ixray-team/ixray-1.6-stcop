#pragma once

#include "New/ReverbInterface.h"
#include "New/SoundSpatializer.h"
#include "New/SoundMeta.h"

#include "../../3rd-party/resonance-audio/resonance_audio/api/binaural_surround_renderer.h"
#include "../../3rd-party/resonance-audio/resonance_audio/api/resonance_audio_api.h"

class CResonanceAudioReverb final :
    public IReverInterface
{
public:
    void InitZone(sound_zone_params& Params) override;
    void ReleaseZone(sound_zone_params& Zone) override;
    void ProcessReverb(sound_zone_params& Zone, float** ReverbBuffer, float** ProcessBuffer, float** BusBuffer) override;

private:
    struct ReverbZoneState
    {
        vraudio::ResonanceAudioApi* ra_context = nullptr;
        int buffer = 0;
        float compressor_envelope[SND_CHANNEL_COUNT][2] = { { FLT_EPSILON, FLT_EPSILON }, { FLT_EPSILON, FLT_EPSILON } };
    };

    float DbToLinear(float Db);
    void EngineToResonanceParams(const sound_zone_params& Zone, vraudio::ReflectionProperties& OutReflection, vraudio::ReverbProperties& OutReverb);

    xr_map<u32, ReverbZoneState> ReverbStates;
    u32 NextReverbId = 1;
};

class CResonanceAudioSpatializer final :
    public ISoundSpatializer
{
public:
    void Initialize() override;
    void Shutdown() override;

    void ResetSlot(u32 SlotIndex) override;
    void FreeSlot(u32 SlotIndex) override;
    void ProcessHrtf(u32 SlotIndex, float** Data, const Fvector& SourcePosition, const Fvector& HeadPosition, const Fvector& RelativeDirection) override;

private:
    struct HrtfSlot
    {
        vraudio::ResonanceAudioApi* Api = nullptr;
        vraudio::ResonanceAudioApi::SourceId SourceId = vraudio::ResonanceAudioApi::kInvalidSourceId;
    };

    // Always-valid slots indexed by the mixer's HRTF slot id (cheap, just pointers).
    xr_vector<HrtfSlot> Slots;
    // Pool of detached engine instances ready to be reused by a new attach.
    xr_vector<HrtfSlot> FreeSlots;
};
