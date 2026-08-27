#pragma once

struct sound_zone_params;

class IReverInterface
{
public:
    virtual ~IReverInterface() = default;

    virtual void InitZone(sound_zone_params& params) {}
    virtual void ReleaseZone(sound_zone_params& zone) {}
    virtual void ProcessReverb(sound_zone_params& zone, float** reverbBuffer, float** processBuffer, float** busBuffer) {}
};

extern IReverInterface* GReverInterface;