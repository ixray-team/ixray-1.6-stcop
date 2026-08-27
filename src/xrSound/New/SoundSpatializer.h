#pragma once

#include "SoundMeta.h"

class ISoundSpatializer
{
public:
    virtual ~ISoundSpatializer() = default;

    virtual void Initialize() = 0;
    virtual void Shutdown() = 0;

    virtual void ResetSlot(u32 slotIndex) = 0;
    virtual void ProcessHrtf(u32 slotIndex, float** data, const Fvector& sourcePosition, const Fvector& headPosition, const Fvector& relativeDirection) = 0;
};

extern ISoundSpatializer* GSpatializer;