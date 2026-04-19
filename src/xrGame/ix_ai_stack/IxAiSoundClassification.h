#pragma once

#include "IxAiTypes.h"

class IxAiSoundClassification final
{
public:
    static IxAiPerceptionEventType MapEngineSoundType(int soundType, float power);
};
