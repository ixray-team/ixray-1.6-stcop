#pragma once

#include <cmath>

#include "../../xrCore/_types.h"

namespace IxAiDecay
{
    // Multiplier applied each tick to a value that decays exponentially with rate decayPerSecond.
    inline f32 ExponentialRetentionFactor(f32 deltaTime, f32 decayPerSecond)
    {
        return std::exp(-deltaTime * decayPerSecond);
    }

    // Linear step subtracted from global suspicion (IxAiManager::ApplySuspicionDecayAndAlertBands).
    inline f32 LinearSuspicionStep(f32 suspicionDecayRate, f32 globalScale, f32 deltaTime)
    {
        return suspicionDecayRate * globalScale * deltaTime;
    }
}
