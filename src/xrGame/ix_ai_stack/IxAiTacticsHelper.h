#pragma once

#include "../../xrCore/_types.h"
#include "../../xrCore/_vector3d.h"

class IxAiTacticsHelper final
{
public:
    static bool ComputeThreatHorizontalBasis(
        const Fvector& selfPosition,
        const Fvector& threatPosition,
        Fvector& outToThreatUnit,
        Fvector& outLateralUnit,
        f32& outDistance);
};
