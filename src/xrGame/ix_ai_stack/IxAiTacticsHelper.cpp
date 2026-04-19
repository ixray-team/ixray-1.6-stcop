#include "StdAfx.h"

#include "../../xrCore/vector.h"
#include "IxAiTacticsHelper.h"

bool IxAiTacticsHelper::ComputeThreatHorizontalBasis(
    const Fvector& selfPosition,
    const Fvector& threatPosition,
    Fvector& outToThreatUnit,
    Fvector& outLateralUnit,
    f32& outDistance)
{
    Fvector toThreat{};
    toThreat.sub(threatPosition, selfPosition);
    outDistance = toThreat.magnitude();

    if (outDistance < EPS_L)
    {
        return false;
    }

    outToThreatUnit = toThreat;
    outToThreatUnit.mul(1.f / outDistance);

    Fvector up{};
    up.set(0.f, 1.f, 0.f);
    outLateralUnit.crossproduct(outToThreatUnit, up);

    if (outLateralUnit.square_magnitude() < EPS_L)
    {
        outLateralUnit.set(1.f, 0.f, 0.f);
    }
    else
    {
        const f32 lateralMag = outLateralUnit.magnitude();

        if (lateralMag > EPS_L)
        {
            outLateralUnit.mul(1.f / lateralMag);
        }
    }

    return true;
}
