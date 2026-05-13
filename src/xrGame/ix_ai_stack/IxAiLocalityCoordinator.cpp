#include "StdAfx.h"

#include "../../xrCore/_vector3d.h"

#include "IxAiLocalityCoordinator.h"
#include "IxAiStackApi.h"
#include "IxAiStackTuning.h"

namespace
{
    f32 HorizontalDistanceSq(const Fvector& a, const Fvector& b)
    {
        const f32 dx = a.x - b.x;
        const f32 dz = a.z - b.z;
        return dx * dx + dz * dz;
    }
}

IxAiLocalityCoordinator::IxAiLocalityCoordinator() = default;

IxAiLocalityCoordinator::~IxAiLocalityCoordinator() = default;

void IxAiLocalityCoordinator::ApplyActorAnchoredAttenuation(
    const Fvector& agentPosition,
    bool hasThreat,
    const Fvector& threatPosition,
    xr_vector<IxAiPerceptionEvent>& events)
{
    if (!IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::LocalityActorAttenuation))
    {
        return;
    }

    if (!hasThreat)
    {
        return;
    }

    if (events.empty())
    {
        return;
    }

    const f32 begin = g_ixAiRuntimeTuning.localityActorSoftBeginDistance;
    const f32 cutoff = g_ixAiRuntimeTuning.localityActorHardCutoffDistance;
    const f32 minScale = g_ixAiRuntimeTuning.localityActorMinIntensityScale;
    const f32 anchorRadius = g_ixAiRuntimeTuning.localityActorAnchorRadius;

    if (begin < EPS_L || cutoff <= begin + EPS_L || anchorRadius < EPS_L)
    {
        return;
    }

    const f32 minScaleClamped = clampr(minScale, 0.f, 1.f);
    const f32 agentDistHoriz = sqrtf(HorizontalDistanceSq(agentPosition, threatPosition));

    if (agentDistHoriz <= begin)
    {
        return;
    }

    f32 scale = 1.f;

    if (agentDistHoriz >= cutoff)
    {
        scale = minScaleClamped;
    }
    else
    {
        const f32 span = cutoff - begin;
        const f32 t = (agentDistHoriz - begin) / span;
        scale = 1.f - t * (1.f - minScaleClamped);
        scale = clampr(scale, minScaleClamped, 1.f);
    }

    const f32 anchorSq = anchorRadius * anchorRadius;

    for (IxAiPerceptionEvent& event : events)
    {
        if (HorizontalDistanceSq(event._position, threatPosition) > anchorSq)
        {
            continue;
        }

        event._intensity *= scale;
    }
}
