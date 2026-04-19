#include "StdAfx.h"

#include <cmath>

#include "../../xrCore/Collision/xr_collide_defs.h"
#include "../../xrCore/vector.h"
#include "../Level.h"
#include "../ai/stalker/ai_stalker.h"
#include "../entity_alive.h"
#include "../GameObject.h"
#include "IxAiStackTuning.h"
#include "IxAiVisionEvaluator.h"

bool IxAiVisionEvaluator::StalkerHasClearSightToTarget(CAI_Stalker& stalker, CEntityAlive& targetAlive)
{
    if (g_pGameLevel == nullptr || !g_pGameLevel->bReady)
    {
        return false;
    }

    CGameObject* stalkerObject = stalker.cast_game_object();
    CGameObject* targetObject = targetAlive.cast_game_object();

    if (stalkerObject == nullptr || targetObject == nullptr)
    {
        return false;
    }

    const Fvector eyePosition = stalker.eye_matrix.c;
    Fvector eyeForward = stalker.eye_matrix.k;
    eyeForward.normalize_safe();

    Fvector aimPoint = targetAlive.Position();
    aimPoint.y += g_ixAiRuntimeTuning.visionTargetChestHeight;

    Fvector toTarget{};
    toTarget.sub(aimPoint, eyePosition);
    const f32 distance = toTarget.magnitude();

    if (distance < EPS_L)
    {
        return true;
    }

    if (stalker.eye_range > EPS_L)
    {
        const f32 maxDist = stalker.eye_range * g_ixAiRuntimeTuning.visionMaxDistanceScale;

        if (distance > maxDist)
        {
            return false;
        }
    }

    Fvector dirToTarget = toTarget;
    dirToTarget.mul(1.f / distance);

    f32 halfFovRad = deg2rad(stalker.eye_fov * 0.5f);

    if (stalker.eye_fov < EPS_L)
    {
        halfFovRad = deg2rad(g_ixAiRuntimeTuning.visionHalfFovDegreesFallback);
    }

    const f32 cosHalfFov = std::cos(halfFovRad);
    const f32 facingDot = eyeForward.dotproduct(dirToTarget);

    if (facingDot < cosHalfFov)
    {
        return false;
    }

    collide::rq_result hit{};
    const f32 rayRange = distance + g_ixAiRuntimeTuning.visionRayRangeExtra;
    const BOOL hadHit = Level().ObjectSpace.RayPick(eyePosition, dirToTarget, rayRange, collide::rqtBoth, hit, stalkerObject);

    if (!hadHit)
    {
        return true;
    }

    if (hit.O != nullptr && hit.O == targetObject)
    {
        return true;
    }

    const f32 depthEps = g_ixAiRuntimeTuning.visionOcclusionDepthEpsilon;

    if (hit.range < distance - depthEps)
    {
        return false;
    }

    return true;
}
