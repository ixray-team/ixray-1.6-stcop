#include "StdAfx.h"

#include "../ai/stalker/ai_stalker.h"
#include "../danger_manager.h"
#include "../memory_manager.h"
#include "IxAiOutputAdapter.h"
#include "IxAiStackApi.h"

namespace
{
    bool IxAiIsLegacyOutputFeature(IxAiFeatureGate feature)
    {
        switch (feature)
        {
        case IxAiFeatureGate::LegacyBridge:
        case IxAiFeatureGate::TacticsFeedMovementHint:
        case IxAiFeatureGate::CoverFeedDangerHint:
            return true;
        default:
            return false;
        }
    }
} // namespace

bool IxAiOutputAdapter::IsLegacyOutputAllowed(IxAiFeatureGate feature)
{
    if (!IxAiIsLegacyOutputFeature(feature))
    {
        return false;
    }

    return IxAiStackApi::IsFeatureEnabled(feature);
}

bool IxAiOutputAdapter::TryAddDanger(
    CAI_Stalker& stalker,
    const CEntityAlive* subject,
    const Fvector& position,
    u32 time,
    CDangerObject::EDangerType dangerType,
    CDangerObject::EDangerPerceiveType perceiveType,
    IxAiFeatureGate feature)
{
    if (!IsLegacyOutputAllowed(feature) || !IsStalkerWritable(stalker))
    {
        return false;
    }

    stalker.memory().danger().add(CDangerObject(subject, position, time, dangerType, perceiveType));
    return true;
}

bool IxAiOutputAdapter::TrySetEnemyVisibleAndAddDanger(
    CAI_Stalker& stalker,
    const CEntityAlive& enemy,
    u32 time,
    CDangerObject::EDangerType dangerType,
    CDangerObject::EDangerPerceiveType perceiveType,
    IxAiFeatureGate feature)
{
    if (!IsLegacyOutputAllowed(feature) || !IsStalkerWritable(stalker))
    {
        return false;
    }

    stalker.memory().make_object_visible_somewhen(&enemy);
    stalker.memory().enemy().set_enemy(&enemy);
    stalker.memory().danger().add(CDangerObject(&enemy, enemy.Position(), time, dangerType, perceiveType));
    return true;
}

bool IxAiOutputAdapter::IsStalkerWritable(CAI_Stalker& stalker)
{
    return stalker.g_Alive() && !stalker.getDestroy();
}
