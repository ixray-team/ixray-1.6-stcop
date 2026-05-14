#pragma once

#include "IxAiTypes.h"
#include "../danger_object.h"

class CAI_Stalker;

class IxAiOutputAdapter final
{
public:
    static bool IsLegacyOutputAllowed(IxAiFeatureGate feature);

    static bool TryAddDanger(
        CAI_Stalker& stalker,
        const CEntityAlive* subject,
        const Fvector& position,
        u32 time,
        CDangerObject::EDangerType dangerType,
        CDangerObject::EDangerPerceiveType perceiveType,
        IxAiFeatureGate feature);

    static bool TrySetEnemyVisibleAndAddDanger(
        CAI_Stalker& stalker,
        const CEntityAlive& enemy,
        u32 time,
        CDangerObject::EDangerType dangerType,
        CDangerObject::EDangerPerceiveType perceiveType,
        IxAiFeatureGate feature);

private:
    IxAiOutputAdapter() = delete;

    static bool IsStalkerWritable(CAI_Stalker& stalker);
};
