#pragma once

#include "../../xrCore/_stl_extensions.h"

#include "IxAiTypes.h"

class IxAiLocalityCoordinator final
{
public:
    IxAiLocalityCoordinator();
    ~IxAiLocalityCoordinator();

    static void ApplyActorAnchoredAttenuation(
        const Fvector& agentPosition,
        bool hasThreat,
        const Fvector& threatPosition,
        xr_vector<IxAiPerceptionEvent>& events);
};
