#pragma once

#include "../../xrCore/_vector3d.h"

class CAI_Stalker;
class IxAiAgent;

class IxAiTacticsSystem final
{
public:
    IxAiTacticsSystem();
    ~IxAiTacticsSystem();

    void EvaluateForStalker(IxAiAgent& agent, CAI_Stalker& stalker, const Fvector& threatPosition);
    void TryPublishTacticDangerHint(CAI_Stalker& stalker, IxAiAgent& agent);
    void TryPublishInvestigateMovementHint(CAI_Stalker& stalker, IxAiAgent& agent);
    void TryPublishCoverDangerHint(CAI_Stalker& stalker, IxAiAgent& agent, const Fvector& threatPosition, u32 framePhase);
};
