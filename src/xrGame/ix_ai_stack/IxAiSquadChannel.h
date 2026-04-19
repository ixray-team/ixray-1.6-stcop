#pragma once

#include "../../xrCore/_vector3d.h"

class CAI_Stalker;
struct SHit;

// Forwards squad wound / combat cues to IX agents via DeliverSquadFanout (bypasses global perception buffer).
class IxAiSquadChannel final
{
public:
    static void NotifyStalkerWound(CAI_Stalker& victim, const SHit& hit, const Fvector& worldPosition);
    static void NotifyCombatRegistered(const CAI_Stalker& registrant);

private:
    IxAiSquadChannel() = delete;
    ~IxAiSquadChannel() = delete;
};
