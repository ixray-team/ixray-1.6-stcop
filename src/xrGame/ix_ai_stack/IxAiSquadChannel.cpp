#include "StdAfx.h"

#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../../xrEngine/device.h"
#include "../agent_manager.h"
#include "../agent_member_manager.h"
#include "../ai/stalker/ai_stalker.h"
#include "../Hit.h"
#include "IxAiManager.h"
#include "IxAiSquadChannel.h"
#include "IxAiStackApi.h"
#include "IxAiStackTuning.h"
#include "IxAiTypes.h"

namespace
{
    bool ClassifyStealthLikeSquadHit(const SHit& hit)
    {
        if (!hit.is_valide())
        {
            return false;
        }

        const ALife::EHitType hitType = hit.type();

        switch (hitType)
        {
        case ALife::eHitTypeStrike:
        case ALife::eHitTypeWound_2:
        case ALife::eHitTypePhysicStrike:
            return true;
        default:
            return false;
        }
    }

    bool EnvironmentAllowsIxAiSquadChannel()
    {
        if (g_dedicated_server)
        {
            return false;
        }

        if (!EngineExternal().CallOfPripyatMode())
        {
            return false;
        }

        if (!EngineExternal()[EEngineExternalGame::EnableIxAiStack])
        {
            return false;
        }

        if (!IsGameTypeSingle())
        {
            return false;
        }

        return true;
    }

    void CollectSquadRecipients(
        const CAI_Stalker& sourceStalker,
        u16 skipObjectId,
        const Fvector& referencePosition,
        xr_vector<u16>& outRecipients)
    {
        outRecipients.clear();

        const CAgentMemberManager& memberManager = sourceStalker.agent_manager().member();
        const CAgentMemberManager::MEMBER_STORAGE& members = memberManager.members();

        if (members.size() <= 1)
        {
            return;
        }

        const f32 maxDistance = g_ixAiRuntimeTuning.squadChannelMaxDistance;
        const bool useDistanceGate = maxDistance > EPS_L;

        outRecipients.reserve(members.size());

        for (CMemberOrder* memberOrder : members)
        {
            if (memberOrder == nullptr)
            {
                continue;
            }

            CAI_Stalker& mate = memberOrder->object();
            const u16 mateId = mate.ID();

            if (mateId == skipObjectId)
            {
                continue;
            }

            if (!mate.g_Alive() || mate.getDestroy())
            {
                continue;
            }

            if (useDistanceGate && mate.Position().distance_to(referencePosition) > maxDistance)
            {
                continue;
            }

            outRecipients.push_back(mateId);
        }
    }
} // namespace

void IxAiSquadChannel::NotifyStalkerWound(CAI_Stalker& victim, const SHit& hit, const Fvector& worldPosition)
{
    if (!EnvironmentAllowsIxAiSquadChannel())
    {
        return;
    }

    xrCriticalSectionGuard tuningGuard(g_ixAiRuntimeTuningCs);

    if (!g_ixAiRuntimeTuning.squadChannelEnabled)
    {
        return;
    }

    if (!hit.is_valide())
    {
        return;
    }

    if (hit.damage() <= EPS_L)
    {
        return;
    }

    if (!IxAiStackApi::IsActive())
    {
        return;
    }

    IxAiManager* manager = IxAiStackApi::Manager();

    if (manager == nullptr)
    {
        return;
    }

    static xr_vector<u16> recipientScratch{};
    CollectSquadRecipients(victim, victim.ID(), worldPosition, recipientScratch);

    if (recipientScratch.empty())
    {
        return;
    }

    const f32 damageScale = clampr(hit.damage() / 50.f, 0.15f, 2.5f);
    const bool stealthClassified =
        g_ixAiRuntimeTuning.squadFanoutStealthHitHandlingEnabled && ClassifyStealthLikeSquadHit(hit);

    u16 sourceObjectId = 0;

    if (hit.who != nullptr)
    {
        sourceObjectId = hit.who->ID();
    }

    if (stealthClassified && g_ixAiRuntimeTuning.squadFanoutClearAttackerIdOnStealthHit)
    {
        sourceObjectId = 0;
    }

    IxAiPerceptionEvent event{};
    event._position = worldPosition;
    event._sourceObjectId = sourceObjectId;
    event._type = IxAiPerceptionEventType::SquadAllyWounded;
    event._intensity = g_ixAiRuntimeTuning.squadAllyWoundIntensity * damageScale;
    event._radius = g_ixAiRuntimeTuning.squadAllyWoundRadius;
    event._timestamp = (f32)Device.dwTimeGlobal;

    if (stealthClassified)
    {
        event._squadFanoutFlags = (u8)IxAiSquadFanoutFlags::StealthClassified;
        event._intensity *= g_ixAiRuntimeTuning.squadFanoutStealthSuspicionScale;
    }

    manager->DeliverSquadFanout(recipientScratch, event);
}

void IxAiSquadChannel::NotifyCombatRegistered(const CAI_Stalker& registrant)
{
    if (!EnvironmentAllowsIxAiSquadChannel())
    {
        return;
    }

    xrCriticalSectionGuard tuningGuard(g_ixAiRuntimeTuningCs);

    if (!g_ixAiRuntimeTuning.squadChannelEnabled)
    {
        return;
    }

    if (!registrant.g_Alive())
    {
        return;
    }

    if (!IxAiStackApi::IsActive())
    {
        return;
    }

    IxAiManager* manager = IxAiStackApi::Manager();

    if (manager == nullptr)
    {
        return;
    }

    static xr_vector<u16> recipientScratch{};
    const Fvector referencePosition = registrant.Position();
    CollectSquadRecipients(registrant, registrant.ID(), referencePosition, recipientScratch);

    if (recipientScratch.empty())
    {
        return;
    }

    IxAiPerceptionEvent event{};
    event._position = referencePosition;
    event._sourceObjectId = registrant.ID();
    event._type = IxAiPerceptionEventType::SquadCombatEngaged;
    event._intensity = g_ixAiRuntimeTuning.squadCombatEngagedIntensity;
    event._radius = g_ixAiRuntimeTuning.squadCombatEngagedRadius;
    event._timestamp = (f32)Device.dwTimeGlobal;

    manager->DeliverSquadFanout(recipientScratch, event);
}
