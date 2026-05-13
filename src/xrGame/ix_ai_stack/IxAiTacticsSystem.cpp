#include "StdAfx.h"

#include <algorithm>

#include "../../xrCore/vector.h"
#include "../../xrEngine/device.h"
#include "../ai/stalker/ai_stalker.h"
#include "../danger_object.h"
#include "../danger_manager.h"
#include "../memory_manager.h"
#include "IxAiAgent.h"
#include "IxAiConstants.h"
#include "IxAiStackApi.h"
#include "IxAiStackTelemetry.h"
#include "IxAiStackTuning.h"
#include "IxAiTacticsHelper.h"
#include "IxAiTacticsSystem.h"
#include "IxAiTypes.h"

IxAiTacticsSystem::IxAiTacticsSystem() = default;

IxAiTacticsSystem::~IxAiTacticsSystem() = default;

void IxAiTacticsSystem::EvaluateForStalker(IxAiAgent& agent, CAI_Stalker& stalker, const Fvector& threatPosition)
{
    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        agent.SetTacticalHint(Fvector().set(0.f, 0.f, 0.f), false);
        return;
    }

    const IxAiAlertLevel alert = agent.GetAlertLevel();

    if (alert != IxAiAlertLevel::Search && alert != IxAiAlertLevel::Combat)
    {
        return;
    }

    const Fvector self = stalker.Position();
    Fvector toThreatUnit{};
    Fvector lateral{};
    f32 dist{};

    if (!IxAiTacticsHelper::ComputeThreatHorizontalBasis(self, threatPosition, toThreatUnit, lateral, dist))
    {
        agent.SetTacticalHint(Fvector().set(0.f, 0.f, 0.f), false);
        return;
    }

    Fvector hint = threatPosition;
    const IxAiBehaviourProfile& profile = agent.GetProfile();

    if (profile._behaviourKind == IxAiBehaviourKind::FlankerLite)
    {
        const f32 side = profile._flankRange * g_ixAiRuntimeTuning.tacticsFlankSideScale;
        hint.mad(lateral, side);
    }
    else
    {
        const f32 back = clampr(
            g_ixAiRuntimeTuning.tacticsGuardHoldDistance,
            IxAiConstants::kTacticsGuardHoldClampMin,
            IxAiConstants::kTacticsGuardHoldClampMax);
        const f32 backAmount = clampr(
            dist * IxAiConstants::kTacticsGuardBackDistanceFraction,
            IxAiConstants::kTacticsGuardBackDistanceMin,
            back);
        hint.mad(toThreatUnit, -backAmount);
    }

    agent.SetTacticalHint(hint, true);
}

void IxAiTacticsSystem::TryPublishTacticDangerHint(CAI_Stalker& stalker, IxAiAgent& agent)
{
    if (!IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::TacticsFeedMovementHint))
    {
        return;
    }

    if (!agent.HasTacticalHint())
    {
        return;
    }

    const IxAiAlertLevel alert = agent.GetAlertLevel();

    if (alert != IxAiAlertLevel::Search && alert != IxAiAlertLevel::Combat)
    {
        return;
    }

    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        return;
    }

    const u32 now = Device.dwTimeGlobal;

    if ((now - agent.GetTacticHintDangerPushTime()) < g_ixAiRuntimeTuning.tacticHintDangerCooldownMs)
    {
        return;
    }

    const Fvector hint = agent.GetTacticalHintPosition();

    stalker.memory().danger().add(
        CDangerObject(
            nullptr,
            hint,
            now,
            CDangerObject::eDangerTypeEnemySound,
            CDangerObject::eDangerPerceiveTypeSound));

    agent.SetTacticHintDangerPushTime(now);
    IxAiStackTelemetry_AddTacticHintPush(1u);
}

void IxAiTacticsSystem::TryPublishInvestigateMovementHint(CAI_Stalker& stalker, IxAiAgent& agent)
{
    if (!IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::TacticsFeedMovementHint))
    {
        return;
    }

    if (!agent.HasTacticalHint())
    {
        return;
    }

    const IxAiAlertLevel alert = agent.GetAlertLevel();

    if (alert != IxAiAlertLevel::Vigilant && alert != IxAiAlertLevel::Suspicious)
    {
        return;
    }

    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        return;
    }

    const u32 now = Device.dwTimeGlobal;

    if ((now - agent.GetInvestigateHintPushTime()) < IxAiConstants::kInvestigateDangerHintCooldownMs)
    {
        return;
    }

    const Fvector hint = agent.GetTacticalHintPosition();

    stalker.memory().danger().add(
        CDangerObject(
            nullptr,
            hint,
            now,
            CDangerObject::eDangerTypeEnemySound,
            CDangerObject::eDangerPerceiveTypeSound));

    agent.SetInvestigateHintPushTime(now);
    IxAiStackTelemetry_AddTacticHintPush(1u);
}

void IxAiTacticsSystem::TryPublishCoverDangerHint(
    CAI_Stalker& stalker,
    IxAiAgent& agent,
    const Fvector& threatPosition,
    u32 framePhase)
{
    if (!IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::CoverFeedDangerHint))
    {
        return;
    }

    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        return;
    }

    const IxAiAlertLevel alert = agent.GetAlertLevel();

    if (alert != IxAiAlertLevel::Search && alert != IxAiAlertLevel::Combat)
    {
        return;
    }

    const u32 interval = std::max(1u, g_ixAiRuntimeTuning.coverHintIntervalFrames);

    if (((framePhase + stalker.ID()) % interval) != 0u)
    {
        return;
    }

    const u32 now = Device.dwTimeGlobal;

    if ((now - agent.GetCoverHintDangerPushTime()) < g_ixAiRuntimeTuning.coverHintDangerCooldownMs)
    {
        return;
    }

    const Fvector self = stalker.Position();
    Fvector toThreatUnit{};
    Fvector lateral{};
    f32 dist{};

    if (!IxAiTacticsHelper::ComputeThreatHorizontalBasis(self, threatPosition, toThreatUnit, lateral, dist))
    {
        return;
    }

    (void)dist;

    Fvector coverHint = self;
    coverHint.mad(lateral, IxAiConstants::kTacticsCoverLateralScale);
    coverHint.mad(toThreatUnit, IxAiConstants::kTacticsCoverBackAlongThreatScale);

    stalker.memory().danger().add(
        CDangerObject(
            nullptr,
            coverHint,
            now,
            CDangerObject::eDangerTypeEnemySound,
            CDangerObject::eDangerPerceiveTypeSound));

    agent.SetCoverHintDangerPushTime(now);
    IxAiStackTelemetry_AddCoverHintPush(1u);
}
