#include "StdAfx.h"

#include "../../xrEngine/device.h"
#include "../Actor.h"
#include "../danger_object.h"
#include "../danger_manager.h"
#include "../memory_manager.h"
#include "../ai/stalker/ai_stalker.h"
#include "IxAiAgent.h"
#include "IxAiConstants.h"
#include "IxAiStackApi.h"
#include "IxAiStackTelemetry.h"
#include "IxAiStackTuning.h"
#include "IxAiStalkerLegacyOutput.h"

namespace
{
    struct LegacyIxToDangerPerceiveRow final
    {
        IxAiPerceptionEventType ixType;
        CDangerObject::EDangerPerceiveType perceive;
    };

    static const LegacyIxToDangerPerceiveRow kIxToLegacyPerceiveTable[] = {
        {IxAiPerceptionEventType::VisualPlayer, CDangerObject::eDangerPerceiveTypeVisual},
        {IxAiPerceptionEventType::VisualCorpse, CDangerObject::eDangerPerceiveTypeVisual},
        {IxAiPerceptionEventType::VisualBlood, CDangerObject::eDangerPerceiveTypeVisual},
        {IxAiPerceptionEventType::LightSource, CDangerObject::eDangerPerceiveTypeVisual},
    };

    CDangerObject::EDangerPerceiveType DangerPerceiveFromIxEventType(IxAiPerceptionEventType type)
    {
        for (const LegacyIxToDangerPerceiveRow& row : kIxToLegacyPerceiveTable)
        {
            if (row.ixType == type)
            {
                return row.perceive;
            }
        }

        return CDangerObject::eDangerPerceiveTypeSound;
    }

    struct LegacyCombatBridgeSpec final
    {
        CDangerObject::EDangerType dangerType;
        CDangerObject::EDangerPerceiveType perceiveType;
    };

    static constexpr LegacyCombatBridgeSpec kCombatBridgeSpec = {
        CDangerObject::eDangerTypeAttackSound,
        CDangerObject::eDangerPerceiveTypeSound,
    };

    struct LegacyInvestigationBridgeSpec final
    {
        CDangerObject::EDangerType dangerType;
    };

    static constexpr LegacyInvestigationBridgeSpec kInvestigationBridgeSpec = {
        CDangerObject::eDangerTypeEnemySound,
    };

    using LegacyFocusResolverFn = bool (*)(
        const IxAiAgent& agent,
        Fvector& outPosition,
        CDangerObject::EDangerPerceiveType& outPerceive);

    bool LegacyResolveFocusWorkingBelief(
        const IxAiAgent& agent,
        Fvector& outPosition,
        CDangerObject::EDangerPerceiveType& outPerceive)
    {
        const IxAiMemoryModel& memory = agent.GetMemoryModel();
        const u32 beliefCount = memory.GetWorkingBeliefCount();
        f32 bestConfidence = -1.f;
        u32 bestBeliefIndex = 0u;
        bool haveBelief = false;

        for (u32 beliefIndex = 0; beliefIndex < beliefCount; ++beliefIndex)
        {
            const IxAiBeliefGrain& grain = memory.GetWorkingBelief(beliefIndex);

            if (grain._confidence > bestConfidence)
            {
                bestConfidence = grain._confidence;
                bestBeliefIndex = beliefIndex;
                haveBelief = true;
            }
        }

        if (!haveBelief || bestConfidence < IxAiConstants::kWorkingBeliefEpsilon)
        {
            return false;
        }

        const IxAiBeliefGrain& pick = memory.GetWorkingBelief(bestBeliefIndex);
        outPosition = pick._position;
        outPerceive = DangerPerceiveFromIxEventType(pick._type);
        return true;
    }

    bool LegacyResolveFocusSensorySlot(
        const IxAiAgent& agent,
        Fvector& outPosition,
        CDangerObject::EDangerPerceiveType& outPerceive)
    {
        const IxAiMemoryModel& memory = agent.GetMemoryModel();
        const u32 slotCount = memory.GetSensorySlotCount();

        if (slotCount == 0u)
        {
            return false;
        }

        f32 bestStrength = -1.f;
        u32 bestIndex = 0u;

        for (u32 slotIndex = 0; slotIndex < slotCount; ++slotIndex)
        {
            const IxAiMemorySlot& slot = memory.GetSensorySlot(slotIndex);

            if (slot._strength > bestStrength)
            {
                bestStrength = slot._strength;
                bestIndex = slotIndex;
            }
        }

        const IxAiMemorySlot& pick = memory.GetSensorySlot(bestIndex);
        outPosition = pick._position;
        outPerceive = DangerPerceiveFromIxEventType(pick._type);
        return true;
    }

    bool LegacyResolveFocusLastFocus(
        const IxAiAgent& agent,
        Fvector& outPosition,
        CDangerObject::EDangerPerceiveType& outPerceive)
    {
        outPerceive = CDangerObject::eDangerPerceiveTypeSound;

        if (!agent.HasLastFocus())
        {
            return false;
        }

        outPosition = agent.GetLastFocusPosition();
        return true;
    }

    static const LegacyFocusResolverFn kAuthoritativeFocusChain[] = {
        &LegacyResolveFocusWorkingBelief,
        &LegacyResolveFocusSensorySlot,
        &LegacyResolveFocusLastFocus,
    };

    bool TryResolveBridgeDangerFocus(const IxAiAgent& agent, Fvector& outPosition, CDangerObject::EDangerPerceiveType& outPerceive)
    {
        outPerceive = CDangerObject::eDangerPerceiveTypeSound;

        if (IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::MemoryAuthoritative))
        {
            for (LegacyFocusResolverFn resolver : kAuthoritativeFocusChain)
            {
                if (resolver(agent, outPosition, outPerceive))
                {
                    return true;
                }
            }

            return false;
        }

        return LegacyResolveFocusLastFocus(agent, outPosition, outPerceive);
    }

    f32 BridgeDangerCooldownSeconds(IxAiAlertLevel alert)
    {
        if (alert == IxAiAlertLevel::Suspicious)
        {
            return g_ixAiRuntimeTuning.bridgeSuspiciousCooldownSeconds;
        }

        return g_ixAiRuntimeTuning.bridgeSearchCooldownSeconds;
    }

    void ApplyCombatBridge(CAI_Stalker& stalker, IxAiAgent& agent, CEntityAlive* enemyAlive, u32 now)
    {
        if (enemyAlive == nullptr || !stalker.is_relation_enemy(enemyAlive))
        {
            return;
        }

        const u32 lastCombat = agent.GetBridgeLastCombatPushTime();

        if (now - lastCombat <= (u32)(g_ixAiRuntimeTuning.bridgeCombatCooldownSeconds * 1000.f))
        {
            return;
        }

        stalker.memory().make_object_visible_somewhen(enemyAlive);
        stalker.memory().enemy().set_enemy(enemyAlive);
        stalker.memory().danger().add(
            CDangerObject(
                enemyAlive,
                enemyAlive->Position(),
                now,
                kCombatBridgeSpec.dangerType,
                kCombatBridgeSpec.perceiveType));
        agent.SetBridgeLastCombatPushTime(now);
        IxAiStackTelemetry_AddBridgePush(1u);
    }

    void ApplyInvestigationBridge(
        CAI_Stalker& stalker,
        IxAiAgent& agent,
        CEntityAlive* enemyAlive,
        IxAiAlertLevel alert,
        u32 now)
    {
        Fvector dangerFocus{};
        CDangerObject::EDangerPerceiveType dangerPerceive = CDangerObject::eDangerPerceiveTypeSound;

        if (!TryResolveBridgeDangerFocus(agent, dangerFocus, dangerPerceive))
        {
            return;
        }

        const u32 lastDanger = agent.GetBridgeLastDangerPushTime();
        const f32 cooldownSec = BridgeDangerCooldownSeconds(alert);

        if (now - lastDanger < (u32)(cooldownSec * 1000.f))
        {
            return;
        }

        const CEntityAlive* dangerSubject =
            (enemyAlive != nullptr && stalker.is_relation_enemy(enemyAlive)) ? enemyAlive : nullptr;

        stalker.memory().danger().add(
            CDangerObject(
                dangerSubject,
                dangerFocus,
                now,
                kInvestigationBridgeSpec.dangerType,
                dangerPerceive));

        agent.SetBridgeLastDangerPushTime(now);
        IxAiStackTelemetry_AddBridgePush(1u);
    }
} // namespace

void IxAiStalkerLegacyOutput_Apply(CAI_Stalker& stalker, IxAiAgent& agent)
{
    if (!IxAiStackApi::IsLegacyOutputAllowed())
    {
        return;
    }

    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        return;
    }

    const IxAiAlertLevel alert = agent.GetAlertLevel();
    const u32 now = Device.dwTimeGlobal;

    CActor* actor = Actor();
    CEntityAlive* enemyAlive = nullptr;

    if (actor != nullptr && actor->g_Alive() && !actor->getDestroy())
    {
        enemyAlive = actor;
    }

    switch (alert)
    {
    case IxAiAlertLevel::Combat:
        ApplyCombatBridge(stalker, agent, enemyAlive, now);
        return;

    case IxAiAlertLevel::Suspicious:
    case IxAiAlertLevel::Search:
        ApplyInvestigationBridge(stalker, agent, enemyAlive, alert, now);
        return;

    default:
        return;
    }
}
