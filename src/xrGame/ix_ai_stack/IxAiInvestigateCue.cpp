#include "StdAfx.h"

#include "IxAiAgent.h"
#include "IxAiConstants.h"
#include "IxAiInvestigateCue.h"
#include "IxAiMemoryModel.h"
#include "IxAiTypes.h"

bool IxAiInvestigateCue_IsRelevantType(IxAiPerceptionEventType type)
{
    switch (type)
    {
    case IxAiPerceptionEventType::SoundPhysics:
    case IxAiPerceptionEventType::SoundExplosion:
    case IxAiPerceptionEventType::SoundBoltImpact:
    case IxAiPerceptionEventType::SoundItemFumble:
    case IxAiPerceptionEventType::SoundWeaponSurface:
    case IxAiPerceptionEventType::SquadAllyWounded:
    case IxAiPerceptionEventType::SquadCombatEngaged:
        return true;
    default:
        return false;
    }
}

bool IxAiInvestigateCue_PickStrongestWorldPosition(const IxAiAgent& agent, Fvector& outPosition)
{
    const f32 minScore = IxAiConstants::kInvestigateMemoryMinStrength;
    const IxAiMemoryModel& memory = agent.GetMemoryModel();
    f32 bestScore = 0.f;
    bool hasBest = false;
    Fvector bestPosition{};

    const u32 beliefCount = memory.GetWorkingBeliefCount();

    for (u32 beliefIndex = 0; beliefIndex < beliefCount; ++beliefIndex)
    {
        const IxAiBeliefGrain& grain = memory.GetWorkingBelief(beliefIndex);

        if (!IxAiInvestigateCue_IsRelevantType(grain._type))
        {
            continue;
        }

        if (grain._confidence < minScore)
        {
            continue;
        }

        if (!hasBest || grain._confidence > bestScore)
        {
            bestScore = grain._confidence;
            bestPosition = grain._position;
            hasBest = true;
        }
    }

    const u32 slotCount = memory.GetSensorySlotCount();

    for (u32 slotIndex = 0; slotIndex < slotCount; ++slotIndex)
    {
        const IxAiMemorySlot& slot = memory.GetSensorySlot(slotIndex);

        if (!IxAiInvestigateCue_IsRelevantType(slot._type))
        {
            continue;
        }

        if (slot._strength < minScore)
        {
            continue;
        }

        if (!hasBest || slot._strength > bestScore)
        {
            bestScore = slot._strength;
            bestPosition = slot._position;
            hasBest = true;
        }
    }

    if (!hasBest)
    {
        return false;
    }

    outPosition = bestPosition;
    return true;
}

bool IxAiInvestigateCue_HasAny(const IxAiAgent& agent)
{
    Fvector unused{};
    return IxAiInvestigateCue_PickStrongestWorldPosition(agent, unused);
}
