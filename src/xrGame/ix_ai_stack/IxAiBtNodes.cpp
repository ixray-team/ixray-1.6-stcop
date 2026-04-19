#include "StdAfx.h"

#include "IxAiAgent.h"
#include "IxAiBtNodes.h"
#include "IxAiBtNodePool.h"
#include "IxAiConstants.h"
#include "IxAiInvestigateCue.h"

namespace
{
    bool IsAlertLevelAtLeast(IxAiAlertLevel current, IxAiAlertLevel threshold)
    {
        if (current == IxAiAlertLevel::None || threshold == IxAiAlertLevel::None)
        {
            return false;
        }

        return (u8)current >= (u8)threshold;
    }

    bool IsAlertLevelBelow(IxAiAlertLevel current, IxAiAlertLevel threshold)
    {
        if (current == IxAiAlertLevel::None || threshold == IxAiAlertLevel::None)
        {
            return false;
        }

        return (u8)current < (u8)threshold;
    }
}

IxAiBtNode::~IxAiBtNode() = default;

IxAiBtConditionAlertGte::IxAiBtConditionAlertGte(IxAiAlertLevel threshold)
    : _threshold(threshold)
{
}

IxAiBtConditionAlertGte::~IxAiBtConditionAlertGte() = default;

IxAiBtStatus IxAiBtConditionAlertGte::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    return IsAlertLevelAtLeast(ctx.agent->GetAlertLevel(), _threshold) ? IxAiBtStatus::Success : IxAiBtStatus::Failure;
}

void IxAiBtConditionAlertGte::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseConditionAlertGte(this);
}

IxAiBtConditionAlertLt::IxAiBtConditionAlertLt(IxAiAlertLevel threshold)
    : _threshold(threshold)
{
}

IxAiBtConditionAlertLt::~IxAiBtConditionAlertLt() = default;

IxAiBtStatus IxAiBtConditionAlertLt::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    return IsAlertLevelBelow(ctx.agent->GetAlertLevel(), _threshold) ? IxAiBtStatus::Success : IxAiBtStatus::Failure;
}

void IxAiBtConditionAlertLt::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseConditionAlertLt(this);
}

IxAiBtActionNoop::~IxAiBtActionNoop() = default;

IxAiBtStatus IxAiBtActionNoop::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)ctx;
    (void)deltaTime;
    return IxAiBtStatus::Success;
}

void IxAiBtActionNoop::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseActionNoop(this);
}

IxAiBtActionClearTacticalHint::~IxAiBtActionClearTacticalHint() = default;

IxAiBtStatus IxAiBtActionClearTacticalHint::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    ctx.agent->SetTacticalHint(Fvector().set(0.f, 0.f, 0.f), false);
    return IxAiBtStatus::Success;
}

void IxAiBtActionClearTacticalHint::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseActionClearTacticalHint(this);
}

IxAiBtActionApplyStrongestMemoryHint::~IxAiBtActionApplyStrongestMemoryHint() = default;

IxAiBtStatus IxAiBtActionApplyStrongestMemoryHint::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    if (ctx.agent->HasTacticalHint())
    {
        return IxAiBtStatus::Success;
    }

    f32 bestScore = 0.f;
    Fvector bestPosition{};
    bool hasBest = false;

    const IxAiMemoryModel& memory = ctx.agent->GetMemoryModel();
    const u32 beliefCount = memory.GetWorkingBeliefCount();

    for (u32 beliefIndex = 0; beliefIndex < beliefCount; ++beliefIndex)
    {
        const IxAiBeliefGrain& grain = memory.GetWorkingBelief(beliefIndex);

        if (grain._confidence > bestScore)
        {
            bestScore = grain._confidence;
            bestPosition = grain._position;
            hasBest = true;
        }
    }

    if (!hasBest || bestScore < IxAiConstants::kWorkingBeliefHintThreshold)
    {
        bestScore = 0.f;
        hasBest = false;

        const u32 slotCount = memory.GetSensorySlotCount();

        for (u32 slotIndex = 0; slotIndex < slotCount; ++slotIndex)
        {
            const IxAiMemorySlot& slot = memory.GetSensorySlot(slotIndex);

            if (slot._strength > bestScore)
            {
                bestScore = slot._strength;
                bestPosition = slot._position;
                hasBest = true;
            }
        }
    }

    if (!hasBest || bestScore < IxAiConstants::kMemoryPushThreshold)
    {
        return IxAiBtStatus::Failure;
    }

    ctx.agent->SetTacticalHint(bestPosition, true);
    return IxAiBtStatus::Success;
}

void IxAiBtActionApplyStrongestMemoryHint::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseActionApplyStrongestMemoryHint(this);
}

IxAiBtConditionHasInvestigateMemory::~IxAiBtConditionHasInvestigateMemory() = default;

IxAiBtStatus IxAiBtConditionHasInvestigateMemory::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    return IxAiInvestigateCue_HasAny(*ctx.agent) ? IxAiBtStatus::Success : IxAiBtStatus::Failure;
}

void IxAiBtConditionHasInvestigateMemory::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseConditionHasInvestigateMemory(this);
}

IxAiBtActionApplyInvestigateMemoryHint::~IxAiBtActionApplyInvestigateMemoryHint() = default;

IxAiBtStatus IxAiBtActionApplyInvestigateMemoryHint::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    (void)deltaTime;

    if (ctx.agent == nullptr)
    {
        return IxAiBtStatus::Failure;
    }

    Fvector worldPosition{};

    if (!IxAiInvestigateCue_PickStrongestWorldPosition(*ctx.agent, worldPosition))
    {
        return IxAiBtStatus::Failure;
    }

    ctx.agent->SetTacticalHint(worldPosition, true);
    return IxAiBtStatus::Success;
}

void IxAiBtActionApplyInvestigateMemoryHint::RecycleForPool(IxAiBtNodePool& pool)
{
    pool.ReleaseActionApplyInvestigateMemoryHint(this);
}

IxAiBtSequence::~IxAiBtSequence()
{
    for (IxAiBtNode* child : _children)
    {
        xr_delete(child);
    }

    _children.clear();
}

void IxAiBtSequence::ReserveChildren(u32 capacity)
{
    _children.reserve(capacity);
}

void IxAiBtSequence::AddChild(IxAiBtNode* child)
{
    _children.push_back(child);
}

void IxAiBtSequence::RecycleForPool(IxAiBtNodePool& pool)
{
    for (IxAiBtNode* child : _children)
    {
        child->RecycleForPool(pool);
    }

    _children.clear();
    _cursor = 0;
    pool.ReleaseSequence(this);
}

IxAiBtStatus IxAiBtSequence::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    while (_cursor < _children.size())
    {
        const IxAiBtStatus status = _children[_cursor]->Tick(ctx, deltaTime);

        if (status == IxAiBtStatus::Failure)
        {
            _cursor = 0;
            return IxAiBtStatus::Failure;
        }

        if (status == IxAiBtStatus::Running)
        {
            return IxAiBtStatus::Running;
        }

        ++_cursor;
    }

    _cursor = 0;
    return IxAiBtStatus::Success;
}

IxAiBtSelector::~IxAiBtSelector()
{
    for (IxAiBtNode* child : _children)
    {
        xr_delete(child);
    }

    _children.clear();
}

void IxAiBtSelector::ReserveChildren(u32 capacity)
{
    _children.reserve(capacity);
}

void IxAiBtSelector::AddChild(IxAiBtNode* child)
{
    _children.push_back(child);
}

void IxAiBtSelector::RecycleForPool(IxAiBtNodePool& pool)
{
    for (IxAiBtNode* child : _children)
    {
        child->RecycleForPool(pool);
    }

    _children.clear();
    _cursor = 0;
    pool.ReleaseSelector(this);
}

IxAiBtStatus IxAiBtSelector::Tick(IxAiBtContext& ctx, f32 deltaTime)
{
    for (u32 childIndex = _cursor; childIndex < _children.size(); ++childIndex)
    {
        const IxAiBtStatus status = _children[childIndex]->Tick(ctx, deltaTime);

        if (status == IxAiBtStatus::Success)
        {
            _cursor = 0;
            return IxAiBtStatus::Success;
        }

        if (status == IxAiBtStatus::Running)
        {
            _cursor = childIndex;
            return IxAiBtStatus::Running;
        }
    }

    _cursor = 0;
    return IxAiBtStatus::Failure;
}
