#pragma once

#include "../../xrCore/_stl_extensions.h"

#include "IxAiBtNode.h"
#include "IxAiTypes.h"

class IxAiBtConditionAlertGte final : public IxAiBtNode
{
public:
    explicit IxAiBtConditionAlertGte(IxAiAlertLevel threshold);
    ~IxAiBtConditionAlertGte() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;

    IxAiAlertLevel _threshold{IxAiAlertLevel::Vigilant};
};

class IxAiBtConditionAlertLt final : public IxAiBtNode
{
public:
    explicit IxAiBtConditionAlertLt(IxAiAlertLevel threshold);
    ~IxAiBtConditionAlertLt() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;

    IxAiAlertLevel _threshold{IxAiAlertLevel::Combat};
};

class IxAiBtActionNoop final : public IxAiBtNode
{
public:
    ~IxAiBtActionNoop() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;
};

class IxAiBtActionClearTacticalHint final : public IxAiBtNode
{
public:
    ~IxAiBtActionClearTacticalHint() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;
};

class IxAiBtActionApplyStrongestMemoryHint final : public IxAiBtNode
{
public:
    ~IxAiBtActionApplyStrongestMemoryHint() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;
};

class IxAiBtConditionHasInvestigateMemory final : public IxAiBtNode
{
public:
    ~IxAiBtConditionHasInvestigateMemory() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;
};

class IxAiBtActionApplyInvestigateMemoryHint final : public IxAiBtNode
{
public:
    ~IxAiBtActionApplyInvestigateMemoryHint() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;
};

class IxAiBtSequence final : public IxAiBtNode
{
public:
    ~IxAiBtSequence() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;

    void ReserveChildren(u32 capacity);
    void AddChild(IxAiBtNode* child);
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;

    xr_vector<IxAiBtNode*> _children{};
    u32 _cursor{};
};

class IxAiBtSelector final : public IxAiBtNode
{
public:
    ~IxAiBtSelector() override;
    IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) override;

    void ReserveChildren(u32 capacity);
    void AddChild(IxAiBtNode* child);
    void RecycleForPool(IxAiBtNodePool& pool) override;

private:
    friend class IxAiBtNodePool;

    xr_vector<IxAiBtNode*> _children{};
    u32 _cursor{};
};
