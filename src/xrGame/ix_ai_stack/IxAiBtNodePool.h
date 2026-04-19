#pragma once

#include "../../xrCore/_stl_extensions.h"

#include "IxAiTypes.h"

class IxAiBtSelector;
class IxAiBtSequence;
class IxAiBtConditionAlertGte;
class IxAiBtConditionAlertLt;
class IxAiBtActionNoop;
class IxAiBtActionClearTacticalHint;
class IxAiBtActionApplyStrongestMemoryHint;
class IxAiBtConditionHasInvestigateMemory;
class IxAiBtActionApplyInvestigateMemoryHint;

class IxAiBtNodePool final
{
public:
    IxAiBtNodePool();
    ~IxAiBtNodePool();

    IxAiBtNodePool(const IxAiBtNodePool&) = delete;
    IxAiBtNodePool& operator=(const IxAiBtNodePool&) = delete;

    void Drain();

    IxAiBtSelector* AcquireSelector();
    void ReleaseSelector(IxAiBtSelector* node);

    IxAiBtSequence* AcquireSequence();
    void ReleaseSequence(IxAiBtSequence* node);

    IxAiBtConditionAlertGte* AcquireConditionAlertGte(IxAiAlertLevel threshold);
    void ReleaseConditionAlertGte(IxAiBtConditionAlertGte* node);

    IxAiBtConditionAlertLt* AcquireConditionAlertLt(IxAiAlertLevel threshold);
    void ReleaseConditionAlertLt(IxAiBtConditionAlertLt* node);

    IxAiBtActionNoop* AcquireActionNoop();
    void ReleaseActionNoop(IxAiBtActionNoop* node);

    IxAiBtActionClearTacticalHint* AcquireActionClearTacticalHint();
    void ReleaseActionClearTacticalHint(IxAiBtActionClearTacticalHint* node);

    IxAiBtActionApplyStrongestMemoryHint* AcquireActionApplyStrongestMemoryHint();
    void ReleaseActionApplyStrongestMemoryHint(IxAiBtActionApplyStrongestMemoryHint* node);

    IxAiBtConditionHasInvestigateMemory* AcquireConditionHasInvestigateMemory();
    void ReleaseConditionHasInvestigateMemory(IxAiBtConditionHasInvestigateMemory* node);

    IxAiBtActionApplyInvestigateMemoryHint* AcquireActionApplyInvestigateMemoryHint();
    void ReleaseActionApplyInvestigateMemoryHint(IxAiBtActionApplyInvestigateMemoryHint* node);

private:
    template <typename TNode>
    static void DrainList(xr_vector<TNode*>& freeList);

    template <typename TNode>
    static void ReleaseNode(xr_vector<TNode*>& freeList, TNode* node);

    template <typename TNode>
    static TNode* AcquireComposite(xr_vector<TNode*>& freeList);

    template <typename TNode>
    static TNode* AcquireThreshold(xr_vector<TNode*>& freeList, IxAiAlertLevel threshold);

    template <typename TNode>
    static TNode* AcquireSimple(xr_vector<TNode*>& freeList);

    xr_vector<IxAiBtSelector*> _freeSelectors{};
    xr_vector<IxAiBtSequence*> _freeSequences{};
    xr_vector<IxAiBtConditionAlertGte*> _freeConditionAlertGte{};
    xr_vector<IxAiBtConditionAlertLt*> _freeConditionAlertLt{};
    xr_vector<IxAiBtActionNoop*> _freeActionNoop{};
    xr_vector<IxAiBtActionClearTacticalHint*> _freeActionClearTacticalHint{};
    xr_vector<IxAiBtActionApplyStrongestMemoryHint*> _freeActionApplyStrongestMemoryHint{};
    xr_vector<IxAiBtConditionHasInvestigateMemory*> _freeConditionHasInvestigateMemory{};
    xr_vector<IxAiBtActionApplyInvestigateMemoryHint*> _freeActionApplyInvestigateMemoryHint{};
};
