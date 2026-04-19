#include "StdAfx.h"

#include "IxAiBtNodePool.h"
#include "IxAiBtNodes.h"

IxAiBtNodePool::IxAiBtNodePool() = default;

IxAiBtNodePool::~IxAiBtNodePool()
{
    Drain();
}

template <typename TNode>
void IxAiBtNodePool::DrainList(xr_vector<TNode*>& freeList)
{
    for (TNode* node : freeList)
    {
        xr_delete(node);
    }

    freeList.clear();
}

template <typename TNode>
void IxAiBtNodePool::ReleaseNode(xr_vector<TNode*>& freeList, TNode* node)
{
    if (node == nullptr)
    {
        return;
    }

    freeList.push_back(node);
}

template <typename TNode>
TNode* IxAiBtNodePool::AcquireComposite(xr_vector<TNode*>& freeList)
{
    if (!freeList.empty())
    {
        TNode* node = freeList.back();
        freeList.pop_back();
        VERIFY(node->_children.empty());
        node->_cursor = 0;
        return node;
    }

    return new TNode();
}

template <typename TNode>
TNode* IxAiBtNodePool::AcquireThreshold(xr_vector<TNode*>& freeList, IxAiAlertLevel threshold)
{
    if (!freeList.empty())
    {
        TNode* node = freeList.back();
        freeList.pop_back();
        node->_threshold = threshold;
        return node;
    }

    return new TNode(threshold);
}

template <typename TNode>
TNode* IxAiBtNodePool::AcquireSimple(xr_vector<TNode*>& freeList)
{
    if (!freeList.empty())
    {
        TNode* node = freeList.back();
        freeList.pop_back();
        return node;
    }

    return new TNode();
}

void IxAiBtNodePool::Drain()
{
    DrainList(_freeSelectors);
    DrainList(_freeSequences);
    DrainList(_freeConditionAlertGte);
    DrainList(_freeConditionAlertLt);
    DrainList(_freeActionNoop);
    DrainList(_freeActionClearTacticalHint);
    DrainList(_freeActionApplyStrongestMemoryHint);
    DrainList(_freeConditionHasInvestigateMemory);
    DrainList(_freeActionApplyInvestigateMemoryHint);
}

IxAiBtSelector* IxAiBtNodePool::AcquireSelector()
{
    return AcquireComposite(_freeSelectors);
}

void IxAiBtNodePool::ReleaseSelector(IxAiBtSelector* node)
{
    ReleaseNode(_freeSelectors, node);
}

IxAiBtSequence* IxAiBtNodePool::AcquireSequence()
{
    return AcquireComposite(_freeSequences);
}

void IxAiBtNodePool::ReleaseSequence(IxAiBtSequence* node)
{
    ReleaseNode(_freeSequences, node);
}

IxAiBtConditionAlertGte* IxAiBtNodePool::AcquireConditionAlertGte(IxAiAlertLevel threshold)
{
    return AcquireThreshold(_freeConditionAlertGte, threshold);
}

void IxAiBtNodePool::ReleaseConditionAlertGte(IxAiBtConditionAlertGte* node)
{
    ReleaseNode(_freeConditionAlertGte, node);
}

IxAiBtConditionAlertLt* IxAiBtNodePool::AcquireConditionAlertLt(IxAiAlertLevel threshold)
{
    return AcquireThreshold(_freeConditionAlertLt, threshold);
}

void IxAiBtNodePool::ReleaseConditionAlertLt(IxAiBtConditionAlertLt* node)
{
    ReleaseNode(_freeConditionAlertLt, node);
}

IxAiBtActionNoop* IxAiBtNodePool::AcquireActionNoop()
{
    return AcquireSimple(_freeActionNoop);
}

void IxAiBtNodePool::ReleaseActionNoop(IxAiBtActionNoop* node)
{
    ReleaseNode(_freeActionNoop, node);
}

IxAiBtActionClearTacticalHint* IxAiBtNodePool::AcquireActionClearTacticalHint()
{
    return AcquireSimple(_freeActionClearTacticalHint);
}

void IxAiBtNodePool::ReleaseActionClearTacticalHint(IxAiBtActionClearTacticalHint* node)
{
    ReleaseNode(_freeActionClearTacticalHint, node);
}

IxAiBtActionApplyStrongestMemoryHint* IxAiBtNodePool::AcquireActionApplyStrongestMemoryHint()
{
    return AcquireSimple(_freeActionApplyStrongestMemoryHint);
}

void IxAiBtNodePool::ReleaseActionApplyStrongestMemoryHint(IxAiBtActionApplyStrongestMemoryHint* node)
{
    ReleaseNode(_freeActionApplyStrongestMemoryHint, node);
}

IxAiBtConditionHasInvestigateMemory* IxAiBtNodePool::AcquireConditionHasInvestigateMemory()
{
    return AcquireSimple(_freeConditionHasInvestigateMemory);
}

void IxAiBtNodePool::ReleaseConditionHasInvestigateMemory(IxAiBtConditionHasInvestigateMemory* node)
{
    ReleaseNode(_freeConditionHasInvestigateMemory, node);
}

IxAiBtActionApplyInvestigateMemoryHint* IxAiBtNodePool::AcquireActionApplyInvestigateMemoryHint()
{
    return AcquireSimple(_freeActionApplyInvestigateMemoryHint);
}

void IxAiBtNodePool::ReleaseActionApplyInvestigateMemoryHint(IxAiBtActionApplyInvestigateMemoryHint* node)
{
    ReleaseNode(_freeActionApplyInvestigateMemoryHint, node);
}

template void IxAiBtNodePool::DrainList<IxAiBtSelector>(xr_vector<IxAiBtSelector*>&);
template void IxAiBtNodePool::DrainList<IxAiBtSequence>(xr_vector<IxAiBtSequence*>&);
template void IxAiBtNodePool::DrainList<IxAiBtConditionAlertGte>(xr_vector<IxAiBtConditionAlertGte*>&);
template void IxAiBtNodePool::DrainList<IxAiBtConditionAlertLt>(xr_vector<IxAiBtConditionAlertLt*>&);
template void IxAiBtNodePool::DrainList<IxAiBtActionNoop>(xr_vector<IxAiBtActionNoop*>&);
template void IxAiBtNodePool::DrainList<IxAiBtActionClearTacticalHint>(xr_vector<IxAiBtActionClearTacticalHint*>&);
template void IxAiBtNodePool::DrainList<IxAiBtActionApplyStrongestMemoryHint>(xr_vector<IxAiBtActionApplyStrongestMemoryHint*>&);
template void IxAiBtNodePool::DrainList<IxAiBtConditionHasInvestigateMemory>(xr_vector<IxAiBtConditionHasInvestigateMemory*>&);
template void IxAiBtNodePool::DrainList<IxAiBtActionApplyInvestigateMemoryHint>(xr_vector<IxAiBtActionApplyInvestigateMemoryHint*>&);

template void IxAiBtNodePool::ReleaseNode<IxAiBtSelector>(xr_vector<IxAiBtSelector*>&, IxAiBtSelector*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtSequence>(xr_vector<IxAiBtSequence*>&, IxAiBtSequence*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtConditionAlertGte>(xr_vector<IxAiBtConditionAlertGte*>&, IxAiBtConditionAlertGte*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtConditionAlertLt>(xr_vector<IxAiBtConditionAlertLt*>&, IxAiBtConditionAlertLt*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtActionNoop>(xr_vector<IxAiBtActionNoop*>&, IxAiBtActionNoop*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtActionClearTacticalHint>(xr_vector<IxAiBtActionClearTacticalHint*>&, IxAiBtActionClearTacticalHint*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtActionApplyStrongestMemoryHint>(xr_vector<IxAiBtActionApplyStrongestMemoryHint*>&, IxAiBtActionApplyStrongestMemoryHint*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtConditionHasInvestigateMemory>(xr_vector<IxAiBtConditionHasInvestigateMemory*>&, IxAiBtConditionHasInvestigateMemory*);
template void IxAiBtNodePool::ReleaseNode<IxAiBtActionApplyInvestigateMemoryHint>(xr_vector<IxAiBtActionApplyInvestigateMemoryHint*>&, IxAiBtActionApplyInvestigateMemoryHint*);

template IxAiBtSelector* IxAiBtNodePool::AcquireComposite<IxAiBtSelector>(xr_vector<IxAiBtSelector*>&);
template IxAiBtSequence* IxAiBtNodePool::AcquireComposite<IxAiBtSequence>(xr_vector<IxAiBtSequence*>&);

template IxAiBtConditionAlertGte* IxAiBtNodePool::AcquireThreshold<IxAiBtConditionAlertGte>(xr_vector<IxAiBtConditionAlertGte*>&, IxAiAlertLevel);
template IxAiBtConditionAlertLt* IxAiBtNodePool::AcquireThreshold<IxAiBtConditionAlertLt>(xr_vector<IxAiBtConditionAlertLt*>&, IxAiAlertLevel);

template IxAiBtActionNoop* IxAiBtNodePool::AcquireSimple<IxAiBtActionNoop>(xr_vector<IxAiBtActionNoop*>&);
template IxAiBtActionClearTacticalHint* IxAiBtNodePool::AcquireSimple<IxAiBtActionClearTacticalHint>(xr_vector<IxAiBtActionClearTacticalHint*>&);
template IxAiBtActionApplyStrongestMemoryHint* IxAiBtNodePool::AcquireSimple<IxAiBtActionApplyStrongestMemoryHint>(xr_vector<IxAiBtActionApplyStrongestMemoryHint*>&);
template IxAiBtConditionHasInvestigateMemory* IxAiBtNodePool::AcquireSimple<IxAiBtConditionHasInvestigateMemory>(xr_vector<IxAiBtConditionHasInvestigateMemory*>&);
template IxAiBtActionApplyInvestigateMemoryHint* IxAiBtNodePool::AcquireSimple<IxAiBtActionApplyInvestigateMemoryHint>(xr_vector<IxAiBtActionApplyInvestigateMemoryHint*>&);
