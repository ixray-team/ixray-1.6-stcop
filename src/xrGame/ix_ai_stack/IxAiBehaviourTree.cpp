#include "StdAfx.h"

#include "IxAiAgent.h"
#include "IxAiBehaviourTree.h"
#include "IxAiBtTreeRegistry.h"

IxAiBehaviourTree::IxAiBehaviourTree()
    : _root(nullptr)
{
}

IxAiBehaviourTree::~IxAiBehaviourTree()
{
    if (_root != nullptr)
    {
        if (_lastRebuildUsedPool)
        {
            _root->RecycleForPool(_nodePool);
        }
        else
        {
            xr_delete(_root);
        }

        _root = nullptr;
    }
}

void IxAiBehaviourTree::RebuildRoot(const IxAiBehaviourProfile& profile)
{
    const bool usePool = profile._useBehaviourTreeNodePool;

    if (_root != nullptr)
    {
        if (_lastRebuildUsedPool)
        {
            _root->RecycleForPool(_nodePool);
        }
        else
        {
            xr_delete(_root);
        }

        _root = nullptr;
    }

    if (_lastRebuildUsedPool && !usePool)
    {
        _nodePool.Drain();
    }

    IxAiBtNodePool* poolPtr = usePool ? &_nodePool : nullptr;
    _root = IxAiBtBuildBehaviourTreeRoot(profile._behaviourKind, poolPtr);
    _lastRebuildUsedPool = usePool;
}

void IxAiBehaviourTree::Tick(IxAiAgent& agent, f32 deltaTime)
{
    if (_root == nullptr)
    {
        return;
    }

    IxAiBtContext ctx{};
    ctx.agent = &agent;
    ctx.tickCounter = ++_tickCounter;
    _root->Tick(ctx, deltaTime);
}
