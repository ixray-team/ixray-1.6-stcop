#pragma once

#include "IxAiTypes.h"
#include "IxAiBtNode.h"
#include "IxAiBtNodePool.h"

class IxAiAgent;

class IxAiBehaviourTree final
{
public:
    IxAiBehaviourTree();
    ~IxAiBehaviourTree();

    IxAiBehaviourTree(const IxAiBehaviourTree&) = delete;
    IxAiBehaviourTree& operator=(const IxAiBehaviourTree&) = delete;

    void RebuildRoot(const IxAiBehaviourProfile& profile);
    void Tick(IxAiAgent& agent, f32 deltaTime);

private:
    IxAiBtNode* _root{};
    IxAiBtNodePool _nodePool{};
    u32 _tickCounter{};
    bool _lastRebuildUsedPool{};
};
