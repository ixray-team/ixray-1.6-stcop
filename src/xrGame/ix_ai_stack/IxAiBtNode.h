#pragma once

#include "IxAiBehaviourTreeTypes.h"

class IxAiBtNodePool;

class IxAiBtNode
{
public:
    virtual ~IxAiBtNode();
    virtual IxAiBtStatus Tick(IxAiBtContext& ctx, f32 deltaTime) = 0;
    virtual void RecycleForPool(IxAiBtNodePool& pool) = 0;
};
