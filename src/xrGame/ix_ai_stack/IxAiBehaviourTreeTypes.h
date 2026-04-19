#pragma once

#include "../../xrCore/_types.h"

class IxAiAgent;

enum class IxAiBtStatus : u8
{
    Success,
    Failure,
    Running
};

struct IxAiBtContext final
{
    IxAiAgent* agent{};
    u32 tickCounter{};
};
