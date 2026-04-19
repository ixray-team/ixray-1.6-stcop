#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "IxAiTypes.h"

enum class IxAiBtSpecNodeKind : u8
{
    Selector,
    Sequence,
    ConditionAlertGte,
    ConditionAlertLt,
    ActionNoop,
    ActionClearTacticalHint,
    ActionApplyStrongestMemoryHint,
    ConditionHasInvestigateMemory,
    ActionApplyInvestigateMemoryHint
};

struct IxAiBtSpecNode final
{
    IxAiBtSpecNodeKind _kind{IxAiBtSpecNodeKind::ActionNoop};
    u8 _childArity{};
    IxAiAlertLevel _threshold{IxAiAlertLevel::Vigilant};
};

constexpr u32 kIxAiBtMaxSpecNodes = 96u;

bool IxAiBtValidatePreorderSpec(const xr_vector<IxAiBtSpecNode>& specs);
