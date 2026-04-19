#pragma once

#include "IxAiTypes.h"

class IxAiAgent;

bool IxAiInvestigateCue_IsRelevantType(IxAiPerceptionEventType type);
bool IxAiInvestigateCue_HasAny(const IxAiAgent& agent);
bool IxAiInvestigateCue_PickStrongestWorldPosition(const IxAiAgent& agent, Fvector& outPosition);
