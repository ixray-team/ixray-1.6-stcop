#pragma once
#include "LogicMetainfo.h"

struct FNodeRenderDesc
{
	xr_string Title;
	FColor Color;

	xr_vector<xr_string> Inputs;
	xr_vector<xr_string> Outputs;

	std::function<void(const FState&)> DrawBody;
};

struct FEventNode
{
    xr_string EventName;
    xr_string DisplayName;
    FTransition LinkedTransition;
    ImVec2 Position;
    ed::NodeId Owner;

    float TimerValue = 0.0f;
    int EventIndex;

    xr_string InfoName;

    ed::PinId InputPinId;
    ed::PinId OutputPinId;

    xr_vector<FParsedCondition> Conditions;
    xr_vector<FParsedEffect> Effects;
};

FNodeRenderDesc GetStateRenderDesc(FState& State);