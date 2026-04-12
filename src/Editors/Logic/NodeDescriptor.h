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

FNodeRenderDesc GetStateRenderDesc(FState& State);