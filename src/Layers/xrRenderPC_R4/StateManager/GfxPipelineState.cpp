#include "stdafx.h"
#include "GfxPipelineState.h"
#include "../dx10StateUtils.h"

GfxPipelineState* GfxPipelineState::Create(SimulatorStates& Code)
{
	GfxPipelineState* State = new GfxPipelineState();
	Code.UpdateState(*State);

	dx10StateUtils::ResetDescription(State->Raster);
	dx10StateUtils::ResetDescription(State->Depth);
	dx10StateUtils::ResetDescription(State->Blend);
	Code.UpdateDesc(State->Raster);
	Code.UpdateDesc(State->Depth);
	Code.UpdateDesc(State->Blend);
	dx10StateUtils::ValidateState(State->Depth);
	dx10StateUtils::ValidateState(State->Blend);

	State->RasterState = GRHI->GetState(State->Raster);
	State->DepthState = GRHI->GetState(State->Depth);
	State->BlendState = GRHI->GetState(State->Blend);
	return State;
}

void GfxPipelineState::Release()
{
	GfxPipelineState* State = this;
	xr_delete(State);
}
