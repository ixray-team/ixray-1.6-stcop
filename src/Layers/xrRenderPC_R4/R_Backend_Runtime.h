#ifndef R_BACKEND_RUNTIMEH
#define R_BACKEND_RUNTIMEH
#pragma once

#include "SH_Texture.h"
#include "SH_Matrix.h"
#include "SH_Constant.h"
#include "SH_RT.h"

#include "dx10R_Backend_Runtime.h"
#include "StateManager/GfxPipelineState.h"

ICF void	GfxCmdList::SetStates		(GfxPipelineState* _state)
{
	state = _state;
	Raster = _state->Raster;
	Depth = _state->Depth;
	Blend = _state->Blend;
	States[0] = _state->RasterState;
	States[1] = _state->DepthState;
	States[2] = _state->BlendState;
	StatesDirty = true;
	if (_state->StencilRef != u32(-1))
	{
		StencilRef = _state->StencilRef;
	}
	if (ScissorOverride && !Raster.ScissorEnable)
	{
		EditRaster().ScissorEnable = true;
	}
	if (FillOverride && Raster.FillMode != FillOverrideMode)
	{
		EditRaster().FillMode = FillOverrideMode;
	}
	SetAlphaRef(_state->AlphaRef);
}

// FFP texture transform emulation (D3DTS_TEXTURE0)
IC void GfxCmdList::SetMatrices			(SMatrixList*	_M)
{
	M = _M;
	CMatrix* mat = (M && !M->empty()) ? (*M)[0]._get() : nullptr;
	if (mat && mat->dwMode == CMatrix::modeTCM)
	{
		mat->Calculate();
		Constants.Write<CBVisor>().m_tc_xform = mat->xform;
	}
	else
	{
		Constants.Write<CBVisor>().m_tc_xform = Fidentity;
	}
}

IC void GfxCmdList::SetPipeline(SPass& P)
{
	SetStates(P.state);
	SetPS(P.ps);
	SetVS(P.vs);
	SetGS(P.gs);
	SetHS(P.hs);
	SetDS(P.ds);
	SetCS(P.cs);
}

IC void GfxCmdList::SetPass(SPass& P)
{
	SetPipeline(P);
	SetConstantTable(P.constants);
	SetTextures(P.T);
	if (EditorMode)
	{
		SetMatrices(P.M);
	}
}

IC void GfxCmdList::SetPass(ShaderElement* S, u32 pass)
{
	SetPass(*S->passes[pass]);
}

ICF void GfxCmdList::SetPass(Shader* S, u32 pass)
{
	if (S == nullptr)
	{
		Msg("! Not found shader in library!");
		return;
	}

	SetPass(S->E[0], pass);
}

#endif
