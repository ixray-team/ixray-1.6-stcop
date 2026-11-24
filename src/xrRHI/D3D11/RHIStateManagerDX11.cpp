/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "RHIStateManagerDX11.h"

RHIStateManagerDX11::RHIStateManagerDX11(ID3D11DeviceContext* InContext) :
	StateCache(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice))
{
	D3DContext = InContext ? InContext : static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	Reset();
}

void RHIStateManagerDX11::Reset()
{
	RasterizerState = nullptr;
	DepthStencilState = nullptr;
	BlendState = nullptr;
	StencilRef = 0;
	RasterizerDirty = true;
	DepthStencilDirty = true;
	BlendDirty = true;
}

void RHIStateManagerDX11::Restore()
{
	RasterizerDirty = true;
	DepthStencilDirty = true;
	BlendDirty = true;
}

void RHIStateManagerDX11::SetContext(ID3D11DeviceContext* InContext)
{
	D3DContext = InContext;
}

void RHIStateManagerDX11::Apply()
{
	R_ASSERT(D3DContext);
	if (RasterizerDirty)
	{
		D3DContext->RSSetState(RasterizerState);
		RasterizerDirty = false;
	}
	if (DepthStencilDirty)
	{
		D3DContext->OMSetDepthStencilState(DepthStencilState, StencilRef);
		DepthStencilDirty = false;
	}
	if (BlendDirty)
	{
		const FLOAT BlendFactor[4] = {};
		D3DContext->OMSetBlendState(BlendState, BlendFactor, 0xffffffff);
		BlendDirty = false;
	}
}

void RHIStateManagerDX11::SetRasterizerState(void* NewState)
{
	RasterizerDirty |= RasterizerState != NewState;
	RasterizerState = static_cast<ID3D11RasterizerState*>(NewState);
}

void RHIStateManagerDX11::SetDepthStencilState(void* NewState)
{
	DepthStencilDirty |= DepthStencilState != NewState;
	DepthStencilState = static_cast<ID3D11DepthStencilState*>(NewState);
}

void RHIStateManagerDX11::SetBlendState(void* NewState)
{
	BlendDirty |= BlendState != NewState;
	BlendState = static_cast<ID3D11BlendState*>(NewState);
}

void RHIStateManagerDX11::SetStencilRef(u32 NewStencilRef)
{
	DepthStencilDirty |= StencilRef != NewStencilRef;
	StencilRef = NewStencilRef;
}

void* RHIStateManagerDX11::GetCache(ERHI_STATE_CACHE_TYPE Type, void* Desc)
{
	switch (Type)
	{
		case ERHI_STATE_CACHE_TYPE::RS: return StateCache.GetRasterizerState(*(D3D11_RASTERIZER_DESC*)Desc);
		case ERHI_STATE_CACHE_TYPE::DS: return StateCache.GetDepthStencilState(*(D3D11_DEPTH_STENCIL_DESC*)Desc);
		case ERHI_STATE_CACHE_TYPE::BS: return StateCache.GetBlendState(*(D3D11_BLEND_DESC*)Desc);
	}
	return nullptr;
}
