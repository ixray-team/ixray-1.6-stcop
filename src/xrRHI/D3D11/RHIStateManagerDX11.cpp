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
