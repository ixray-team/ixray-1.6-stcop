#include "RHIStateManagerDX11.h"

RHIStateManagerDX11::RHIStateManagerDX11() :
	StateCache(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice))
{
	Reset();
}

RHIStateManagerDX11::~RHIStateManagerDX11()
{
}

void RHIStateManagerDX11::Reset()
{
	UnmapConstants();

	RasterizerState = 0;
	DepthStencilState = 0;
	BlendState = 0;

	StencilRef = 0;
	AlphaRef = 0;

	bRSNeedApply = true;
	bDSSNeedApply = true;
	bBSNeedApply = true;

	bRSChanged = false;
	bDSSChanged = false;
	bBSChanged = false;

	ResetRDesc();
	ResetDDesc();
	ResetBDesc();

	bOverrideScissoring = false;
	bOverrideScissoringValue = FALSE;
	SampleMask = 0xffffffff;
}

void RHIStateManagerDX11::ResetBDesc()
{
	ZeroMemory(&BDesc, sizeof(BDesc));

	BDesc.AlphaToCoverageEnable = FALSE;
	BDesc.IndependentBlendEnable = FALSE;

	for (int i = 0; i < 8; ++i)
	{
		BDesc.RenderTarget[i].SrcBlend = D3D11_BLEND_ONE;
		BDesc.RenderTarget[i].DestBlend = D3D11_BLEND_ZERO;
		BDesc.RenderTarget[i].BlendOp = D3D11_BLEND_OP_ADD;
		BDesc.RenderTarget[i].SrcBlendAlpha = D3D11_BLEND_ONE;
		BDesc.RenderTarget[i].DestBlendAlpha = D3D11_BLEND_ZERO;
		BDesc.RenderTarget[i].BlendOpAlpha = D3D11_BLEND_OP_ADD;
		BDesc.RenderTarget[i].BlendEnable = FALSE;
		BDesc.RenderTarget[i].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
	}
}

void RHIStateManagerDX11::ResetDDesc()
{
	ZeroMemory(&DSDesc, sizeof(DSDesc));

	DSDesc.DepthEnable = TRUE;
	DSDesc.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ALL;
	DSDesc.DepthFunc = D3D11_COMPARISON_LESS;
	DSDesc.StencilEnable = TRUE;
	DSDesc.StencilReadMask = 0xFF;
	DSDesc.StencilWriteMask = 0xFF;

	DSDesc.FrontFace.StencilFailOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.FrontFace.StencilDepthFailOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.FrontFace.StencilPassOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.FrontFace.StencilFunc = D3D11_COMPARISON_ALWAYS;

	DSDesc.BackFace.StencilFailOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.BackFace.StencilDepthFailOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.BackFace.StencilPassOp = D3D11_STENCIL_OP_KEEP;
	DSDesc.BackFace.StencilFunc = D3D11_COMPARISON_ALWAYS;
}

void RHIStateManagerDX11::ResetRDesc()
{
	ZeroMemory(&RDesc, sizeof(RDesc));
	RDesc.FillMode = D3D11_FILL_SOLID;
	RDesc.CullMode = D3D11_CULL_BACK;
	RDesc.FrontCounterClockwise = FALSE;
	RDesc.DepthBias = 0;
	RDesc.DepthBiasClamp = 0.0f;
	RDesc.SlopeScaledDepthBias = 0.0f;
	RDesc.DepthClipEnable = TRUE;
	RDesc.ScissorEnable = FALSE;
	RDesc.MultisampleEnable = FALSE;
	RDesc.AntialiasedLineEnable = FALSE;
}

void RHIStateManagerDX11::UnmapConstants()
{
	BindAlphaCallback = nullptr;
	AlphaRef = 0;
}

void RHIStateManagerDX11::Apply()
{
	ID3D11DeviceContext* Context = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());

	if (bRSNeedApply || bRSChanged)
	{
		if (bRSChanged)
		{
			RasterizerState = StateCache.GetRasterizerState(RDesc);
			// Get state from cache
		}
		Context->RSSetState(RasterizerState);
		bRSNeedApply = false;
		bRSChanged = false;
	}

	if (bDSSNeedApply || bDSSChanged)
	{
		if (bDSSChanged)
		{
			DepthStencilState = StateCache.GetDepthStencilState(DSDesc);
		}
		Context->OMSetDepthStencilState(DepthStencilState, StencilRef);
		bDSSNeedApply = false;
		bDSSChanged = false;
	}

	if (bBSNeedApply || bBSChanged)
	{
		if (bBSChanged)
		{
			BlendState = StateCache.GetBlendState(BDesc);
		}
		FLOAT BlendFactor[4] = {0.0f, 0.0f, 0.0f, 0.0f};
		Context->OMSetBlendState(BlendState, BlendFactor, SampleMask);
		bBSNeedApply = false;
		bBSChanged = false;
	}
}

void RHIStateManagerDX11::SetRasterizerState(void* NewState)
{
	bRSChanged = false;

	if (NewState != RasterizerState)
	{
		RasterizerState = (ID3D11RasterizerState*)NewState;
		bRSNeedApply = true;
		bRSForced = true;
	}

	if (bOverrideScissoring)
	{
		EnableScissoring(bOverrideScissoringValue);
	}
}

void RHIStateManagerDX11::SetDepthStencilState(void* NewState)
{
	bDSSChanged = false;

	if (NewState != DepthStencilState)
	{
		DepthStencilState = (ID3D11DepthStencilState*)NewState;
		bDSSNeedApply = true;
		bDSSForced = true;
	}
}

void RHIStateManagerDX11::SetBlendState(void* NewState)
{
	bBSChanged = false;

	if (NewState != BlendState)
	{
		BlendState = (ID3D11BlendState*)NewState;
		bBSNeedApply = true;
		bBSForced = true;
	}
}

void RHIStateManagerDX11::EnableScissoring(bool Enable)
{
	ValidateRDesc();

	if ((bool)RDesc.ScissorEnable != Enable)
	{
		bRSChanged = true;
		RDesc.ScissorEnable = Enable;
	}
}

void RHIStateManagerDX11::OverrideScissoring(bool Override, bool Value)
{
	bOverrideScissoring = Override;
	bOverrideScissoringValue = Value;

	if (bOverrideScissoring)
	{
		EnableScissoring(bOverrideScissoringValue);
	}
	else
	{
		D3D11_RASTERIZER_DESC tmpDesc = {};
		if (RasterizerState)
		{
			RasterizerState->GetDesc(&tmpDesc);
		}
		else
		{
			tmpDesc.ScissorEnable = false;
		}

		ValidateRDesc();
		if (RDesc.ScissorEnable != tmpDesc.ScissorEnable)
		{
			RDesc.ScissorEnable = tmpDesc.ScissorEnable;
			bRSChanged = true;
		}
	}
}

void RHIStateManagerDX11::SetStencilRef(u32 NewStencilRef)
{
	if (StencilRef != NewStencilRef)
	{
		StencilRef = NewStencilRef;
		bDSSNeedApply = true;
	}
}

void RHIStateManagerDX11::SetAlphaRef(u32 NewAlphaRef)
{
	AlphaRef = NewAlphaRef;

	if (BindAlphaCallback)
	{
		BindAlphaCallback(AlphaRef);
	}
}

void RHIStateManagerDX11::SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail)
{
	ValidateDSDesc();

	bDSSChanged = true;

	DSDesc.StencilEnable = Enable;
	DSDesc.StencilReadMask = Mask;
	DSDesc.StencilWriteMask = WriteMask;

	DSDesc.FrontFace.StencilFailOp = (D3D11_STENCIL_OP)Fail;
	DSDesc.FrontFace.StencilDepthFailOp = (D3D11_STENCIL_OP)ZFail;
	DSDesc.FrontFace.StencilPassOp = (D3D11_STENCIL_OP)Pass;
	DSDesc.FrontFace.StencilFunc = (D3D11_COMPARISON_FUNC)Func;

	DSDesc.BackFace.StencilFailOp = (D3D11_STENCIL_OP)Fail;
	DSDesc.BackFace.StencilDepthFailOp = (D3D11_STENCIL_OP)ZFail;
	DSDesc.BackFace.StencilPassOp = (D3D11_STENCIL_OP)Pass;
	DSDesc.BackFace.StencilFunc = (D3D11_COMPARISON_FUNC)Func;

	SetStencilRef(Ref);
}

void RHIStateManagerDX11::SetDepthFunc(u32 Func)
{
	ValidateDSDesc();

	bDSSChanged = bDSSChanged || DSDesc.DepthFunc != (D3D11_COMPARISON_FUNC)Func;
	DSDesc.DepthFunc = (D3D11_COMPARISON_FUNC)Func;
}

void RHIStateManagerDX11::SetDepthEnable(u32 Enable)
{
	ValidateDSDesc();

	bDSSChanged = bDSSChanged || DSDesc.DepthEnable != Enable;
	DSDesc.DepthEnable = Enable;
}

void RHIStateManagerDX11::SetColorWriteEnable(u32 WriteMask)
{
	ValidateBDesc();

	for (int i = 0; i < 8; ++i)
	{
		bBSChanged = bBSChanged || BDesc.RenderTarget[i].RenderTargetWriteMask != WriteMask;
		BDesc.RenderTarget[i].RenderTargetWriteMask = WriteMask;
	}
}

void RHIStateManagerDX11::SetCullMode(u32 Mode)
{
	ValidateRDesc();

	bRSChanged = bRSChanged || RDesc.CullMode != (D3D11_CULL_MODE)Mode;
	RDesc.CullMode = (D3D11_CULL_MODE)Mode;
	CacheCullMode = Mode;
}

void RHIStateManagerDX11::BindAlphaRefCallback(const BindAlphaCallbackDecl& Callback)
{
	BindAlphaCallback = Callback;

	if (BindAlphaCallback)
	{
		BindAlphaCallback(AlphaRef);
	}
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

void RHIStateManagerDX11::SetMultisample(u32 Enable)
{
	ValidateRDesc();

	bRSChanged = bRSChanged || RDesc.MultisampleEnable != Enable;
	RDesc.MultisampleEnable = Enable;
}


void RHIStateManagerDX11::SetSampleMask(u32 Mask)
{
	bBSNeedApply = bBSNeedApply || SampleMask != Mask;
	SampleMask = Mask;
}

void RHIStateManagerDX11::ValidateRDesc()
{
	if (!bRSForced)
	{
		return;
	}

	if (RasterizerState == nullptr)
	{
		ResetRDesc();
		bRSNeedApply = true;
		return;
	}

	RasterizerState->GetDesc(&RDesc);
	bRSNeedApply = true;
}

void RHIStateManagerDX11::ValidateDSDesc()
{
	if (!bDSSForced)
	{
		return;
	}

	bDSSForced = false;

	if (DepthStencilState == nullptr)
	{
		ResetDDesc();
		bDSSNeedApply = true;
		return;
	}

	DepthStencilState->GetDesc(&DSDesc);
	bDSSNeedApply = true;
}

void RHIStateManagerDX11::ValidateBDesc()
{
	if (!bBSForced)
	{
		return;
	}

	if (DepthStencilState == nullptr)
	{
		ResetBDesc();
		bBSNeedApply = true;
		return;
	}

	BlendState->GetDesc(&BDesc);
	bBSNeedApply = true;
}