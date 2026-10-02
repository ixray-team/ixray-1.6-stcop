#include "RHIStateManagerDX11.h"
#include "../../xrCore/D3DLegacy.h"

RHIStateManagerDX11::RHIStateManagerDX11(ID3D11DeviceContext* InContext) :
	StateCache(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice))
{
	if (InContext)
	{
		D3DContext = InContext;
	}
	else
	{
		D3DContext = static_cast<ID3D11DeviceContext*>(GRHI->GetContext());
	}

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

	bRSNeedApply = true;
	bDSSNeedApply = true;
	bBSNeedApply = true;

	bRSChanged = false;
	bDSSChanged = false;
	bBSChanged = false;

	bRSForced = false;
	bDSSForced = false;
	bBSForced = false;

	ResetRDesc();
	ResetDDesc();
	ResetBDesc();

	bOverrideScissoring = false;
	bOverrideScissoringValue = false;
	SampleMask = 0xffffffff;
}

void RHIStateManagerDX11::ResetBDesc()
{
	ZeroMemory(&BDesc, sizeof(BDesc));

	BDesc.AlphaToCoverageEnable = false;
	BDesc.IndependentBlendEnable = false;

	for (int i = 0; i < 8; ++i)
	{
		BDesc.RenderTarget[i].SrcBlend = D3D11_BLEND_ONE;
		BDesc.RenderTarget[i].DestBlend = D3D11_BLEND_ZERO;
		BDesc.RenderTarget[i].BlendOp = D3D11_BLEND_OP_ADD;
		BDesc.RenderTarget[i].SrcBlendAlpha = D3D11_BLEND_ONE;
		BDesc.RenderTarget[i].DestBlendAlpha = D3D11_BLEND_ZERO;
		BDesc.RenderTarget[i].BlendOpAlpha = D3D11_BLEND_OP_ADD;
		BDesc.RenderTarget[i].BlendEnable = false;
		BDesc.RenderTarget[i].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
	}
}

void RHIStateManagerDX11::ResetDDesc()
{
	ZeroMemory(&DSDesc, sizeof(DSDesc));

	DSDesc.DepthEnable = true;
	DSDesc.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ALL;
	DSDesc.DepthFunc = D3D11_COMPARISON_LESS;
	DSDesc.StencilEnable = true;
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
	RDesc.FrontCounterClockwise = false;
	RDesc.DepthBias = 0;
	RDesc.DepthBiasClamp = 0.0f;
	RDesc.SlopeScaledDepthBias = 0.0f;
	RDesc.DepthClipEnable = true;
	RDesc.ScissorEnable = false;
	RDesc.MultisampleEnable = false;
	RDesc.AntialiasedLineEnable = false;
}

void RHIStateManagerDX11::UnmapConstants()
{
	BindAlphaCallback = nullptr;
}

void RHIStateManagerDX11::SetContext(ID3D11DeviceContext* InContext)
{
	D3DContext = InContext;
}

void RHIStateManagerDX11::Apply()
{
	ID3D11DeviceContext* Context = D3DContext;
	R_ASSERT(Context);

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
	bBSForced = true;

	if (NewState != BlendState)
	{
		BlendState = (ID3D11BlendState*)NewState;
		bBSNeedApply = true;
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
void RHIStateManagerDX11::SetRenderState(u32 p1, u32 p2)
{
	switch (p1)
	{
		case D3DRS_ZENABLE:
			SetDepthEnable(p2 ? TRUE : FALSE);
			break;

		case D3DRS_FILLMODE:
			ValidateRDesc();
			bRSChanged = bRSChanged || RDesc.FillMode != (D3D11_FILL_MODE)p2;
			RDesc.FillMode = (D3D11_FILL_MODE)p2;
			break;

		case D3DRS_ZWRITEENABLE:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || (DSDesc.DepthWriteMask != (p2 ? D3D11_DEPTH_WRITE_MASK_ALL : D3D11_DEPTH_WRITE_MASK_ZERO));
			DSDesc.DepthWriteMask = p2 ? D3D11_DEPTH_WRITE_MASK_ALL : D3D11_DEPTH_WRITE_MASK_ZERO;
			break;

		case D3DRS_ALPHATESTENABLE:
			// В DirectX 11 alpha test реализуется через шейдеры или blend state
			// Можно сохранять для обратной совместимости
			break;

		case D3DRS_TEXTUREFACTOR:
			// Texture factor используется в шейдерах, должен устанавливаться через константы
			// Можно вызвать callback или сохранить для использования в шейдерах
			break;

		case D3DRS_CULLMODE:
			switch (p2)
			{
				case 1:
					SetCullMode(ERHI_CULLMODE::NONE);
					break;
				case 2:
					SetCullMode(ERHI_CULLMODE::FRONT);
					break;
				case 3:
					SetCullMode(ERHI_CULLMODE::BACK);
					break;
			}
			break;

		case D3DRS_SRCBLEND:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].SrcBlend != (D3D11_BLEND)p2;
				BDesc.RenderTarget[i].SrcBlend = (D3D11_BLEND)p2;
			}
			break;

		case D3DRS_DESTBLEND:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].DestBlend != (D3D11_BLEND)p2;
				BDesc.RenderTarget[i].DestBlend = (D3D11_BLEND)p2;
			}
			break;

		case D3DRS_ZFUNC:
			SetDepthFunc(p2);
			break;

		case D3DRS_ALPHAREF:
			SetAlphaRef(p2);
			break;

		case D3DRS_ALPHAFUNC:
			// Alpha func используется в alpha test, сохраняем для совместимости
			break;

		case D3DRS_ALPHABLENDENABLE:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].BlendEnable != (BOOL)p2;
				BDesc.RenderTarget[i].BlendEnable = (BOOL)p2;
			}
			break;

		case D3DRS_STENCILENABLE:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.StencilEnable != (BOOL)p2;
			DSDesc.StencilEnable = (BOOL)p2;
			break;

		case D3DRS_STENCILFAIL:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.FrontFace.StencilFailOp != (D3D11_STENCIL_OP)p2;
			DSDesc.FrontFace.StencilFailOp = (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilFailOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_STENCILZFAIL:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.FrontFace.StencilDepthFailOp != (D3D11_STENCIL_OP)p2;
			DSDesc.FrontFace.StencilDepthFailOp = (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilDepthFailOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_STENCILPASS:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.FrontFace.StencilPassOp != (D3D11_STENCIL_OP)p2;
			DSDesc.FrontFace.StencilPassOp = (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilPassOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_STENCILFUNC:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.FrontFace.StencilFunc != (D3D11_COMPARISON_FUNC)p2;
			DSDesc.FrontFace.StencilFunc = (D3D11_COMPARISON_FUNC)p2;
			DSDesc.BackFace.StencilFunc = (D3D11_COMPARISON_FUNC)p2;
			break;

		case D3DRS_CCW_STENCILFAIL:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.BackFace.StencilFailOp != (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilFailOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_CCW_STENCILZFAIL:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.BackFace.StencilDepthFailOp != (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilDepthFailOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_CCW_STENCILPASS:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.BackFace.StencilPassOp != (D3D11_STENCIL_OP)p2;
			DSDesc.BackFace.StencilPassOp = (D3D11_STENCIL_OP)p2;
			break;

		case D3DRS_CCW_STENCILFUNC:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.BackFace.StencilFunc != (D3D11_COMPARISON_FUNC)p2;
			DSDesc.BackFace.StencilFunc = (D3D11_COMPARISON_FUNC)p2;
			break;

		case D3DRS_STENCILREF:
			SetStencilRef(p2);
			break;

		case D3DRS_STENCILMASK:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.StencilReadMask != (UINT8)p2;
			DSDesc.StencilReadMask = (UINT8)p2;
			break;

		case D3DRS_STENCILWRITEMASK:
			ValidateDSDesc();
			bDSSChanged = bDSSChanged || DSDesc.StencilWriteMask != (UINT8)p2;
			DSDesc.StencilWriteMask = (UINT8)p2;
			break;

		case D3DRS_SCISSORTESTENABLE:
			EnableScissoring(p2 ? TRUE : FALSE);
			break;

		case D3DRS_SLOPESCALEDEPTHBIAS:
			ValidateRDesc();
			bRSChanged = bRSChanged || RDesc.SlopeScaledDepthBias != *((float*)&p2);
			RDesc.SlopeScaledDepthBias = *((float*)&p2);
			break;

		case D3DRS_DEPTHBIAS:
			ValidateRDesc();
			bRSChanged = bRSChanged || RDesc.DepthBias != (INT)p2;
			RDesc.DepthBias = (INT)p2;
			break;

		case D3DRS_COLORWRITEENABLE:
			SetColorWriteEnable(p2);
			break;

		case D3DRS_COLORWRITEENABLE1:
		case D3DRS_COLORWRITEENABLE2:
		case D3DRS_COLORWRITEENABLE3:
		{
			int targetIndex = p1 - D3DRS_COLORWRITEENABLE1;
			if (targetIndex >= 0 && targetIndex < 8)
			{
				ValidateBDesc();
				bBSChanged = bBSChanged || BDesc.RenderTarget[targetIndex].RenderTargetWriteMask != p2;
				BDesc.RenderTarget[targetIndex].RenderTargetWriteMask = p2;
			}
		}
		break;

		case D3DRS_BLENDOP:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].BlendOp != (D3D11_BLEND_OP)p2;
				BDesc.RenderTarget[i].BlendOp = (D3D11_BLEND_OP)p2;
			}
			break;

		case D3DRS_SRCBLENDALPHA:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].SrcBlendAlpha != (D3D11_BLEND)p2;
				BDesc.RenderTarget[i].SrcBlendAlpha = (D3D11_BLEND)p2;
			}
			break;

		case D3DRS_DESTBLENDALPHA:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].DestBlendAlpha != (D3D11_BLEND)p2;
				BDesc.RenderTarget[i].DestBlendAlpha = (D3D11_BLEND)p2;
			}
			break;

		case D3DRS_BLENDOPALPHA:
			ValidateBDesc();
			for (int i = 0; i < 8; ++i)
			{
				bBSChanged = bBSChanged || BDesc.RenderTarget[i].BlendOpAlpha != (D3D11_BLEND_OP)p2;
				BDesc.RenderTarget[i].BlendOpAlpha = (D3D11_BLEND_OP)p2;
			}
			break;

		case D3DRS_SEPARATEALPHABLENDENABLE:
			ValidateBDesc();
			// В D3D11 это всегда TRUE для отдельных настроек альфы
			BDesc.IndependentBlendEnable = TRUE;
			break;

		case D3DRS_MULTISAMPLEANTIALIAS:
			SetMultisample(p2);
			break;

		case D3DRS_MULTISAMPLEMASK:
			SetSampleMask(p2);
			break;

		case D3DRS_ANTIALIASEDLINEENABLE:
			ValidateRDesc();
			bRSChanged = bRSChanged || RDesc.AntialiasedLineEnable != (BOOL)p2;
			RDesc.AntialiasedLineEnable = (BOOL)p2;
			break;

		default:
			break;
	}
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

void RHIStateManagerDX11::SetCullMode(ERHI_CULLMODE Mode)
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

	bBSForced = false;

	if (BlendState == nullptr)
	{
		ResetBDesc();
		bBSNeedApply = true;
		return;
	}

	BlendState->GetDesc(&BDesc);
	bBSNeedApply = true;
}