#include "stdafx.h"
#include "dx10StateUtils.h"


namespace dx10StateUtils
{
ERHI_COMPARISON ConvertCmpFunction(D3DCMPFUNC Func)
{
	switch (Func)
	{
	case D3DCMP_NEVER:
		return RHI_COMPARISON_NEVER;
	case D3DCMP_LESS:
		return RHI_COMPARISON_LESS;
	case D3DCMP_EQUAL:
		return RHI_COMPARISON_EQUAL;
	case D3DCMP_LESSEQUAL:
		return RHI_COMPARISON_LESS_EQUAL;
	case D3DCMP_GREATER:
		return RHI_COMPARISON_GREATER;
	case D3DCMP_NOTEQUAL:
		return RHI_COMPARISON_NOT_EQUAL;
	case D3DCMP_GREATEREQUAL:
		return RHI_COMPARISON_GREATER_EQUAL;
	case D3DCMP_ALWAYS:
		return RHI_COMPARISON_ALWAYS;
	default:
		VERIFY(!"ConvertCmpFunction can't convert argument!");
		return RHI_COMPARISON_ALWAYS;
	}
}

ERHI_STENCIL_OP ConvertStencilOp(D3DSTENCILOP Op)
{
	switch (Op)
	{
	case D3DSTENCILOP_KEEP:
		return RHI_STENCIL_OP_KEEP;
	case D3DSTENCILOP_ZERO:
		return RHI_STENCIL_OP_ZERO;
	case D3DSTENCILOP_REPLACE:
		return RHI_STENCIL_OP_REPLACE;
	case D3DSTENCILOP_INCRSAT:
		return RHI_STENCIL_OP_INCR_SAT;
	case D3DSTENCILOP_DECRSAT:
		return RHI_STENCIL_OP_DECR_SAT;
	case D3DSTENCILOP_INVERT:
		return RHI_STENCIL_OP_INVERT;
	case D3DSTENCILOP_INCR:
		return RHI_STENCIL_OP_INCR;
	case D3DSTENCILOP_DECR:
		return RHI_STENCIL_OP_DECR;
	default:
		VERIFY(!"ConvertStencilOp can't convert argument!");
		return RHI_STENCIL_OP_KEEP;
	}
}

ERHI_BLEND ConvertBlendArg(D3DBLEND Arg)
{
	switch (Arg)
	{
	case D3DBLEND_ZERO:
		return RHI_BLEND_ZERO;
	case D3DBLEND_ONE:
		return RHI_BLEND_ONE;
	case D3DBLEND_SRCCOLOR:
		return RHI_BLEND_SRC_COLOR;
	case D3DBLEND_INVSRCCOLOR:
		return RHI_BLEND_INV_SRC_COLOR;
	case D3DBLEND_SRCALPHA:
		return RHI_BLEND_SRC_ALPHA;
	case D3DBLEND_INVSRCALPHA:
		return RHI_BLEND_INV_SRC_ALPHA;
	case D3DBLEND_DESTALPHA:
		return RHI_BLEND_DEST_ALPHA;
	case D3DBLEND_INVDESTALPHA:
		return RHI_BLEND_INV_DEST_ALPHA;
	case D3DBLEND_DESTCOLOR:
		return RHI_BLEND_DEST_COLOR;
	case D3DBLEND_INVDESTCOLOR:
		return RHI_BLEND_INV_DEST_COLOR;
	case D3DBLEND_SRCALPHASAT:
		return RHI_BLEND_SRC_ALPHA_SAT;
	//case D3DBLEND_BOTHSRCALPHA:
	//	return ;
	//case D3DBLEND_BOTHINVSRCALPHA:
	//	return ;
	case D3DBLEND_BLENDFACTOR:
		return RHI_BLEND_BLEND_FACTOR;
	case D3DBLEND_INVBLENDFACTOR:
		return RHI_BLEND_INV_BLEND_FACTOR;
	case D3DBLEND_SRCCOLOR2:
		return RHI_BLEND_SRC1_COLOR;
	case D3DBLEND_INVSRCCOLOR2:
		return RHI_BLEND_INV_SRC1_COLOR;
		default:
			VERIFY(!"ConvertBlendArg can't convert argument!");
			return RHI_BLEND_ONE;
	}
}

ERHI_BLEND_OP ConvertBlendOp(D3DBLENDOP Op)
{
	switch (Op)
	{
	case D3DBLENDOP_ADD:
		return RHI_BLEND_OP_ADD;
	case D3DBLENDOP_SUBTRACT:
		return RHI_BLEND_OP_SUBTRACT;
	case D3DBLENDOP_REVSUBTRACT:
		return RHI_BLEND_OP_REV_SUBTRACT;
	case D3DBLENDOP_MIN:
		return RHI_BLEND_OP_MIN;
	case D3DBLENDOP_MAX:
		return RHI_BLEND_OP_MAX;
	default:
		VERIFY(!"ConvertBlendOp can't convert argument!");
		return RHI_BLEND_OP_ADD;
	}
}

void ResetDescription( RHIRasterizerDesc &desc )
{
	ZeroMemory(&desc, sizeof(desc));
	desc.FillMode = RHI_FILL_SOLID;
	desc.CullMode = RHI_CULL_BACK;
	desc.FrontCounterClockwise = false;
	desc.DepthBias = 0;
	desc.DepthBiasClamp = 0.0f;
	desc.SlopeScaledDepthBias = 0.0f;
	desc.DepthClipEnable = true;
	desc.ScissorEnable = false;
	desc.MultisampleEnable = false;
	desc.AntialiasedLineEnable = false;
}

void ResetDescription( RHIDepthStencilDesc &desc )
{
	ZeroMemory(&desc, sizeof(desc));
	desc.DepthEnable = true;
	desc.DepthWriteMask = RHI_DEPTH_WRITE_MASK_ALL;
	desc.DepthFunc = RHI_COMPARISON_LESS;
	desc.StencilEnable = true;
	desc.StencilReadMask = 0xFF;
	desc.StencilWriteMask = 0xFF;

	desc.FrontFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
	desc.FrontFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
	desc.FrontFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
	desc.FrontFace.StencilFunc = RHI_COMPARISON_ALWAYS;

	desc.BackFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
	desc.BackFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
	desc.BackFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
	desc.BackFace.StencilFunc = RHI_COMPARISON_ALWAYS;
}

void ResetDescription( RHIBlendDesc &desc )
{
	ZeroMemory(&desc, sizeof(desc));

	desc.AlphaToCoverageEnable = false;
	desc.IndependentBlendEnable = false;

	for ( int i=0; i<8; ++i)
	{
		desc.RenderTarget[i].SrcBlend = RHI_BLEND_ONE;
		desc.RenderTarget[i].DestBlend = RHI_BLEND_ZERO;
		desc.RenderTarget[i].BlendOp = RHI_BLEND_OP_ADD;
		desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_ONE;
		desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_ZERO;
		desc.RenderTarget[i].BlendOpAlpha = RHI_BLEND_OP_ADD;
		desc.RenderTarget[i].BlendEnable = false;
		desc.RenderTarget[i].RenderTargetWriteMask = RHI_COLOR_WRITE_ENABLE_ALL;
	}
}

void ValidateState(RHIDepthStencilDesc &desc)
{
	VERIFY( (desc.DepthEnable==0) || (desc.DepthEnable==1));
	VERIFY( (desc.StencilEnable==0) || (desc.StencilEnable==1));

	if (!desc.DepthEnable)
	{
		desc.DepthWriteMask = RHI_DEPTH_WRITE_MASK_ALL;
		desc.DepthFunc = RHI_COMPARISON_LESS;
	}

	if (!desc.StencilEnable)
	{
		desc.StencilReadMask = 0xFF;
		desc.StencilWriteMask = 0xFF;

		desc.FrontFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
		desc.FrontFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
		desc.FrontFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
		desc.FrontFace.StencilFunc = RHI_COMPARISON_ALWAYS;

		desc.BackFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
		desc.BackFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
		desc.BackFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
		desc.BackFace.StencilFunc = RHI_COMPARISON_ALWAYS;
	}
}

void ValidateState(RHIBlendDesc &desc)
{
	bool bBlendEnable = false;

	for ( int i=0; i<8; ++i)
	{
		VERIFY( (desc.RenderTarget[i].BlendEnable==0) || (desc.RenderTarget[i].BlendEnable==1));
		bBlendEnable |= desc.RenderTarget[i].BlendEnable;
	}

	for ( int i=0; i<8; ++i)
	{
		if (!bBlendEnable)
		{
			desc.RenderTarget[i].SrcBlend = RHI_BLEND_ONE;
			desc.RenderTarget[i].DestBlend = RHI_BLEND_ZERO;
			desc.RenderTarget[i].BlendOp = RHI_BLEND_OP_ADD;
			desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_ONE;
			desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_ZERO;
			desc.RenderTarget[i].BlendOpAlpha = RHI_BLEND_OP_ADD;
		}
		else
		{
			switch(desc.RenderTarget[i].SrcBlendAlpha)
			{
			case RHI_BLEND_SRC_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_SRC_ALPHA;
				break;
			case RHI_BLEND_INV_SRC_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_INV_SRC_ALPHA;
				break;
			case RHI_BLEND_DEST_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_DEST_ALPHA;
				break;
			case RHI_BLEND_INV_DEST_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_INV_DEST_ALPHA;
				break;
			case RHI_BLEND_SRC1_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_SRC1_ALPHA;
				break;
			case RHI_BLEND_INV_SRC1_COLOR:
				desc.RenderTarget[i].SrcBlendAlpha = RHI_BLEND_INV_SRC1_ALPHA;
					break;
			}

			switch(desc.RenderTarget[i].DestBlendAlpha)
			{
			case RHI_BLEND_SRC_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_SRC_ALPHA;
				break;
			case RHI_BLEND_INV_SRC_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_INV_SRC_ALPHA;
				break;
			case RHI_BLEND_DEST_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_DEST_ALPHA;
				break;
			case RHI_BLEND_INV_DEST_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_INV_DEST_ALPHA;
				break;
			case RHI_BLEND_SRC1_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_SRC1_ALPHA;
				break;
			case RHI_BLEND_INV_SRC1_COLOR:
				desc.RenderTarget[i].DestBlendAlpha = RHI_BLEND_INV_SRC1_ALPHA;
				break;
			}
		}
	}
}

};
