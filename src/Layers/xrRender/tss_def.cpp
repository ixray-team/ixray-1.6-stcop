#include "stdafx.h"


#include "tss_def.h"

IDirect3DStateBlock9* SimulatorStates::record	()
{
#ifdef USE_DX11
	return 0;
#else //USE_DX11
	CHK_DX(RDevice->BeginStateBlock());
	for (u32 it=0; it<States.size(); it++)
	{
		State& S	= States[it];
		switch (S.type)
		{
		case 0:	CHK_DX(RDevice->SetRenderState		((D3DRENDERSTATETYPE)S.v1,S.v2));				break;
		case 1: CHK_DX(RDevice->SetTextureStageState	(S.v1,(D3DTEXTURESTAGESTATETYPE)S.v2,S.v3));	break;
		case 2: 
			{
				CHK_DX(RDevice->SetSamplerState		(S.v1,
														(D3DSAMPLERSTATETYPE)S.v2,
														((D3DSAMPLERSTATETYPE)S.v2==D3DSAMP_MAGFILTER&&S.v3==D3DTEXF_ANISOTROPIC)?D3DTEXF_LINEAR:S.v3));
			}break;
		}
	}
	IDirect3DStateBlock9*	SB = 0;
	CHK_DX	(RDevice->EndStateBlock(&SB));
	return	SB;
#endif
}

void	SimulatorStates::set_RS(u32 a, u32 b, u32 c)
{
	States.erase(std::remove_if(States.begin(), States.end(),
			[&](auto& S)
			{
				return (0 == S.type) && (a == S.v1) && (c == S.v3 || c == u32(-1));
			}
		),
	States.end());

	// Register
	State st;
	st.set_RS(a, b, c);
	States.push_back(st);
}

void	SimulatorStates::set_TSS	(u32 a, u32 b, u32 c)
{
	// Search duplicates
	for (int t=0; t<int(States.size()); t++)
	{
		State& S	= States[t];
		if ((1==S.type)&&(a==S.v1)&&(b==S.v2)) {
			States.erase(States.begin()+t);
			break;
		}
	}

	// Register
	State		st;
	st.set_TSS	(a,b,c);
	States.push_back(st);
}

void	SimulatorStates::set_SAMP	(u32 a, u32 b, u32 c)
{
	// Search duplicates
	for (int t=0; t<int(States.size()); t++)
	{
		State& S	= States[t];
		if ((2==S.type)&&(a==S.v1)&&(b==S.v2)) {
			States.erase(States.begin()+t);
			break;
		}
	}

	// Register
	State		st;
	st.set_SAMP	(a,b,c);
	States.push_back(st);
}

bool	SimulatorStates::equal	(SimulatorStates& S)
{
	if (States.size()!=S.States.size())												return FALSE;
	if (0!=memcmp(&*States.begin(),&*S.States.begin(),States.size()*sizeof(State))) return FALSE;
	return TRUE;
}

void	SimulatorStates::clear	()
{
	States.clear();
}

#ifdef USE_DX11

#include "../xrRenderDX10/dx10StateUtils.h"

void SimulatorStates::UpdateState( dx10State &state) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			switch (S.v1)
			{
			case D3DRS_STENCILREF:
				state.UpdateStencilRef(S.v2);
				break;
			case D3DRS_ALPHAREF:
				state.UpdateAlphaRef(S.v2);
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc( D3D_RASTERIZER_DESC &desc ) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			switch (S.v1)
			{
			case D3DRS_FILLMODE:
				if (S.v2==D3DFILL_SOLID)
					desc.FillMode = D3D_FILL_SOLID;
				else
				{
					VERIFY(S.v2==D3DFILL_WIREFRAME);
					desc.FillMode = D3D_FILL_WIREFRAME;
				}
				break;

			case D3DRS_CULLMODE:
				desc.CullMode = (D3D11_CULL_MODE)S.v2;
				break;

			case D3DRS_DEPTHBIAS:
				VERIFY(0);
				break;

			case D3DRS_SLOPESCALEDEPTHBIAS:
				VERIFY(0);
				break;
				
			//	desc.DepthClipEnable = TRUE;
			
			case D3DRS_SCISSORTESTENABLE:
				desc.ScissorEnable = S.v2;
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc( D3D_DEPTH_STENCIL_DESC &desc ) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			switch (S.v1)
			{
			case D3DRS_ZENABLE:
				desc.DepthEnable = S.v2?1:0;
				break;

			case D3DRS_ZWRITEENABLE:
				desc.DepthWriteMask = S.v2 ? D3D_DEPTH_WRITE_MASK_ALL : D3D_DEPTH_WRITE_MASK_ZERO;
				break;

			case D3DRS_ZFUNC:
				desc.DepthFunc = dx10StateUtils::ConvertCmpFunction((D3DCMPFUNC)S.v2);
				break;

			case D3DRS_STENCILENABLE:
				desc.StencilEnable = S.v2?1:0;
				break;

			case D3DRS_STENCILMASK:
				desc.StencilReadMask = (UINT8)S.v2;
				break;

			case D3DRS_STENCILWRITEMASK:
				desc.StencilWriteMask = (UINT8)S.v2;
				break;

			case D3DRS_STENCILFAIL:
				desc.FrontFace.StencilFailOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_STENCILZFAIL:
				desc.FrontFace.StencilDepthFailOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_STENCILPASS:
				desc.FrontFace.StencilPassOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_STENCILFUNC:
				desc.FrontFace.StencilFunc = dx10StateUtils::ConvertCmpFunction((D3DCMPFUNC)S.v2);
				break;

			case D3DRS_CCW_STENCILFAIL:
				desc.BackFace.StencilFailOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_CCW_STENCILZFAIL:
				desc.BackFace.StencilDepthFailOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_CCW_STENCILPASS:
				desc.BackFace.StencilPassOp = dx10StateUtils::ConvertStencilOp((D3DSTENCILOP)S.v2);
				break;

			case D3DRS_CCW_STENCILFUNC:
				desc.BackFace.StencilFunc = dx10StateUtils::ConvertCmpFunction((D3DCMPFUNC)S.v2);
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc( D3D_BLEND_DESC &desc ) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			int start_idx = 0;
			int end_idx = std::size(desc.RenderTarget);

			if (S.v3 != u32(-1))
			{
				start_idx = S.v3;
				end_idx = start_idx + 1;

				desc.IndependentBlendEnable = TRUE;
			}

			switch (S.v1)
			{
			case XRDX10RS_ALPHATOCOVERAGE:
				desc.AlphaToCoverageEnable = S.v2 ? 1 : 0;
				break;
				
			case D3DRS_SRCBLEND:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].SrcBlend = dx10StateUtils::ConvertBlendArg((D3DBLEND)S.v2);
				}
				break;

			case D3DRS_DESTBLEND:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].DestBlend = dx10StateUtils::ConvertBlendArg((D3DBLEND)S.v2);
				}
				break;
			
				//D3DRS_ALPHAFUNC

			case D3DRS_BLENDOP:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].BlendOp = dx10StateUtils::ConvertBlendOp((D3DBLENDOP)S.v2);
				}
				break;

			case D3DRS_SRCBLENDALPHA:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].SrcBlendAlpha = dx10StateUtils::ConvertBlendArg((D3DBLEND)S.v2);
				}
				break;

			case D3DRS_DESTBLENDALPHA:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].DestBlendAlpha = dx10StateUtils::ConvertBlendArg((D3DBLEND)S.v2);
				}
				break;

			case D3DRS_BLENDOPALPHA:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].BlendOpAlpha = dx10StateUtils::ConvertBlendOp((D3DBLENDOP)S.v2);
				}
				break;

			case D3DRS_ALPHABLENDENABLE:
				for (int i = start_idx; i < end_idx; ++i)
				{
					desc.RenderTarget[i].BlendEnable = S.v2 ? 1 : 0;
				}
				break;

			case D3DRS_COLORWRITEENABLE:
				desc.RenderTarget[start_idx].RenderTargetWriteMask = (u8)S.v2;
				break;

			case D3DRS_COLORWRITEENABLE1:
				desc.RenderTarget[1].RenderTargetWriteMask = (u8)S.v2;
				break;

			case D3DRS_COLORWRITEENABLE2:
				desc.RenderTarget[2].RenderTargetWriteMask = (u8)S.v2;
				break;

			case D3DRS_COLORWRITEENABLE3:
				desc.RenderTarget[3].RenderTargetWriteMask = (u8)S.v2;
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc(RHISampleDesc descArray[RHI_COMMONSHADER_SAMPLER_SLOT_COUNT], bool SamplerUsed[RHI_COMMONSHADER_SAMPLER_SLOT_COUNT], int iBaseSamplerIndex) const
{
	const ERHI_FILTER MipfilterLinear = ERHI_FILTER::MIN_MAG_POINT_MIP_LINEAR;
	const ERHI_FILTER MagfilterLinear = ERHI_FILTER::MIN_POINT_MAG_LINEAR_MIP_POINT;
	const ERHI_FILTER MinfilterLinear = ERHI_FILTER::MIN_LINEAR_MAG_MIP_POINT;
	const ERHI_FILTER AllfilterLinear = ERHI_FILTER::MIN_MAG_MIP_LINEAR;
	const ERHI_FILTER FilterAnisotropic = ERHI_FILTER::ANISOTROPIC;
	const ERHI_FILTER FilterComparison = ERHI_FILTER::COMPARISON_MIN_MAG_MIP_POINT;

	for (u32 it = 0; it < States.size(); it++)
	{
		const State& S = States[it];
		if (S.type == 2)
		{
			int iSamplerIndex = int(S.v1);
			iSamplerIndex -= iBaseSamplerIndex;

			if ((iSamplerIndex >= RHI_COMMONSHADER_SAMPLER_SLOT_COUNT) || iSamplerIndex < 0)
				continue;

			SamplerUsed[iSamplerIndex] = true;
			RHISampleDesc& desc = descArray[iSamplerIndex];

			switch (S.v2)
			{
				//D3D_FILTER Filter;
			case D3DSAMP_MAGFILTER:	/* D3DTEXTUREFILTER filter to use for magnification */
				switch (S.v3)
				{
				case D3DTEXF_NONE:
				case D3DTEXF_POINT:
					desc.Filter = (ERHI_FILTER)(desc.Filter & (~MagfilterLinear));
					break;
				case D3DTEXF_LINEAR:
					desc.Filter = (ERHI_FILTER)(desc.Filter | MagfilterLinear);
					break;
				default:
					NODEFAULT;
				}
				break;

			case D3DSAMP_MINFILTER:	/* D3DTEXTUREFILTER filter to use for minification */
				switch (S.v3)
				{
				case D3DTEXF_NONE:
				case D3DTEXF_POINT:
					desc.Filter = (ERHI_FILTER)(desc.Filter & (~MinfilterLinear));
					break;
				case D3DTEXF_LINEAR:
					desc.Filter = (ERHI_FILTER)(desc.Filter | MinfilterLinear);
					break;
				default:
					NODEFAULT;
				}
				break;

			case D3DSAMP_MIPFILTER:	/* D3DTEXTUREFILTER filter to use between mipmaps during minification */
				switch (S.v3)
				{
				case D3DTEXF_NONE:
				case D3DTEXF_POINT:
					desc.Filter = (ERHI_FILTER)(desc.Filter & (~MipfilterLinear));
					//desc.Filter &= ~MipfilterLinear;
					break;
				case D3DTEXF_LINEAR:
					desc.Filter = (ERHI_FILTER)(desc.Filter | MipfilterLinear);
					//desc.Filter |= MipfilterLinear;
					break;
				default:
					NODEFAULT;
				}
				break;

			case XRDX10SAMP_ANISOTROPICFILTER:
				if (S.v3)
					desc.Filter = (ERHI_FILTER)(desc.Filter | FilterAnisotropic);
				//desc.Filter |= FilterAnisotropic;
				else
					desc.Filter = (ERHI_FILTER)(desc.Filter & (~FilterAnisotropic));
				//desc.Filter &= ~FilterAnisotropic;
				break;

			case XRDX10SAMP_COMPARISONFILTER:
				if (S.v3)
					desc.Filter = (ERHI_FILTER)(desc.Filter | FilterComparison);
				else
					desc.Filter = (ERHI_FILTER)(desc.Filter & (~FilterComparison));
				break;

				//D3Dxx_TEXTURE_ADDRESS_MODE AddressU;
			case D3DSAMP_ADDRESSU:	/* D3DTEXTUREADDRESS for U coordinate */
				desc.AddressU = dx10StateUtils::ConvertTextureAddressMode(D3DTEXTUREADDRESS(S.v3));
				break;

			case D3DSAMP_ADDRESSV:	/* D3DTEXTUREADDRESS for V coordinate */
				desc.AddressV = dx10StateUtils::ConvertTextureAddressMode(D3DTEXTUREADDRESS(S.v3));
				break;

			case D3DSAMP_ADDRESSW:	/* D3DTEXTUREADDRESS for W coordinate */
				desc.AddressW = dx10StateUtils::ConvertTextureAddressMode(D3DTEXTUREADDRESS(S.v3));
				break;

				//	FLOAT MipLODBias
			case D3DSAMP_MIPMAPLODBIAS:
				desc.MipLODBias = *((float*)(&(S.v3)));
				break;

				//	UINT MaxAnisotropy;
			case D3DSAMP_MAXANISOTROPY:
				desc.MaxAnisotropy = S.v3;
				break;

				//	D3Dxx_COMPARISON_FUNC ComparisonFunc;
			case XRDX10SAMP_COMPARISONFUNC:
				desc.ComparisonFunc = (ERHI_COMPARISON_FUNC)S.v3;
				break;

				//	FLOAT BorderColor[4];
			case D3DSAMP_BORDERCOLOR:
			{
				desc.BorderColor[0] = ((S.v3 >> 16) & 0xff) / 255.0f;
				desc.BorderColor[1] = ((S.v3 >> 8) & 0xff) / 255.0f;
				desc.BorderColor[2] = ((S.v3) & 0xff) / 255.0f;
				desc.BorderColor[3] = ((S.v3 >> 24) & 0xff) / 255.0f;
			}
			break;

			//	FLOAT MinLOD;
			case XRDX10SAMP_MINLOD:
				desc.MinLOD = (FLOAT)S.v3;
				break;

				//	FLOAT MaxLOD;
			case D3DSAMP_MAXMIPLEVEL:
				desc.MaxLOD = (FLOAT)S.v3;
				break;
			}
		}
	}

	//	Validate data
	for (int i = 0; i < RHI_COMMONSHADER_SAMPLER_SLOT_COUNT; ++i)
	{
		RHISampleDesc& desc = descArray[i];
		if (!!(desc.Filter & FilterAnisotropic))
		{
			desc.Filter = (ERHI_FILTER)(desc.Filter | AllfilterLinear);
			//desc.Filter |= AllfilterLinear;
		}

		VERIFY(desc.MinLOD <= desc.MaxLOD);
		if (desc.MinLOD > desc.MaxLOD)
			desc.MaxLOD = desc.MinLOD;
	}
}

#endif //USE_DX11