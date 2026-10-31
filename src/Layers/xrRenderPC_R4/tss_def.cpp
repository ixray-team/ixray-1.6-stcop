#include "stdafx.h"


#include "tss_def.h"

void	SimulatorStates::set_RS(u32 a, u32 b, u32 c)
{
	States.erase(std::remove_if(States.begin(), States.end(),
			[&](auto& S)
			{
				return (0 == S.type) && (a == S.v1) && (c == S.v3 || c == u32(-1));
			}
		),
	States.end());

	States.push_back({ 0, a, b, c });
}

bool	SimulatorStates::equal	(SimulatorStates& S)
{
	if (States.size()!=S.States.size())												return false;
	if (0!=memcmp(&*States.begin(),&*S.States.begin(),States.size()*sizeof(State))) return false;
	return true;
}

void	SimulatorStates::clear	()
{
	States.clear();
}


#include "dx10StateUtils.h"

void SimulatorStates::UpdateState( GfxPipelineState &state) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			switch (S.v1)
			{
			case D3DRS_STENCILREF:
				state.StencilRef = S.v2;
				break;
			case D3DRS_ALPHAREF:
				state.AlphaRef = S.v2;
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc( RHIRasterizerDesc &desc ) const
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
					desc.FillMode = RHI_FILL_SOLID;
				else
				{
					VERIFY(S.v2==D3DFILL_WIREFRAME);
					desc.FillMode = RHI_FILL_WIREFRAME;
				}
				break;

			case D3DRS_CULLMODE:
				desc.CullMode = (ERHI_CULL_MODE)S.v2;
				break;
			}
		}
	}
}

void SimulatorStates::UpdateDesc( RHIDepthStencilDesc &desc ) const
{
	for (u32 it=0; it<States.size(); it++)
	{
		const State& S	= States[it];
		if (S.type==0)
		{
			switch (S.v1)
			{
			case D3DRS_ZWRITEENABLE:
				desc.DepthWriteMask = S.v2 ? RHI_DEPTH_WRITE_MASK_ALL : RHI_DEPTH_WRITE_MASK_ZERO;
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

void SimulatorStates::UpdateDesc( RHIBlendDesc &desc ) const
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

				desc.IndependentBlendEnable = true;
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
