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
#pragma once

inline D3D11_RASTERIZER_DESC
RHI_ConvertState(const RHIRasterizerDesc& desc)
{
	return { static_cast<D3D11_FILL_MODE>(desc.FillMode), static_cast<D3D11_CULL_MODE>(desc.CullMode),
		desc.FrontCounterClockwise, desc.DepthBias, desc.DepthBiasClamp, desc.SlopeScaledDepthBias,
		desc.DepthClipEnable, desc.ScissorEnable, desc.MultisampleEnable, desc.AntialiasedLineEnable };
}

inline D3D11_DEPTH_STENCILOP_DESC
RHI_ConvertState(const RHIStencilOpDesc& desc)
{
	return { static_cast<D3D11_STENCIL_OP>(desc.StencilFailOp), static_cast<D3D11_STENCIL_OP>(desc.StencilDepthFailOp),
		static_cast<D3D11_STENCIL_OP>(desc.StencilPassOp), static_cast<D3D11_COMPARISON_FUNC>(desc.StencilFunc) };
}

inline D3D11_DEPTH_STENCIL_DESC
RHI_ConvertState(const RHIDepthStencilDesc& desc)
{
	// Zero-filled: the state cache compares descs with memcmp, padding included.
	D3D11_DEPTH_STENCIL_DESC result;
	memset(&result, 0, sizeof(result));
	result.DepthEnable = desc.DepthEnable;
	result.DepthWriteMask = static_cast<D3D11_DEPTH_WRITE_MASK>(desc.DepthWriteMask);
	result.DepthFunc = static_cast<D3D11_COMPARISON_FUNC>(desc.DepthFunc);
	result.StencilEnable = desc.StencilEnable;
	result.StencilReadMask = desc.StencilReadMask;
	result.StencilWriteMask = desc.StencilWriteMask;
	result.FrontFace = RHI_ConvertState(desc.FrontFace);
	result.BackFace = RHI_ConvertState(desc.BackFace);
	return result;
}

inline D3D11_BLEND_DESC
RHI_ConvertState(const RHIBlendDesc& desc)
{
	D3D11_BLEND_DESC result;
	memset(&result, 0, sizeof(result));
	result.AlphaToCoverageEnable = desc.AlphaToCoverageEnable;
	result.IndependentBlendEnable = desc.IndependentBlendEnable;
	for (u32 target_idx = 0; target_idx < std::size(result.RenderTarget); ++target_idx) {
		const auto& target = desc.RenderTarget[target_idx];
		auto& native = result.RenderTarget[target_idx];
		native.BlendEnable = target.BlendEnable;
		native.SrcBlend = static_cast<D3D11_BLEND>(target.SrcBlend);
		native.DestBlend = static_cast<D3D11_BLEND>(target.DestBlend);
		native.BlendOp = static_cast<D3D11_BLEND_OP>(target.BlendOp);
		native.SrcBlendAlpha = static_cast<D3D11_BLEND>(target.SrcBlendAlpha);
		native.DestBlendAlpha = static_cast<D3D11_BLEND>(target.DestBlendAlpha);
		native.BlendOpAlpha = static_cast<D3D11_BLEND_OP>(target.BlendOpAlpha);
		native.RenderTargetWriteMask = target.RenderTargetWriteMask;
	}
	return result;
}
