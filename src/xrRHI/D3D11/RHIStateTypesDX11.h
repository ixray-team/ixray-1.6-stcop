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
	return { desc.DepthEnable, static_cast<D3D11_DEPTH_WRITE_MASK>(desc.DepthWriteMask), static_cast<D3D11_COMPARISON_FUNC>(desc.DepthFunc),
		desc.StencilEnable, desc.StencilReadMask, desc.StencilWriteMask, RHI_ConvertState(desc.FrontFace), RHI_ConvertState(desc.BackFace) };
}

inline D3D11_BLEND_DESC
RHI_ConvertState(const RHIBlendDesc& desc)
{
	D3D11_BLEND_DESC result = {};
	result.AlphaToCoverageEnable = desc.AlphaToCoverageEnable;
	result.IndependentBlendEnable = desc.IndependentBlendEnable;
	for (u32 target_idx = 0; target_idx < std::size(result.RenderTarget); ++target_idx) {
		const auto& target = desc.RenderTarget[target_idx];
		result.RenderTarget[target_idx] = { target.BlendEnable, static_cast<D3D11_BLEND>(target.SrcBlend), static_cast<D3D11_BLEND>(target.DestBlend),
			static_cast<D3D11_BLEND_OP>(target.BlendOp), static_cast<D3D11_BLEND>(target.SrcBlendAlpha), static_cast<D3D11_BLEND>(target.DestBlendAlpha),
			static_cast<D3D11_BLEND_OP>(target.BlendOpAlpha), target.RenderTargetWriteMask };
	}
	return result;
}
