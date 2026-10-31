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
