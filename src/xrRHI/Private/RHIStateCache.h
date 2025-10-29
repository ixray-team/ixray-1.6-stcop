#pragma once
#include "../RHI.h"
#include <d3d11.h>

inline bool operator<(const D3D11_RASTERIZER_DESC& lhs, const D3D11_RASTERIZER_DESC& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(D3D11_RASTERIZER_DESC)) < 0;
}

inline bool operator<(const D3D11_DEPTH_STENCIL_DESC& lhs, const D3D11_DEPTH_STENCIL_DESC& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(D3D11_DEPTH_STENCIL_DESC)) < 0;
}

inline bool operator<(const D3D11_BLEND_DESC& lhs, const D3D11_BLEND_DESC& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(D3D11_BLEND_DESC)) < 0;
}

// Hash functions for D3D11 state descriptions
namespace std
{
	template<>
	struct hash<D3D11_RASTERIZER_DESC>
	{
		size_t operator()(const D3D11_RASTERIZER_DESC& desc) const
		{
			size_t hash = 0;
			hash = desc.FillMode;
			hash = hash * 31 + desc.CullMode;
			hash = hash * 31 + desc.FrontCounterClockwise;
			hash = hash * 31 + desc.DepthBias;
			hash = hash * 31 + *reinterpret_cast<const UINT*>(&desc.DepthBiasClamp);
			hash = hash * 31 + *reinterpret_cast<const UINT*>(&desc.SlopeScaledDepthBias);
			hash = hash * 31 + desc.DepthClipEnable;
			hash = hash * 31 + desc.ScissorEnable;
			hash = hash * 31 + desc.MultisampleEnable;
			hash = hash * 31 + desc.AntialiasedLineEnable;
			return hash;
		}
	};

	template<>
	struct hash<D3D11_DEPTH_STENCIL_DESC>
	{
		size_t operator()(const D3D11_DEPTH_STENCIL_DESC& desc) const
		{
			size_t hash = 0;
			hash = desc.DepthEnable;
			hash = hash * 31 + desc.DepthWriteMask;
			hash = hash * 31 + desc.DepthFunc;
			hash = hash * 31 + desc.StencilEnable;
			hash = hash * 31 + desc.StencilReadMask;
			hash = hash * 31 + desc.StencilWriteMask;
			
			// Front face
			hash = hash * 31 + desc.FrontFace.StencilFailOp;
			hash = hash * 31 + desc.FrontFace.StencilDepthFailOp;
			hash = hash * 31 + desc.FrontFace.StencilPassOp;
			hash = hash * 31 + desc.FrontFace.StencilFunc;
			
			// Back face
			hash = hash * 31 + desc.BackFace.StencilFailOp;
			hash = hash * 31 + desc.BackFace.StencilDepthFailOp;
			hash = hash * 31 + desc.BackFace.StencilPassOp;
			hash = hash * 31 + desc.BackFace.StencilFunc;
			
			return hash;
		}
	};

	template<>
	struct hash<D3D11_BLEND_DESC>
	{
		size_t operator()(const D3D11_BLEND_DESC& desc) const
		{
			size_t hash = 0;
			hash = desc.AlphaToCoverageEnable;
			hash = hash * 31 + desc.IndependentBlendEnable;
			
			for (int i = 0; i < 8; ++i)
			{
				const auto& rt = desc.RenderTarget[i];
				hash = hash * 31 + rt.BlendEnable;
				hash = hash * 31 + rt.SrcBlend;
				hash = hash * 31 + rt.DestBlend;
				hash = hash * 31 + rt.BlendOp;
				hash = hash * 31 + rt.SrcBlendAlpha;
				hash = hash * 31 + rt.DestBlendAlpha;
				hash = hash * 31 + rt.BlendOpAlpha;
				hash = hash * 31 + rt.RenderTargetWriteMask;
			}
			
			return hash;
		}
	};
}

class RHIStateCache
{
public:
	RHIStateCache(ID3D11Device* NewDevice);
	~RHIStateCache();

	ID3D11RasterizerState* GetRasterizerState(const D3D11_RASTERIZER_DESC& Desc);
	ID3D11DepthStencilState* GetDepthStencilState(const D3D11_DEPTH_STENCIL_DESC& Desc);
	ID3D11BlendState* GetBlendState(const D3D11_BLEND_DESC& Desc);

	void ClearCache();

private:
	ID3D11Device* Device;

	xr_map<D3D11_RASTERIZER_DESC, ID3D11RasterizerState*> RSCache;
	xr_map<D3D11_DEPTH_STENCIL_DESC, ID3D11DepthStencilState*> DSSCache;
	xr_map<D3D11_BLEND_DESC, ID3D11BlendState*> BSCache;
};