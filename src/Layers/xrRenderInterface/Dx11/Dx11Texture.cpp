#include "stdafx.h"
#include "Dx11Texture.h"

D3D11_USAGE GetD3D11Usage(eResourceUsage usage)
{
	switch (usage)
	{
	case USAGE_DEFAULT:
		return D3D11_USAGE_DEFAULT;
	case USAGE_IMMUTABLE:
		return D3D11_USAGE_IMMUTABLE;
	case USAGE_DYNAMIC:
		return D3D11_USAGE_DYNAMIC;
	case USAGE_STAGING:
		return D3D11_USAGE_STAGING;
	}

	R_ASSERT(0); // failure...
	return D3D11_USAGE_DEFAULT;
}

CD3D11Texture2D::CD3D11Texture2D() :
	m_pTexture2D(nullptr)
{
	AddRef();
}

CD3D11Texture2D::~CD3D11Texture2D()
{
	if (m_pTexture2D)
	{
		m_pTexture2D->Release();
		m_pTexture2D = nullptr;
	}
}

HRESULT CD3D11Texture2D::Create(const STexture2DDesc& desc, const SubresourceData* pSubresourceData)
{
	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();
	
	D3D11_TEXTURE2D_DESC d3dTextureDesc = {};
	d3dTextureDesc.Width = desc.Width;
	d3dTextureDesc.Height = desc.Height;
	d3dTextureDesc.MipLevels = desc.MipLevels;
	d3dTextureDesc.ArraySize = desc.ArraySize;
	d3dTextureDesc.Format = g_PixelFormats[desc.Format].PlatformFormat;
	d3dTextureDesc.SampleDesc.Quality = 0;
	d3dTextureDesc.SampleDesc.Count = 1;
	d3dTextureDesc.Usage = GetD3D11Usage(desc.Usage);
	
	d3dTextureDesc.BindFlags |= D3D11_BIND_SHADER_RESOURCE;

	// #TODO: Make BindFlags for STexture2DDesc
	if (desc.IsRenderTarget)
		d3dTextureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;
	else if (desc.IsDepthStencil)
		d3dTextureDesc.BindFlags |= D3D11_BIND_DEPTH_STENCIL;

	// #TODO: Make CPUAccessFlags for STexture2DDesc
	d3dTextureDesc.CPUAccessFlags = (desc.Usage == USAGE_DYNAMIC ? D3D11_CPU_ACCESS_WRITE : 0);
	d3dTextureDesc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subresourceData[16] = {};
	if (pSubresourceData)
	{
		for (int i = 0; i < desc.MipLevels; i++)
		{
			const SubresourceData* it = pSubresourceData + i;
			subresourceData[i].pSysMem = it->pSysMem;
			subresourceData[i].SysMemPitch = it->SysMemPitch;
			subresourceData[i].SysMemSlicePitch = it->SysMemSlicePitch;
		}
	}

	//R_CHK(pDevice->CreateTexture2D(&d3dTextureDesc, pSubresourceData ? subresourceData : NULL, &m_pTexture2D));
	HRESULT hr = pDevice->CreateTexture2D(&d3dTextureDesc, pSubresourceData ? subresourceData : NULL, &m_pTexture2D);
	return hr;
}

void CD3D11Texture2D::GetType(eResourceDimension* pResourceDimension)
{
	R_ASSERT(pResourceDimension);
	*pResourceDimension = RESOURCE_DIMENSION_TEXTURE2D;
}

void CD3D11Texture2D::SetDebugName(const char* name)
{
	R_ASSERT(m_pTexture2D);
	m_pTexture2D->SetPrivateData(WKPDID_D3DDebugObjectName, strlen(name), name);
}
