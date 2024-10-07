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

/////////////////////////////////////////////////////////////////////
// Texture 1D Implementation

CD3D11Texture1D::CD3D11Texture1D() :
	m_pTexture1D(nullptr),
	m_pShaderResourceView(nullptr)
{
	memset(&m_TextureDesc, 0, sizeof(m_TextureDesc));

	AddRef();
}

CD3D11Texture1D::~CD3D11Texture1D()
{
	if (m_pShaderResourceView)
	{
		m_pShaderResourceView->Release();
		m_pShaderResourceView = nullptr;
	}

	if (m_pTexture1D)
	{
		m_pTexture1D->Release();
		m_pTexture1D = nullptr;
	}
}

HRESULT CD3D11Texture1D::Create(const STexture1DDesc& desc, const SubresourceData* pSubresourceData)
{
	m_TextureDesc = desc;

	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();

	D3D11_TEXTURE1D_DESC d3dTextureDesc = {};
	d3dTextureDesc.Width = desc.Width;
	d3dTextureDesc.MipLevels = desc.MipLevels;
	d3dTextureDesc.ArraySize = desc.ArraySize;
	d3dTextureDesc.Format = g_PixelFormats[desc.Format].PlatformFormat;
	d3dTextureDesc.Usage = GetD3D11Usage(desc.Usage);

	d3dTextureDesc.BindFlags |= D3D11_BIND_SHADER_RESOURCE;

	// #TODO: Make BindFlags for STexture1DDesc
	if (desc.IsRenderTarget)
		d3dTextureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;
	else if (desc.IsDepthStencil)
		d3dTextureDesc.BindFlags |= D3D11_BIND_DEPTH_STENCIL;

	// #TODO: Make CPUAccessFlags for STexture1DDesc
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

	HRESULT hr = pDevice->CreateTexture1D(&d3dTextureDesc, pSubresourceData ? subresourceData : NULL, &m_pTexture1D);
	R_CHK(hr);

	if ((d3dTextureDesc.BindFlags & D3D11_BIND_SHADER_RESOURCE) != 0)
	{
		D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc;
		memset(&shaderResourceViewDesc, 0, sizeof(shaderResourceViewDesc));
		shaderResourceViewDesc.Format = d3dTextureDesc.Format;
		shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE1D;
		shaderResourceViewDesc.Texture2D.MipLevels = desc.MipLevels;
		shaderResourceViewDesc.Texture2D.MostDetailedMip = 0;

		hr = pDevice->CreateShaderResourceView(m_pTexture1D, &shaderResourceViewDesc, &m_pShaderResourceView);
		R_CHK(hr);
	}

	return hr;
}

void CD3D11Texture1D::GetType(eResourceDimension* pResourceDimension)
{
	R_ASSERT(pResourceDimension);
	*pResourceDimension = RESOURCE_DIMENSION_TEXTURE1D;
}

void CD3D11Texture1D::SetDebugName(const char* name)
{
	R_ASSERT(m_pTexture1D);
	m_pTexture1D->SetPrivateData(WKPDID_D3DDebugObjectName, strlen(name), name);
}

void CD3D11Texture1D::GetDesc(STexture1DDesc* desc)
{
	R_ASSERT(desc);
	*desc = m_TextureDesc;
}

void CD3D11Texture1D::GetShaderResourceView(IShaderResourceView** ppShaderResourceView)
{
	R_ASSERT(ppShaderResourceView);
	*ppShaderResourceView = (IShaderResourceView*)m_pShaderResourceView;
}

void CD3D11Texture1D::Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex1D)
{
	R_CHK(g_RenderRHI_DX11Implementation.GetDeviceContext()->Map(m_pTexture1D, Subresource, GetD3D11Map(MapType), MapFlags, (D3D11_MAPPED_SUBRESOURCE*)pMappedTex1D));
}

void CD3D11Texture1D::Unmap(u32 Subresource)
{
	g_RenderRHI_DX11Implementation.GetDeviceContext()->Unmap(m_pTexture1D, Subresource);
}

/////////////////////////////////////////////////////////////////////
// Texture 2D Implementation

CD3D11Texture2D::CD3D11Texture2D() :
	m_pTexture2D(nullptr),
	m_pShaderResourceView(nullptr)
{
	memset(&m_TextureDesc, 0, sizeof(m_TextureDesc));

	AddRef();
}

CD3D11Texture2D::~CD3D11Texture2D()
{
	if (m_pShaderResourceView)
	{
		m_pShaderResourceView->Release();
		m_pShaderResourceView = nullptr;
	}

	if (m_pTexture2D)
	{
		m_pTexture2D->Release();
		m_pTexture2D = nullptr;
	}
}

HRESULT CD3D11Texture2D::Create(const STexture2DDesc& desc, const SubresourceData* pSubresourceData)
{
	m_TextureDesc = desc;

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
	
	if (!desc.NoShaderResourceView)
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

	HRESULT hr = pDevice->CreateTexture2D(&d3dTextureDesc, pSubresourceData ? subresourceData : NULL, &m_pTexture2D);
	R_CHK(hr);

	if ((d3dTextureDesc.BindFlags & D3D11_BIND_SHADER_RESOURCE) != 0 && !desc.NoShaderResourceView)
	{
		D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc;
		memset(&shaderResourceViewDesc, 0, sizeof(shaderResourceViewDesc));
		shaderResourceViewDesc.Format = d3dTextureDesc.Format;

		// #TODO: RHI - Ugly
		switch (d3dTextureDesc.Format)
		{
		case DXGI_FORMAT_R24G8_TYPELESS:
			shaderResourceViewDesc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
			break;
		case DXGI_FORMAT_R32_TYPELESS:
			shaderResourceViewDesc.Format = DXGI_FORMAT_D32_FLOAT;
			break;
		}

		shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
		shaderResourceViewDesc.Texture2D.MipLevels = desc.MipLevels;
		shaderResourceViewDesc.Texture2D.MostDetailedMip = 0;

		hr = pDevice->CreateShaderResourceView(m_pTexture2D, &shaderResourceViewDesc, &m_pShaderResourceView);
		R_CHK(hr);
	}

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

void CD3D11Texture2D::GetDesc(STexture2DDesc* desc)
{
	R_ASSERT(desc);
	*desc = m_TextureDesc;
}

void CD3D11Texture2D::GetShaderResourceView(IShaderResourceView** ppShaderResourceView)
{
	R_ASSERT(ppShaderResourceView);
	*ppShaderResourceView = (IShaderResourceView*)m_pShaderResourceView;
}

void CD3D11Texture2D::Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex2D)
{
	R_CHK(g_RenderRHI_DX11Implementation.GetDeviceContext()->Map(m_pTexture2D, Subresource, GetD3D11Map(MapType), MapFlags, (D3D11_MAPPED_SUBRESOURCE*)pMappedTex2D));
}

void CD3D11Texture2D::Unmap(u32 Subresource)
{
	g_RenderRHI_DX11Implementation.GetDeviceContext()->Unmap(m_pTexture2D, Subresource);
}

/////////////////////////////////////////////////////////////////////
// Texture 2D Implementation

CD3D11Texture3D::CD3D11Texture3D() :
	m_pTexture3D(nullptr),
	m_pShaderResourceView(nullptr)
{
	memset(&m_TextureDesc, 0, sizeof(m_TextureDesc));

	AddRef();
}

CD3D11Texture3D::~CD3D11Texture3D()
{
	if (m_pTexture3D)
	{
		m_pTexture3D->Release();
		m_pTexture3D = nullptr;
	}
}

HRESULT CD3D11Texture3D::Create(const STexture3DDesc& desc, const SubresourceData* pSubresourceData)
{
	ID3D11Device* pDevice = g_RenderRHI_DX11Implementation.GetDevice();

	D3D11_TEXTURE3D_DESC d3dTextureDesc = {};
	d3dTextureDesc.Width = desc.Width;
	d3dTextureDesc.Height = desc.Height;
	d3dTextureDesc.Depth = desc.Depth;
	d3dTextureDesc.MipLevels = desc.MipLevels;
	d3dTextureDesc.Format = g_PixelFormats[desc.Format].PlatformFormat;
	d3dTextureDesc.Usage = GetD3D11Usage(desc.Usage);

	d3dTextureDesc.BindFlags |= D3D11_BIND_SHADER_RESOURCE;

	// #TODO: Make BindFlags for STexture3DDesc
	if (desc.IsRenderTarget)
		d3dTextureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;
	else if (desc.IsDepthStencil)
		d3dTextureDesc.BindFlags |= D3D11_BIND_DEPTH_STENCIL;

	// #TODO: Make CPUAccessFlags for STexture3DDesc
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

	HRESULT hr = pDevice->CreateTexture3D(&d3dTextureDesc, pSubresourceData ? subresourceData : NULL, &m_pTexture3D);
	R_CHK(hr);

	if ((d3dTextureDesc.BindFlags & D3D11_BIND_SHADER_RESOURCE) != 0)
	{
		D3D11_SHADER_RESOURCE_VIEW_DESC shaderResourceViewDesc;
		memset(&shaderResourceViewDesc, 0, sizeof(shaderResourceViewDesc));
		shaderResourceViewDesc.Format = d3dTextureDesc.Format;
		shaderResourceViewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE3D;
		shaderResourceViewDesc.Texture3D.MipLevels = desc.MipLevels;
		shaderResourceViewDesc.Texture3D.MostDetailedMip = 0;

		hr = pDevice->CreateShaderResourceView(m_pTexture3D, &shaderResourceViewDesc, &m_pShaderResourceView);
		R_CHK(hr);
	}

	return hr;
}

void CD3D11Texture3D::GetType(eResourceDimension* pResourceDimension)
{
	R_ASSERT(pResourceDimension);
	*pResourceDimension = RESOURCE_DIMENSION_TEXTURE3D;
}

void CD3D11Texture3D::SetDebugName(const char* name)
{
	R_ASSERT(m_pTexture3D);
	m_pTexture3D->SetPrivateData(WKPDID_D3DDebugObjectName, strlen(name), name);
}

void CD3D11Texture3D::GetDesc(STexture3DDesc* desc)
{
	R_ASSERT(desc);
	*desc = m_TextureDesc;
}

void CD3D11Texture3D::GetShaderResourceView(IShaderResourceView** ppShaderResourceView)
{
	R_ASSERT(ppShaderResourceView);
	*ppShaderResourceView = (IShaderResourceView*)m_pShaderResourceView;
}

void CD3D11Texture3D::Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex3D)
{
	R_CHK(g_RenderRHI_DX11Implementation.GetDeviceContext()->Map(m_pTexture3D, Subresource, GetD3D11Map(MapType), MapFlags, (D3D11_MAPPED_SUBRESOURCE*)pMappedTex3D));
}

void CD3D11Texture3D::Unmap(u32 Subresource)
{
	g_RenderRHI_DX11Implementation.GetDeviceContext()->Unmap(m_pTexture3D, Subresource);
}

