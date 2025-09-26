#include "DX11TextureImplementations.h"
#include <d3d11.h>

// DX11 Surface implementation
DX11Surface::DX11Surface(ID3D11Texture2D* texture)
	: m_pTexture2D(texture), m_pResource(texture)
{
	if (texture)
	{
		D3D11_TEXTURE2D_DESC desc;
		texture->GetDesc(&desc);
		m_desc.Width = desc.Width;
		m_desc.Height = desc.Height;
		m_desc.Depth = 1;
		m_desc.MipLevels = desc.MipLevels;
		m_desc.Format = desc.Format;
		m_desc.Usage = desc.Usage;
		m_desc.BindFlags = desc.BindFlags;
		m_desc.CPUAccessFlags = desc.CPUAccessFlags;
		m_desc.MiscFlags = desc.MiscFlags;
	}
}

DX11Surface::DX11Surface(ID3D11Texture3D* texture)
	: m_pTexture3D(texture), m_pResource(texture)
{
	if (texture)
	{
		D3D11_TEXTURE3D_DESC desc;
		texture->GetDesc(&desc);
		m_desc.Width = desc.Width;
		m_desc.Height = desc.Height;
		m_desc.Depth = desc.Depth;
		m_desc.MipLevels = desc.MipLevels;
		m_desc.Format = desc.Format;
		m_desc.Usage = desc.Usage;
		m_desc.BindFlags = desc.BindFlags;
		m_desc.CPUAccessFlags = desc.CPUAccessFlags;
		m_desc.MiscFlags = desc.MiscFlags;
	}
}

DX11Surface::DX11Surface(ID3D11Texture1D* texture)
	: m_pTexture1D(texture), m_pResource(texture)
{
	if (texture)
	{
		D3D11_TEXTURE1D_DESC desc;
		texture->GetDesc(&desc);
		m_desc.Width = desc.Width;
		m_desc.Height = 1;
		m_desc.Depth = 1;
		m_desc.MipLevels = desc.MipLevels;
		m_desc.Format = desc.Format;
		m_desc.Usage = desc.Usage;
		m_desc.BindFlags = desc.BindFlags;
		m_desc.CPUAccessFlags = desc.CPUAccessFlags;
		m_desc.MiscFlags = desc.MiscFlags;
	}
}

DX11Surface::~DX11Surface()
{
	if (m_pSRV) delete m_pSRV;
	if (m_pRTV) delete m_pRTV;
	if (m_pDSV) delete m_pDSV;
	if (m_pResource) m_pResource->Release();
}

void* DX11Surface::GetRawTexture()
{
	return m_pResource;
}

void DX11Surface::AddRef()
{
	if (m_pResource) m_pResource->AddRef();
}

u32 DX11Surface::Release()
{
	if (m_pResource) return m_pResource->Release();
	return 0;
}

u32 DX11Surface::GetWidth() const
{
	return m_desc.Width;
}

u32 DX11Surface::GetHeight() const
{
	return m_desc.Height;
}

u32 DX11Surface::GetDepth() const
{
	return m_desc.Depth;
}

u32 DX11Surface::GetMipLevels() const
{
	return m_desc.MipLevels;
}

u32 DX11Surface::GetFormat() const
{
	return m_desc.Format;
}

u32 DX11Surface::GetTextureType() const
{
	D3D11_RESOURCE_DIMENSION DescInfo;
	m_pResource->GetType(&DescInfo);
	//if (m_pTexture2D) return D3D11_RESOURCE_DIMENSION_TEXTURE2D;
	//if (m_pTexture3D) return D3D11_RESOURCE_DIMENSION_TEXTURE3D;
	//if (m_pTexture1D) return D3D11_RESOURCE_DIMENSION_TEXTURE1D;
	return DescInfo;
}

IRHIShaderResourceView* DX11Surface::GetShaderResourceView()
{
	if (!m_pSRV && m_pResource)
	{
		// Create SRV if needed
		// This would need device context to create SRV
	}
	return m_pSRV;
}

IRHIRenderTargetView* DX11Surface::GetRenderTargetView()
{
	return m_pRTV;
}

IRHIDepthStencilView* DX11Surface::GetDepthStencilView()
{
	return m_pDSV;
}

bool DX11Surface::UpdateData(const void* data, u32 size)
{
	// Implementation would depend on usage flags
	return false;
}

void* DX11Surface::Lock(u32 mipLevel, u32* pitch)
{
	// Implementation would depend on usage flags
	return nullptr;
}

void DX11Surface::Unlock()
{
	// Implementation would depend on usage flags
}

// DX11 Shader Resource View implementation
DX11ShaderResourceView::DX11ShaderResourceView(ID3D11ShaderResourceView* srv, IRHISurface* surface)
	: m_pSRV(srv), m_pSurface(surface)
{
}

DX11ShaderResourceView::~DX11ShaderResourceView()
{
	if (m_pSRV) m_pSRV->Release();
}

void* DX11ShaderResourceView::GetRawSRV()
{
	return m_pSRV;
}

void DX11ShaderResourceView::AddRef()
{
	if (m_pSRV) m_pSRV->AddRef();
}

u32 DX11ShaderResourceView::Release()
{
	if (m_pSRV) return m_pSRV->Release();
	return 0;
}

IRHISurface* DX11ShaderResourceView::GetSurface()
{
	return m_pSurface;
}

void DX11ShaderResourceView::BindToPixelShader(u32 slot)
{
	if (m_pSRV)
	{
		// Would need device context
		// RContext->PSSetShaderResources(slot, 1, &m_pSRV);
	}
}

void DX11ShaderResourceView::BindToVertexShader(u32 slot)
{
	if (m_pSRV)
	{
		// Would need device context
		// RContext->VSSetShaderResources(slot, 1, &m_pSRV);
	}
}

void DX11ShaderResourceView::BindToGeometryShader(u32 slot)
{
	if (m_pSRV)
	{
		// Would need device context
		// RContext->GSSetShaderResources(slot, 1, &m_pSRV);
	}
}

void DX11ShaderResourceView::BindToComputeShader(u32 slot)
{
	if (m_pSRV)
	{
		// Would need device context
		// RContext->CSSetShaderResources(slot, 1, &m_pSRV);
	}
}

// DX11 Render Target View implementation
DX11RenderTargetView::DX11RenderTargetView(ID3D11RenderTargetView* rtv, IRHISurface* surface)
	: m_pRTV(rtv), m_pSurface(surface)
{
}

DX11RenderTargetView::~DX11RenderTargetView()
{
	if (m_pRTV) m_pRTV->Release();
}

void* DX11RenderTargetView::GetRawRTV()
{
	return m_pRTV;
}

void DX11RenderTargetView::AddRef()
{
	if (m_pRTV) m_pRTV->AddRef();
}

u32 DX11RenderTargetView::Release()
{
	if (m_pRTV) return m_pRTV->Release();
	return 0;
}

IRHISurface* DX11RenderTargetView::GetSurface()
{
	return m_pSurface;
}

void DX11RenderTargetView::BindAsRenderTarget(u32 slot)
{
	if (m_pRTV)
	{
		// Would need device context
		// RContext->OMSetRenderTargets(1, &m_pRTV, nullptr);
	}
}

void DX11RenderTargetView::UnbindRenderTarget()
{
	// Would need device context
	// RContext->OMSetRenderTargets(0, nullptr, nullptr);
}

void DX11RenderTargetView::Clear(float r, float g, float b, float a)
{
	if (m_pRTV)
	{
		float color[4] = { r, g, b, a };
		// Would need device context
		// RContext->ClearRenderTargetView(m_pRTV, color);
	}
}

DX11DepthStencilView::DX11DepthStencilView(ID3D11DepthStencilView* dsv, IRHISurface* surface)
	: m_pDSV(dsv), m_pSurface(surface)
{
}

DX11DepthStencilView::~DX11DepthStencilView()
{
	if (m_pDSV) m_pDSV->Release();
}

void* DX11DepthStencilView::GetRawDSV()
{
	return m_pDSV;
}

void DX11DepthStencilView::AddRef()
{
	if (m_pDSV) m_pDSV->AddRef();
}

u32 DX11DepthStencilView::Release()
{
	if (m_pDSV) return m_pDSV->Release();
	return 0;
}

IRHISurface* DX11DepthStencilView::GetSurface()
{
	return m_pSurface;
}

void DX11DepthStencilView::BindAsDepthStencil()
{
	if (m_pDSV)
	{
		// RContext->OMSetRenderTargets(0, nullptr, m_pDSV);
	}
}

void DX11DepthStencilView::UnbindDepthStencil()
{
	// RContext->OMSetRenderTargets(0, nullptr, nullptr);
}

void DX11DepthStencilView::ClearDepth(float depth)
{
	if (m_pDSV)
	{
		// RContext->ClearDepthStencilView(m_pDSV, D3D11_CLEAR_DEPTH, depth, 0);
	}
}

void DX11DepthStencilView::ClearStencil(u8 stencil)
{
	if (m_pDSV)
	{
		// RContext->ClearDepthStencilView(m_pDSV, D3D11_CLEAR_STENCIL, 1.0f, stencil);
	}
}

void DX11DepthStencilView::ClearDepthStencil(float depth, u8 stencil)
{
	if (m_pDSV)
	{
		// RContext->ClearDepthStencilView(m_pDSV, D3D11_CLEAR_DEPTH | D3D11_CLEAR_STENCIL, depth, stencil);
	}
}

DX11TextureFactory::DX11TextureFactory(ID3D11Device* device, ID3D11DeviceContext* context)
	: m_pDevice(device), m_pContext(context)
{
}

DX11TextureFactory::~DX11TextureFactory()
{
}

IRHISurface* DX11TextureFactory::CreateTextureFromFile(const char* filename, u32& memorySize)
{
	return nullptr;
}

IRHISurface* DX11TextureFactory::CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc)
{
	if (data && size == 0)
	{
		ID3D11Resource* Resource = static_cast<ID3D11Resource*>(const_cast<void*>(data));

		if (Resource)
		{
			D3D11_RESOURCE_DIMENSION Info;
			Resource->GetType(&Info);

			switch (Info)
			{
				case D3D11_RESOURCE_DIMENSION_TEXTURE1D: return new DX11Surface((ID3D11Texture1D*)Resource);
				case D3D11_RESOURCE_DIMENSION_TEXTURE2D: return new DX11Surface((ID3D11Texture2D*)Resource);
				case D3D11_RESOURCE_DIMENSION_TEXTURE3D: return new DX11Surface((ID3D11Texture3D*)Resource);
			}

			VERIFY(!"Invalid texture type");
		}
	}
	
	D3D11_TEXTURE2D_DESC d3dDesc = {};
	d3dDesc.Width = desc.Width;
	d3dDesc.Height = desc.Height;
	d3dDesc.MipLevels = desc.MipLevels;
	d3dDesc.ArraySize = 1;
	d3dDesc.Format = ConvertFormat(desc.Format);
	d3dDesc.SampleDesc.Count = 1;
	d3dDesc.SampleDesc.Quality = 0;
	d3dDesc.Usage = ConvertUsage(desc.Usage);
	d3dDesc.BindFlags = ConvertBindFlags(desc.BindFlags);
	d3dDesc.CPUAccessFlags = ConvertCPUAccessFlags(desc.CPUAccessFlags);
	d3dDesc.MiscFlags = ConvertMiscFlags(desc.MiscFlags);

	ID3D11Texture2D* texture = nullptr;
	HRESULT hr = m_pDevice->CreateTexture2D(&d3dDesc, nullptr, &texture);
	if (FAILED(hr))
		return nullptr;

	return new DX11Surface(texture);
}

IRHISurface* DX11TextureFactory::CreateRenderTarget(const RHITextureDesc& desc)
{
	D3D11_TEXTURE2D_DESC d3dDesc = {};
	d3dDesc.Width = desc.Width;
	d3dDesc.Height = desc.Height;
	d3dDesc.MipLevels = desc.MipLevels;
	d3dDesc.ArraySize = desc.ArraySize;
	d3dDesc.Format = ConvertFormat(desc.Format);
	d3dDesc.SampleDesc.Count = desc.Depth;
	d3dDesc.SampleDesc.Quality = 0;
	d3dDesc.Usage = ConvertUsage(desc.Usage);
	d3dDesc.BindFlags = ConvertBindFlags(desc.BindFlags);
	d3dDesc.CPUAccessFlags = ConvertCPUAccessFlags(desc.CPUAccessFlags);
	d3dDesc.MiscFlags = ConvertMiscFlags(desc.MiscFlags);

	ID3D11Texture2D* texture = nullptr;
	HRESULT hr = m_pDevice->CreateTexture2D(&d3dDesc, nullptr, &texture);
	if (FAILED(hr))
		return nullptr;

	return new DX11Surface(texture);
}

IRHISurface* DX11TextureFactory::CreateDepthStencil(const RHITextureDesc& desc)
{
	// Similar to CreateRenderTarget but with depth format
	return nullptr;
}

IRHIShaderResourceView* DX11TextureFactory::CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc)
{
	DX11Surface* dx11Surface = static_cast<DX11Surface*>(surface);
	if (!dx11Surface)
		return nullptr;

	D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
	if (desc != nullptr)
	{
		srvDesc.Format = (DXGI_FORMAT)desc->Format;
		srvDesc.ViewDimension = (D3D11_SRV_DIMENSION)desc->ViewDimension;

		if (desc->ViewDimension == D3D11_SRV_DIMENSION_TEXTURE2D)
		{
			srvDesc.Texture2D.MostDetailedMip = desc->MostDetailedMip;
			srvDesc.Texture2D.MipLevels = desc->MipLevels;
		}
		else if (desc->ViewDimension == D3D11_SRV_DIMENSION_TEXTURE2DARRAY)
		{
			srvDesc.Texture2DArray.MostDetailedMip = desc->MostDetailedMip;
			srvDesc.Texture2DArray.MipLevels = desc->MipLevels;
			srvDesc.Texture2DArray.FirstArraySlice = desc->FirstArraySlice;
			srvDesc.Texture2DArray.ArraySize = desc->ArraySize;
		}
		else if (desc->ViewDimension == D3D11_SRV_DIMENSION_TEXTURE2DMS)
		{
			srvDesc.Texture2DMS.UnusedField_NothingToDefine = 0;
		}
		else if (desc->ViewDimension == D3D11_SRV_DIMENSION_TEXTURECUBE)
		{
			srvDesc.TextureCube.MostDetailedMip = desc->MostDetailedMip;
			srvDesc.TextureCube.MipLevels = desc->MipLevels;
		}
	}

	ID3D11ShaderResourceView* srv = nullptr;
	HRESULT hr = m_pDevice->CreateShaderResourceView(dx11Surface->GetDX11Resource(), desc != nullptr ? &srvDesc : nullptr , &srv);
	if (FAILED(hr))
		return nullptr;

	return new DX11ShaderResourceView(srv, surface);
}

IRHIRenderTargetView* DX11TextureFactory::CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc)
{
	DX11Surface* dx11Surface = static_cast<DX11Surface*>(surface);
	if (!dx11Surface)
		return nullptr;

	D3D11_RENDER_TARGET_VIEW_DESC rtvDesc = {};
	rtvDesc.Format = (DXGI_FORMAT)desc.Format;
	rtvDesc.ViewDimension = (D3D11_RTV_DIMENSION)desc.ViewDimension;
	
	if (desc.ViewDimension == D3D11_RTV_DIMENSION_TEXTURE2D)
	{
		rtvDesc.Texture2D.MipSlice = desc.MipSlice;
	}
	else if (desc.ViewDimension == D3D11_RTV_DIMENSION_TEXTURE2DARRAY)
	{
		rtvDesc.Texture2DArray.MipSlice = desc.MipSlice;
		rtvDesc.Texture2DArray.FirstArraySlice = desc.FirstArraySlice;
		rtvDesc.Texture2DArray.ArraySize = desc.ArraySize;
	}

	ID3D11RenderTargetView* rtv = nullptr;
	HRESULT hr = m_pDevice->CreateRenderTargetView(dx11Surface->GetDX11Resource(), &rtvDesc, &rtv);
	if (FAILED(hr))
		return nullptr;

	return new DX11RenderTargetView(rtv, surface);
}

IRHIDepthStencilView* DX11TextureFactory::CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc)
{
	DX11Surface* dx11Surface = static_cast<DX11Surface*>(surface);
	if (!dx11Surface)
		return nullptr;

	D3D11_DEPTH_STENCIL_VIEW_DESC dsvDesc = {};
	dsvDesc.Format = (DXGI_FORMAT)desc.Format;
	dsvDesc.ViewDimension = (D3D11_DSV_DIMENSION)desc.ViewDimension;
	dsvDesc.Flags = desc.Flags;
	
	if (desc.ViewDimension == D3D11_DSV_DIMENSION_TEXTURE2D) {
		dsvDesc.Texture2D.MipSlice = desc.MipSlice;
	} else if (desc.ViewDimension == D3D11_DSV_DIMENSION_TEXTURE2DARRAY) {
		dsvDesc.Texture2DArray.MipSlice = desc.MipSlice;
		dsvDesc.Texture2DArray.FirstArraySlice = desc.FirstArraySlice;
		dsvDesc.Texture2DArray.ArraySize = desc.ArraySize;
	}

	ID3D11DepthStencilView* dsv = nullptr;
	HRESULT hr = m_pDevice->CreateDepthStencilView(dx11Surface->GetDX11Resource(), &dsvDesc, &dsv);
	if (FAILED(hr))
		return nullptr;

	return new DX11DepthStencilView(dsv, surface);
}

// Helper methods
DXGI_FORMAT DX11TextureFactory::ConvertFormat(u32 format)
{
	return static_cast<DXGI_FORMAT>(format);
}

D3D11_USAGE DX11TextureFactory::ConvertUsage(u32 usage)
{
	return static_cast<D3D11_USAGE>(usage);
}

u32 DX11TextureFactory::ConvertBindFlags(u32 bindFlags)
{
	return bindFlags;
}

u32 DX11TextureFactory::ConvertCPUAccessFlags(u32 cpuAccessFlags)
{
	return cpuAccessFlags;
}

u32 DX11TextureFactory::ConvertMiscFlags(u32 miscFlags)
{
	return miscFlags;
}
