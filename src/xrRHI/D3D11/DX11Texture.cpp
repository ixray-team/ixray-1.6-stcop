#include "DX11Texture.h"
#include <d3d11.h>

// DX11 Surface implementation
DX11Surface::DX11Surface(ID3D11Texture2D* texture)
	: Texture2D(texture), Resource(texture)
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
	: Texture3D(texture), Resource(texture)
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
	: Texture1D(texture), Resource(texture)
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
	xr_delete(SRV);
	xr_delete(RTV);
	xr_delete(DSV);

	if (Resource)
	{
		Resource->Release();
	}
}

void* DX11Surface::GetRawTexture()
{
	return Resource;
}

void DX11Surface::AddRef()
{
	if (Resource) Resource->AddRef();
}

u32 DX11Surface::Release()
{
	if (Resource) return Resource->Release();
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

u32 DX11Surface::GetMiscFlags() const
{
	return m_desc.MiscFlags;
}

u32 DX11Surface::GetTextureType() const
{
	D3D11_RESOURCE_DIMENSION DescInfo;
	Resource->GetType(&DescInfo);
	//if (Texture2D) return D3D11_RESOURCE_DIMENSION_TEXTURE2D;
	//if (Texture3D) return D3D11_RESOURCE_DIMENSION_TEXTURE3D;
	//if (Texture1D) return D3D11_RESOURCE_DIMENSION_TEXTURE1D;
	return DescInfo;
}

ERHI_USAGE DX11Surface::GetUsage() const
{
	return (ERHI_USAGE)m_desc.Usage;
}

IRHIShaderResourceView* DX11Surface::GetShaderResourceView()
{
	if (!SRV && Resource)
	{
		// Create SRV if needed
		// This would need device context to create SRV
	}
	return SRV;
}

IRHIRenderTargetView* DX11Surface::GetRenderTargetView()
{
	return RTV;
}

IRHIDepthStencilView* DX11Surface::GetDepthStencilView()
{
	return DSV;
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
	: SRV(srv), Surface(surface)
{
}

DX11ShaderResourceView::~DX11ShaderResourceView()
{
	if (SRV) SRV->Release();
}

void* DX11ShaderResourceView::GetRawSRV()
{
	return SRV;
}

void DX11ShaderResourceView::AddRef()
{
	if (SRV) SRV->AddRef();
}

u32 DX11ShaderResourceView::Release()
{
	if (SRV) return SRV->Release();
	return 0;
}

IRHISurface* DX11ShaderResourceView::GetSurface()
{
	return Surface;
}

void DX11ShaderResourceView::BindToPixelShader(u32 slot)
{
	if (SRV)
	{
		// Would need device context
		// RContext->PSSetShaderResources(slot, 1, &SRV);
	}
}

void DX11ShaderResourceView::BindToVertexShader(u32 slot)
{
	if (SRV)
	{
		// Would need device context
		// RContext->VSSetShaderResources(slot, 1, &SRV);
	}
}

void DX11ShaderResourceView::BindToGeometryShader(u32 slot)
{
	if (SRV)
	{
		// Would need device context
		// RContext->GSSetShaderResources(slot, 1, &SRV);
	}
}

void DX11ShaderResourceView::BindToComputeShader(u32 slot)
{
	if (SRV)
	{
		// Would need device context
		// RContext->CSSetShaderResources(slot, 1, &SRV);
	}
}

// DX11 Render Target View implementation
DX11RenderTargetView::DX11RenderTargetView(ID3D11RenderTargetView* rtv, IRHISurface* surface)
	: RTV(rtv), Surface(surface)
{
}

DX11RenderTargetView::~DX11RenderTargetView()
{
	if (RTV) RTV->Release();
}

void* DX11RenderTargetView::GetRawRTV()
{
	return RTV;
}

void DX11RenderTargetView::AddRef()
{
	if (RTV) RTV->AddRef();
}

u32 DX11RenderTargetView::Release()
{
	if (RTV) return RTV->Release();
	return 0;
}

IRHISurface* DX11RenderTargetView::GetSurface()
{
	return Surface;
}

void DX11RenderTargetView::BindAsRenderTarget(u32 slot)
{
	if (RTV)
	{
		// Would need device context
		// RContext->OMSetRenderTargets(1, &RTV, nullptr);
	}
}

void DX11RenderTargetView::UnbindRenderTarget()
{
	// Would need device context
	// RContext->OMSetRenderTargets(0, nullptr, nullptr);
}

void DX11RenderTargetView::Clear(float r, float g, float b, float a)
{
	if (RTV)
	{
		float color[4] = { r, g, b, a };
		// Would need device context
		// RContext->ClearRenderTargetView(RTV, color);
	}
}

DX11DepthStencilView::DX11DepthStencilView(ID3D11DepthStencilView* dsv, IRHISurface* surface)
	: DSV(dsv), Surface(surface)
{
}

DX11DepthStencilView::~DX11DepthStencilView()
{
	if (DSV)
	{
		DSV->Release();
	}
}

void* DX11DepthStencilView::GetRawDSV()
{
	return DSV;
}

void DX11DepthStencilView::AddRef()
{
	if (DSV)
	{
		DSV->AddRef();
	}
}

u32 DX11DepthStencilView::Release()
{
	if (DSV)
	{
		return DSV->Release();
	}
	return 0;
}

IRHISurface* DX11DepthStencilView::GetSurface()
{
	return Surface;
}

void DX11DepthStencilView::BindAsDepthStencil()
{
	if (DSV)
	{
		// RContext->OMSetRenderTargets(0, nullptr, DSV);
	}
}

void DX11DepthStencilView::UnbindDepthStencil()
{
	// RContext->OMSetRenderTargets(0, nullptr, nullptr);
}

DX11TextureFactory::DX11TextureFactory(ID3D11Device* device, ID3D11DeviceContext* context)
	: Device(device), Context(context)
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
	HRESULT hr = Device->CreateTexture2D(&d3dDesc, nullptr, &texture);
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
	HRESULT hr = Device->CreateTexture2D(&d3dDesc, nullptr, &texture);
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
	HRESULT hr = Device->CreateShaderResourceView(dx11Surface->GetDX11Resource(), desc != nullptr ? &srvDesc : nullptr , &srv);
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
	HRESULT hr = Device->CreateRenderTargetView(dx11Surface->GetDX11Resource(), &rtvDesc, &rtv);
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
	
	if (desc.ViewDimension == D3D11_DSV_DIMENSION_TEXTURE2D)
	{
		dsvDesc.Texture2D.MipSlice = desc.MipSlice;
	}
	else if (desc.ViewDimension == D3D11_DSV_DIMENSION_TEXTURE2DARRAY)
	{
		dsvDesc.Texture2DArray.MipSlice = desc.MipSlice;
		dsvDesc.Texture2DArray.FirstArraySlice = desc.FirstArraySlice;
		dsvDesc.Texture2DArray.ArraySize = desc.ArraySize;
	}

	ID3D11DepthStencilView* dsv = nullptr;
	HRESULT hr = Device->CreateDepthStencilView(dx11Surface->GetDX11Resource(), &dsvDesc, &dsv);
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
