#include "DX11Texture.h"
#include <d3d11.h>

u32 GetFormatSize(DXGI_FORMAT format)
{
	switch (format)
	{
		// 8-bit per channel
	case DXGI_FORMAT_R8_UNORM:
	case DXGI_FORMAT_R8_UINT:
	case DXGI_FORMAT_R8_SNORM:
	case DXGI_FORMAT_R8_SINT:
	case DXGI_FORMAT_A8_UNORM:
		return 1;

		// 16-bit per channel
	case DXGI_FORMAT_R16_FLOAT:
	case DXGI_FORMAT_R16_UNORM:
	case DXGI_FORMAT_R16_UINT:
	case DXGI_FORMAT_R16_SNORM:
	case DXGI_FORMAT_R16_SINT:
	case DXGI_FORMAT_R8G8_UNORM:
	case DXGI_FORMAT_R8G8_UINT:
	case DXGI_FORMAT_R8G8_SNORM:
	case DXGI_FORMAT_R8G8_SINT:
	case DXGI_FORMAT_B5G6R5_UNORM:
	case DXGI_FORMAT_B5G5R5A1_UNORM:
	case DXGI_FORMAT_B4G4R4A4_UNORM:
		return 2;

		// 32-bit per channel
	case DXGI_FORMAT_R32_FLOAT:
	case DXGI_FORMAT_R32_UINT:
	case DXGI_FORMAT_R32_SINT:
	case DXGI_FORMAT_R16G16_FLOAT:
	case DXGI_FORMAT_R16G16_UNORM:
	case DXGI_FORMAT_R16G16_UINT:
	case DXGI_FORMAT_R16G16_SNORM:
	case DXGI_FORMAT_R16G16_SINT:
	case DXGI_FORMAT_R8G8B8A8_UNORM:
	case DXGI_FORMAT_R8G8B8A8_UINT:
	case DXGI_FORMAT_R8G8B8A8_SNORM:
	case DXGI_FORMAT_R8G8B8A8_SINT:
	case DXGI_FORMAT_B8G8R8A8_UNORM:
	case DXGI_FORMAT_B8G8R8X8_UNORM:
	case DXGI_FORMAT_R10G10B10A2_UNORM:
	case DXGI_FORMAT_R10G10B10A2_UINT:
	case DXGI_FORMAT_R11G11B10_FLOAT:
	case DXGI_FORMAT_R9G9B9E5_SHAREDEXP:
		return 4;

		// 64-bit per channel
	case DXGI_FORMAT_R32G32_FLOAT:
	case DXGI_FORMAT_R32G32_UINT:
	case DXGI_FORMAT_R32G32_SINT:
	case DXGI_FORMAT_R16G16B16A16_FLOAT:
	case DXGI_FORMAT_R16G16B16A16_UNORM:
	case DXGI_FORMAT_R16G16B16A16_UINT:
	case DXGI_FORMAT_R16G16B16A16_SNORM:
	case DXGI_FORMAT_R16G16B16A16_SINT:
	case DXGI_FORMAT_R32G8X24_TYPELESS: // Depth-stencil
		return 8;

		// 128-bit per channel
	case DXGI_FORMAT_R32G32B32_FLOAT:
	case DXGI_FORMAT_R32G32B32_UINT:
	case DXGI_FORMAT_R32G32B32_SINT:
		return 12;

		// 128-bit
	case DXGI_FORMAT_R32G32B32A32_FLOAT:
	case DXGI_FORMAT_R32G32B32A32_UINT:
	case DXGI_FORMAT_R32G32B32A32_SINT:
		return 16;

		// Compressed formats (block-based)
	case DXGI_FORMAT_BC1_UNORM:
	case DXGI_FORMAT_BC1_UNORM_SRGB:
	case DXGI_FORMAT_BC4_UNORM:
	case DXGI_FORMAT_BC4_SNORM:
		return 8; // 8 bytes per 4x4 block

	case DXGI_FORMAT_BC2_UNORM:
	case DXGI_FORMAT_BC2_UNORM_SRGB:
	case DXGI_FORMAT_BC3_UNORM:
	case DXGI_FORMAT_BC3_UNORM_SRGB:
	case DXGI_FORMAT_BC5_UNORM:
	case DXGI_FORMAT_BC5_SNORM:
	case DXGI_FORMAT_BC6H_UF16:
	case DXGI_FORMAT_BC6H_SF16:
	case DXGI_FORMAT_BC7_UNORM:
	case DXGI_FORMAT_BC7_UNORM_SRGB:
		return 16; // 16 bytes per 4x4 block

		// Depth-stencil formats
	case DXGI_FORMAT_D16_UNORM:
		return 2;
	case DXGI_FORMAT_D24_UNORM_S8_UINT:
		return 3; // Но обычно выравнивается до 4 байт
	case DXGI_FORMAT_D32_FLOAT:
		return 4;
	case DXGI_FORMAT_D32_FLOAT_S8X24_UINT:
		return 8;

		// Unknown/typeless - используем fallback
	case DXGI_FORMAT_UNKNOWN:
	default:
		return 4; // По умолчанию предполагаем RGBA8
	}
}

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
		m_desc.Format = (ERHI_FORMAT)desc.Format;
		m_desc.Usage = (ERHI_USAGE)desc.Usage;
		m_desc.BindFlags = (ERHI_BIND_FLAG)desc.BindFlags;
		m_desc.CPUAccessFlags = desc.CPUAccessFlags;
		m_desc.MiscFlags = desc.MiscFlags;
		m_desc.SampleDescCount = desc.SampleDesc.Count;
		m_desc.ArraySize = desc.ArraySize;
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
		m_desc.Format = (ERHI_FORMAT)desc.Format;
		m_desc.Usage = (ERHI_USAGE)desc.Usage;
		m_desc.BindFlags = (ERHI_BIND_FLAG)desc.BindFlags;
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
		m_desc.Format = (ERHI_FORMAT)desc.Format;
		m_desc.Usage = (ERHI_USAGE)desc.Usage;
		m_desc.BindFlags = (ERHI_BIND_FLAG)desc.BindFlags;
		m_desc.CPUAccessFlags = desc.CPUAccessFlags;
		m_desc.MiscFlags = desc.MiscFlags;
		m_desc.ArraySize = desc.ArraySize;
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
	if (Resource)
	{
		u32 Counter = Resource->Release();
		if (Counter == 0)
		{
			Resource = nullptr;
			xr_delete(this);
		}

		return Counter;
	}

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

ERHI_FORMAT DX11Surface::GetFormat() const
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

u32 DX11Surface::GetSampleDescCount() const
{
	return m_desc.SampleDescCount;
}

u32 DX11Surface::GetArraySize() const
{
	return m_desc.ArraySize;
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

bool DX11Surface::UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource)
{
	if (!Resource || !subResource || !subResource->Data)
		return false;

	ID3D11DeviceContext* context = (ID3D11DeviceContext*)GRHI->GetContext();

	D3D11_MAPPED_SUBRESOURCE mappedData;
	HRESULT hr = context->Map(Texture2D, 0, D3D11_MAP_WRITE_DISCARD, 0, &mappedData);
	memcpy(mappedData.pData, subResource->Data, mappedData.DepthPitch);

	context->Unmap(Resource, 0);
	return true;
}

void* DX11Surface::Lock(u32 mipLevel, u32* pitch)
{
	ID3D11DeviceContext* context = (ID3D11DeviceContext*)GRHI->GetContext();

	D3D11_MAPPED_SUBRESOURCE mappedData;
	HRESULT hr = context->Map(Resource, 0, D3D11_MAP_WRITE_DISCARD, 0, &mappedData);
	if (pitch) *pitch = mappedData.RowPitch;
	return mappedData.pData;
}

void DX11Surface::Unlock()
{
	ID3D11DeviceContext* context = (ID3D11DeviceContext*)GRHI->GetContext();
	context->Unmap(Resource, 0);
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
	if (SRV)
	{
		u32 Counter = SRV->Release();
		if (Counter == 0)
		{
			SRV = nullptr;
			xr_delete(this);
		}

		return Counter;
	}

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
	if (RTV)
	{
		RTV->Release();
	}
}

void* DX11RenderTargetView::GetRawRTV()
{
	return RTV;
}

void DX11RenderTargetView::AddRef()
{
	if (RTV)
	{
		RTV->AddRef();
	}
}

u32 DX11RenderTargetView::Release()
{
	if (RTV)
	{
		u32 Counter = RTV->Release();
		if (Counter == 0)
		{
			RTV = nullptr;
			xr_delete(this);
		}

		return Counter;
	}
	
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
		u32 Counter = DSV->Release();
		if (Counter == 0)
		{
			DSV = nullptr;
			xr_delete(this);
		}

		return Counter;
	}
	return 0;
}

ERHI_DSV_DIMENSION DX11DepthStencilView::GetDimension() const
{
	D3D11_DEPTH_STENCIL_VIEW_DESC Desc = {};
	DSV->GetDesc(&Desc);

	return ERHI_DSV_DIMENSION(Desc.ViewDimension);
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

IRHISurface* DX11TextureFactory::CreateTexture2D(const RHITextureDesc& Desc, const RHISubResource* SubResource)
{
	D3D11_TEXTURE2D_DESC d3dDesc = {};
	d3dDesc.Width = Desc.Width;
	d3dDesc.Height = Desc.Height;
	d3dDesc.MipLevels = Desc.MipLevels;
	d3dDesc.ArraySize = 1;
	d3dDesc.Format = DXGI_FORMAT(Desc.Format);
	d3dDesc.SampleDesc.Count = 1;
	d3dDesc.SampleDesc.Quality = 0;
	d3dDesc.Usage = D3D11_USAGE(Desc.Usage);
	d3dDesc.BindFlags = ConvertBindFlags(Desc.BindFlags);
	d3dDesc.CPUAccessFlags = ConvertCPUAccessFlags(Desc.CPUAccessFlags);
	d3dDesc.MiscFlags = ConvertMiscFlags(Desc.MiscFlags);

	ID3D11Texture2D* texture = nullptr;
	D3D11_SUBRESOURCE_DATA DxSubResource = {};
	if (SubResource != nullptr && SubResource->Data != nullptr)
	{
		DxSubResource.pSysMem = SubResource->Data;
		DxSubResource.SysMemPitch = SubResource->DataSize;
	}

	HRESULT hr = Device->CreateTexture2D(&d3dDesc, SubResource ? &DxSubResource : nullptr, &texture);
	if (FAILED(hr))
	{
		return nullptr;
	}

	return new DX11Surface(texture);
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
	
	return CreateTexture2D(desc, nullptr);
}

IRHISurface* DX11TextureFactory::CreateRenderTarget(const RHITextureDesc& desc)
{
	D3D11_TEXTURE2D_DESC d3dDesc = {};
	d3dDesc.Width = desc.Width;
	d3dDesc.Height = desc.Height;
	d3dDesc.MipLevels = desc.MipLevels;
	d3dDesc.ArraySize = desc.ArraySize;
	d3dDesc.Format = DXGI_FORMAT(desc.Format);
	d3dDesc.SampleDesc.Count = desc.SampleDescCount;
	d3dDesc.SampleDesc.Quality = 0;
	d3dDesc.Usage = D3D11_USAGE(desc.Usage);
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
	
	if (desc.ViewDimension == ERHI_RTV_DIMENSION::TEXTURE2D)
	{
		rtvDesc.Texture2D.MipSlice = desc.MipSlice;
	}
	else if (desc.ViewDimension == ERHI_RTV_DIMENSION::TEXTURE2DARRAY)
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
	
	if (desc.ViewDimension == ERHI_DSV_DIMENSION::TEXTURE2D)
	{
		dsvDesc.Texture2D.MipSlice = desc.MipSlice;
	}
	else if (desc.ViewDimension == ERHI_DSV_DIMENSION::TEXTURE2DARRAY)
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

IRHIUnorderedAccessView* DX11TextureFactory::CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc)
{
	D3D11_UNORDERED_ACCESS_VIEW_DESC dxDesc;
	ZeroMemory(&dxDesc, sizeof(dxDesc));

	// Конвертация формата
	dxDesc.Format = DXGI_FORMAT(desc.Format);

	switch (desc.ViewDimension)
	{
	case ERHI_VIEW_DIMENSION::Texture2D:
		dxDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
		dxDesc.Texture2D.MipSlice = desc.MipSlice;
		break;
	case ERHI_VIEW_DIMENSION::Texture3D:
		dxDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE3D;
		dxDesc.Texture3D.MipSlice = desc.MipSlice;
		dxDesc.Texture3D.FirstWSlice = desc.FirstWSlice;
		dxDesc.Texture3D.WSize = desc.WSize;
		break;
	case ERHI_VIEW_DIMENSION::Buffer:
		dxDesc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
		dxDesc.Buffer.FirstElement = desc.FirstElement;
		dxDesc.Buffer.NumElements = desc.NumElements;
		break;
	}

	ID3D11UnorderedAccessView* pUAV = nullptr;
	ID3D11Resource* pResource = static_cast<ID3D11Resource*>(pTexture->GetRawTexture());

	HRESULT hr = Device->CreateUnorderedAccessView(pResource, &dxDesc, &pUAV);
	if (SUCCEEDED(hr))
		return new DX11UnorderedAccessView(pUAV);

	return nullptr;
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

void DX11UnorderedAccessView::AddRef()
{
	UAV->AddRef();
}

u32 DX11UnorderedAccessView::Release()
{
	if (UAV)
	{
		u32 Counter = UAV->Release();
		if (Counter == 0)
		{
			UAV = nullptr;
			xr_delete(this);
		}

		return Counter;
	}

	return 0;
}