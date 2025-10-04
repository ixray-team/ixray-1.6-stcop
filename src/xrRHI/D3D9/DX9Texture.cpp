#include "DX9Texture.h"
#include <d3d9.h>

D3DPOOL ConvertPoolRHIToDX9(const RHITextureDesc& desc)
{
	if ((desc.BindFlags & ERHI_BIND_FLAG::RENDER_TARGET) ||
		(desc.BindFlags & ERHI_BIND_FLAG::DEPTH_STENCIL) ||
		 desc.Usage == ERHI_USAGE::USAGE_DYNAMIC)
	{
		return D3DPOOL_DEFAULT;
	}

	if (desc.CPUAccessFlags & ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_READ)
		return D3DPOOL_SYSTEMMEM;

	return D3DPOOL_MANAGED;
}

DX9Surface::DX9Surface(IDirect3DTexture9* texture)
	: Texture2D(texture), BaseTexture(texture)
{
	if (texture)
	{
		D3DSURFACE_DESC desc;
		texture->GetLevelDesc(0, &desc);
		m_desc = ConvertResource(desc);
	}
}

DX9Surface::DX9Surface(const RHITextureDesc& texture)
{
	//FX: IT'S FAKE for DEPTH!!!
	m_desc = texture;
}

DX9Surface::DX9Surface(IDirect3DVolumeTexture9* texture)
	: Texture3D(texture), BaseTexture(texture)
{
	if (texture)
	{
		D3DVOLUME_DESC desc;
		texture->GetLevelDesc(0, &desc);
		m_desc = ConvertResource(desc);
	}
}

DX9Surface::DX9Surface(IDirect3DCubeTexture9* texture)
	: TextureCube(texture), BaseTexture(texture)
{
	if (texture)
	{
		D3DSURFACE_DESC desc;
		texture->GetLevelDesc(0, &desc);
		m_desc = ConvertResource(desc);
	}
}

DX9Surface::~DX9Surface()
{
	xr_delete(SRV);
	xr_delete(RTV);
	xr_delete(DSV);

	if (BaseTexture)
	{
		BaseTexture->Release();
	}
}

void* DX9Surface::GetRawTexture()
{
	return BaseTexture;
}

void DX9Surface::AddRef()
{
	if (BaseTexture)
	{
		BaseTexture->AddRef();
	}
}

u32 DX9Surface::Release()
{
	if (BaseTexture)
	{
		return BaseTexture->Release();
	}
	return 0;
}

u32 DX9Surface::GetWidth() const
{
	return m_desc.Width;
}

u32 DX9Surface::GetHeight() const
{
	return m_desc.Height;
}

u32 DX9Surface::GetDepth() const
{
	return m_desc.Depth;
}

u32 DX9Surface::GetMipLevels() const
{
	return m_desc.MipLevels;
}

ERHI_FORMAT DX9Surface::GetFormat() const
{
	return m_desc.Format;
}

u32 DX9Surface::GetMiscFlags() const
{
	return m_desc.MiscFlags;
}

ERHI_RESOURCE_DIMENSION DX9Surface::GetTextureType() const
{
	if (Texture2D) return ERHI_RESOURCE_DIMENSION::TEXTURE2D;
	if (Texture3D) return ERHI_RESOURCE_DIMENSION::TEXTURE3D;
	if (TextureCube) return ERHI_RESOURCE_DIMENSION::UNKNOWN;
	return ERHI_RESOURCE_DIMENSION::UNKNOWN;
}

ERHI_USAGE DX9Surface::GetUsage() const
{
	return m_desc.Usage;
}

u32 DX9Surface::GetSampleDescCount() const
{
	return m_desc.SampleDescCount;
}

u32 DX9Surface::GetArraySize() const
{
	return m_desc.ArraySize;
}

IRHIShaderResourceView* DX9Surface::GetShaderResourceView()
{
	if (!SRV && BaseTexture)
	{
		SRV = new DX9ShaderResourceView(this);
	}
	return SRV;
}

IRHIRenderTargetView* DX9Surface::GetRenderTargetView()
{
	return RTV;
}

IRHIDepthStencilView* DX9Surface::GetDepthStencilView()
{
	return DSV;
}

bool DX9Surface::UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource)
{
	if (!Texture2D || !subResource || !subResource->Data || mipLevel > 0 || arrayLayer > 0)
		return false; // DX9 не поддерживает мип-уровни и слои напрямую

	u32 rowPitch = subResource->RowPitch;
	if (rowPitch == 0)
	{
		rowPitch = subResource->Width * GetDX9FormatSize(ConvertRHIFormatToDX9(subResource->TextureFormat));
	}

	D3DLOCKED_RECT lockedRect;
	HRESULT hr = Texture2D->LockRect(0, &lockedRect, nullptr, D3DLOCK_DISCARD);
	if (FAILED(hr))
		return false;

	const u8* srcData = (const u8*)subResource->Data;
	u8* dstData = (u8*)lockedRect.pBits;

	for (u32 y = 0; y < subResource->Height; ++y)
	{
		memcpy(dstData, srcData, rowPitch);
		srcData += rowPitch;
		dstData += lockedRect.Pitch;
	}

	Texture2D->UnlockRect(0);
	return true;
}

void* DX9Surface::Lock(u32 mipLevel, u32* pitch)
{
	if (Texture2D)
	{
		D3DLOCKED_RECT lockedRect;
		HRESULT hr = Texture2D->LockRect(mipLevel, &lockedRect, nullptr, 0);
		if (SUCCEEDED(hr))
		{
			if (pitch) *pitch = lockedRect.Pitch;
			return lockedRect.pBits;
		}
	}
	return nullptr;
}

void DX9Surface::Unlock()
{
	if (Texture2D)
	{
		Texture2D->UnlockRect(0);
	}
}

template <typename T>
RHITextureDesc DX9Surface::ConvertResource(const T& Desc)
{
	RHITextureDesc DescRhi;

	DescRhi.Width = Desc.Width;
	DescRhi.Height = Desc.Height;

	if constexpr (requires { Desc.Depth; })
		DescRhi.Depth = Desc.Depth;
	else
		DescRhi.Depth = 1;

	DescRhi.ArraySize = 1;
	DescRhi.MipLevels = 1;

	if constexpr (requires { Desc.MultiSampleType; })
		DescRhi.SampleDescCount = Desc.MultiSampleType > D3DMULTISAMPLE_NONE ? Desc.MultiSampleType : 1;
	else
		DescRhi.SampleDescCount = 1;

	DescRhi.Format = ConvertDX9FormatToRHI(Desc.Format);

	DescRhi.Usage = ERHI_USAGE::USAGE_DEFAULT;
	DescRhi.BindFlags = ERHI_BIND_FLAG::NOT_SET;
	DescRhi.MiscFlags = 0;
	DescRhi.CPUAccessFlags = 0;

	if (Desc.Usage & D3DUSAGE_RENDERTARGET)
	{
		DescRhi.BindFlags |= ERHI_BIND_FLAG::RENDER_TARGET;
	}
	
	if (Desc.Usage & D3DUSAGE_DEPTHSTENCIL)
	{
		DescRhi.BindFlags |= ERHI_BIND_FLAG::DEPTH_STENCIL;
	}
	if (Desc.Usage & D3DUSAGE_AUTOGENMIPMAP)
	{
		DescRhi.BindFlags |= ERHI_BIND_FLAG::SHADER_RESOURCE;
		DescRhi.MiscFlags |= 1;
	}
	
	if (Desc.Usage & D3DUSAGE_DYNAMIC)
	{
		DescRhi.Usage = ERHI_USAGE::USAGE_DYNAMIC;
		DescRhi.CPUAccessFlags |= 1;
	}

	return DescRhi;
}

// DX9 Shader Resource View implementation
DX9ShaderResourceView::DX9ShaderResourceView(IRHISurface* surface)
	: Surface(surface)
{
}

DX9ShaderResourceView::~DX9ShaderResourceView()
{
}

void* DX9ShaderResourceView::GetRawSRV()
{
	return Surface ? Surface->GetRawTexture() : nullptr;
}

void DX9ShaderResourceView::AddRef()
{
	if (Surface)
	{
		Surface->AddRef();
	}
}

u32 DX9ShaderResourceView::Release()
{
	if (Surface)
	{
		return Surface->Release();
	}
	return 0;
}

IRHISurface* DX9ShaderResourceView::GetSurface()
{
	return Surface;
}

void DX9ShaderResourceView::BindToPixelShader(u32 slot)
{
	if (Surface)
	{
		DX9Surface* dx9Surface = static_cast<DX9Surface*>(Surface);
		if (dx9Surface->GetDX9BaseTexture())
		{
			// Would need device
			// RDevice->SetTexture(slot, dx9Surface->GetDX9BaseTexture());
		}
	}
}

void DX9ShaderResourceView::BindToVertexShader(u32 slot)
{
	if (Surface)
	{
		DX9Surface* dx9Surface = static_cast<DX9Surface*>(Surface);
		if (dx9Surface->GetDX9BaseTexture())
		{
			// Would need device
			// RDevice->SetTexture(D3DVERTEXTEXTURESAMPLER0 + slot, dx9Surface->GetDX9BaseTexture());
		}
	}
}

void DX9ShaderResourceView::BindToGeometryShader(u32 slot)
{
	// DX9 doesn't have geometry shaders
}

void DX9ShaderResourceView::BindToComputeShader(u32 slot)
{
	// DX9 doesn't have compute shaders
}

// DX9 Render Target View implementation
DX9RenderTargetView::DX9RenderTargetView(IDirect3DSurface9* surface, IRHISurface* texture)
	: Surface(surface), Texture(texture)
{
}

DX9RenderTargetView::~DX9RenderTargetView()
{
	if (Surface)
	{
		Surface->Release();
	}
}

void* DX9RenderTargetView::GetRawRTV()
{
	return Surface;
}

void DX9RenderTargetView::AddRef()
{
	if (Surface)
	{
		Surface->AddRef();
	}
}

u32 DX9RenderTargetView::Release()
{
	if (Surface)
	{
		return Surface->Release();
	}
	return 0;
}

IRHISurface* DX9RenderTargetView::GetSurface()
{
	return Texture;
}

void DX9RenderTargetView::BindAsRenderTarget(u32 slot)
{
	if (Surface)
	{
		// Would need device
		// RDevice->SetRenderTarget(slot, Surface);
	}
}

void DX9RenderTargetView::UnbindRenderTarget()
{
	// Would need device
	// RDevice->SetRenderTarget(0, nullptr);
}

// DX9 Depth Stencil View implementation
DX9DepthStencilView::DX9DepthStencilView(IDirect3DSurface9* surface, IRHISurface* texture)
	: Surface(surface), Texture(texture)
{
}

DX9DepthStencilView::~DX9DepthStencilView()
{
	if (Surface)
	{
		Surface->Release();
	}
}

void* DX9DepthStencilView::GetRawDSV()
{
	return Surface;
}

void DX9DepthStencilView::AddRef()
{
	if (Surface)
	{
		Surface->AddRef();
	}
}

u32 DX9DepthStencilView::Release()
{
	if (Surface)
	{
		u32 Counter = Surface->Release();
		if (Counter == 0)
		{
			xr_delete(this);
		}

		return Counter;
	}
	return 0;
}

ERHI_DSV_DIMENSION DX9DepthStencilView::GetDimension() const
{
	return ERHI_DSV_DIMENSION::TEXTURE2D;
}

IRHISurface* DX9DepthStencilView::GetSurface()
{
	return Texture;
}

void DX9DepthStencilView::BindAsDepthStencil()
{
	if (Surface)
	{
		// Would need device
		// RDevice->SetDepthStencilSurface(Surface);
	}
}

void DX9DepthStencilView::UnbindDepthStencil()
{
	// Would need device
	// RDevice->SetDepthStencilSurface(nullptr);
}

// DX9 Texture Factory implementation
DX9TextureFactory::DX9TextureFactory(IDirect3DDevice9* device)
	: Device(device)
{
}

DX9TextureFactory::~DX9TextureFactory()
{
}

IRHISurface* DX9TextureFactory::CreateTexture2D(const RHITextureDesc& Desc, const RHISubResource* SubResource)
{
	auto Usage = ConvertUsage(Desc);
	if (Usage == D3DUSAGE_DEPTHSTENCIL)
	{
		return new DX9Surface(Desc);
	}

	IDirect3DTexture9* texture = nullptr;
	HRESULT hr = Device->CreateTexture
	(
		Desc.Width,
		Desc.Height,
		Desc.MipLevels,
		Usage,
		ConvertRHIFormatToDX9(Desc.Format),
		D3DPOOL_MANAGED,
		&texture,
		nullptr
	);

	if (FAILED(hr))
	{
		return nullptr;
	}

	if (SubResource && SubResource->Data)
	{
		D3DLOCKED_RECT lockedRect;
		hr = texture->LockRect(0, &lockedRect, nullptr, 0);
		if (SUCCEEDED(hr))
		{
			const u8* src = reinterpret_cast<const u8*>(SubResource->Data);
			u8* dst = reinterpret_cast<u8*>(lockedRect.pBits);

			u32 rowPitchSrc = SubResource->RowPitch ? SubResource->RowPitch : Desc.Width * 4;
			u32 rowPitchDst = lockedRect.Pitch;

			for (u32 y = 0; y < Desc.Height; ++y)
			{
				memcpy(dst + y * rowPitchDst, src + y * rowPitchSrc, rowPitchSrc);
			}

			texture->UnlockRect(0);
		}
	}

	return new DX9Surface(texture);
}

IRHISurface* DX9TextureFactory::CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc)
{
	if (data && size == 0)
	{
		IDirect3DResource9* Resource = static_cast<IDirect3DResource9*>(const_cast<void*>(data));

		if (Resource)
		{
			D3DRESOURCETYPE resourceType = Resource->GetType();

			switch (resourceType)
			{
			case D3DRTYPE_TEXTURE:
				return new DX9Surface((IDirect3DTexture9*)Resource);
			case D3DRTYPE_CUBETEXTURE:
				return new DX9Surface((IDirect3DCubeTexture9*)Resource);
			case D3DRTYPE_VOLUMETEXTURE:
				return new DX9Surface((IDirect3DVolumeTexture9*)Resource);
			}

			VERIFY(!"Invalid texture type");
		}
	}

	return CreateTexture2D(desc, nullptr);
}


IRHISurface* DX9TextureFactory::CreateRenderTarget(const RHITextureDesc& desc)
{
	IDirect3DTexture9* texture = nullptr;
	HRESULT hr = Device->CreateTexture
	(
		desc.Width, desc.Height,
		desc.MipLevels,
		ConvertUsage(desc),
		ConvertRHIFormatToDX9(desc.Format),
		ConvertPoolRHIToDX9(desc),
		&texture,
		nullptr
	);
	
	if (FAILED(hr))
		return nullptr;

	return new DX9Surface(texture);
}

IRHISurface* DX9TextureFactory::CreateDepthStencil(const RHITextureDesc& desc)
{
	// Similar to CreateRenderTarget but with depth format
	return nullptr;
}

IRHIShaderResourceView* DX9TextureFactory::CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc)
{
	// For DX9, shader resource view is typically the surface itself
	// The descriptor parameters are not directly applicable to DX9
	return new DX9ShaderResourceView(surface);
}

IRHIRenderTargetView* DX9TextureFactory::CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc)
{
	DX9Surface* dx9Surface = static_cast<DX9Surface*>(surface);
	if (!dx9Surface || !dx9Surface->GetDX9Texture2D())
		return nullptr;

	IDirect3DSurface9* renderTarget = nullptr;
	HRESULT hr = dx9Surface->GetDX9Texture2D()->GetSurfaceLevel(desc.MipSlice, &renderTarget);
	if (FAILED(hr))
		return nullptr;

	return new DX9RenderTargetView(renderTarget, surface);
}

IRHIDepthStencilView* DX9TextureFactory::CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc)
{
	DX9Surface* SurfacePtr = static_cast<DX9Surface*>(surface);
	if (!SurfacePtr)
	{
		return nullptr;
	}

	IDirect3DSurface9* dx9SurfacePtr = nullptr;
	if (SurfacePtr->GetRawTexture())
	{
		// Get the surface from the texture
		IDirect3DTexture9* texture = static_cast<IDirect3DTexture9*>(SurfacePtr->GetRawTexture());
		texture->GetSurfaceLevel(0, &dx9SurfacePtr);
	}

	if (dx9SurfacePtr == nullptr)
	{
		HRESULT hr = Device->CreateDepthStencilSurface
		(
			SurfacePtr->GetWidth(),
			SurfacePtr->GetHeight(),
			ConvertRHIFormatToDX9(SurfacePtr->GetFormat()),
			D3DMULTISAMPLE_NONE,
			0,
			TRUE,
			&dx9SurfacePtr,
			nullptr
		);
	}

	return new DX9DepthStencilView(dx9SurfacePtr, surface);
}

IRHIUnorderedAccessView* DX9TextureFactory::CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc)
{
	return new DX9UnorderedAccessView();
}

u32 DX9TextureFactory::ConvertUsage(const RHITextureDesc& desc)
{
	DWORD Usage = 0;

	if (desc.Usage == ERHI_USAGE::USAGE_DYNAMIC)
		Usage |= D3DUSAGE_DYNAMIC;

	if (desc.BindFlags & ERHI_BIND_FLAG::RENDER_TARGET)
		Usage |= D3DUSAGE_RENDERTARGET;

	if (desc.BindFlags & ERHI_BIND_FLAG::DEPTH_STENCIL)
		Usage |= D3DUSAGE_DEPTHSTENCIL;

	if (desc.MiscFlags & 1) // RHI_MISC_GENERATE_MIPS
		Usage |= D3DUSAGE_AUTOGENMIPMAP;

	if (desc.CPUAccessFlags & 1) // RHI_CPU_ACCESS_WRITE
		Usage |= D3DUSAGE_WRITEONLY;

	return Usage;
}
