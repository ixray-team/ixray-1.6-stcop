#include "DX9Texture.h"
#include <d3d9.h>

// DX9 Surface implementation
DX9Surface::DX9Surface(IDirect3DTexture9* texture)
    : Texture2D(texture), BaseTexture(texture)
{
    if (texture)
    {
        D3DSURFACE_DESC desc;
        texture->GetLevelDesc(0, &desc);
        m_desc.Width = desc.Width;
        m_desc.Height = desc.Height;
        m_desc.Depth = 1;
        m_desc.MipLevels = texture->GetLevelCount();
        m_desc.Format = desc.Format;
        m_desc.Usage = desc.Usage;
        m_desc.BindFlags = 0; // DX9 doesn't have explicit bind flags
        m_desc.CPUAccessFlags = 0;
        m_desc.MiscFlags = 0;
    }
}

DX9Surface::DX9Surface(IDirect3DVolumeTexture9* texture)
    : Texture3D(texture), BaseTexture(texture)
{
    if (texture)
    {
        D3DVOLUME_DESC desc;
        texture->GetLevelDesc(0, &desc);
        m_desc.Width = desc.Width;
        m_desc.Height = desc.Height;
        m_desc.Depth = desc.Depth;
        m_desc.MipLevels = texture->GetLevelCount();
        m_desc.Format = desc.Format;
        m_desc.Usage = desc.Usage;
        m_desc.BindFlags = 0;
        m_desc.CPUAccessFlags = 0;
        m_desc.MiscFlags = 0;
    }
}

DX9Surface::DX9Surface(IDirect3DCubeTexture9* texture)
    : TextureCube(texture), BaseTexture(texture)
{
    if (texture)
    {
        D3DSURFACE_DESC desc;
        texture->GetLevelDesc(0, &desc);
        m_desc.Width = desc.Width;
        m_desc.Height = desc.Height;
        m_desc.Depth = 6; // Cube has 6 faces
        m_desc.MipLevels = texture->GetLevelCount();
        m_desc.Format = desc.Format;
        m_desc.Usage = desc.Usage;
        m_desc.BindFlags = 0;
        m_desc.CPUAccessFlags = 0;
        m_desc.MiscFlags = 0;
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
    if (BaseTexture) BaseTexture->AddRef();
}

u32 DX9Surface::Release()
{
    if (BaseTexture) return BaseTexture->Release();
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

u32 DX9Surface::GetFormat() const
{
    return m_desc.Format;
}

u32 DX9Surface::GetMiscFlags() const
{
    return m_desc.MiscFlags;
}

u32 DX9Surface::GetTextureType() const
{
    if (Texture2D) return D3DRTYPE_TEXTURE;
    if (Texture3D) return D3DRTYPE_VOLUMETEXTURE;
    if (TextureCube) return D3DRTYPE_CUBETEXTURE;
    return D3DRTYPE_SURFACE;
}

ERHI_USAGE DX9Surface::GetUsage() const
{
    if (m_desc.Usage == D3DUSAGE_DYNAMIC)
    {
        return ERHI_USAGE::USAGE_DYNAMIC;
    }

    return ERHI_USAGE::USAGE_DEFAULT;
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

bool DX9Surface::UpdateData(const void* data, u32 size)
{
    // Implementation would depend on usage flags
    return false;
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
    if (Surface) Surface->AddRef();
}

u32 DX9ShaderResourceView::Release()
{
    if (Surface) return Surface->Release();
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
    if (Surface) Surface->Release();
}

void* DX9RenderTargetView::GetRawRTV()
{
    return Surface;
}

void DX9RenderTargetView::AddRef()
{
    if (Surface) Surface->AddRef();
}

u32 DX9RenderTargetView::Release()
{
    if (Surface) return Surface->Release();
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

void DX9RenderTargetView::Clear(float r, float g, float b, float a)
{
    if (Surface)
    {
        D3DCOLOR color = D3DCOLOR_ARGB
        (
            static_cast<BYTE>(a * 255),
            static_cast<BYTE>(r * 255),
            static_cast<BYTE>(g * 255),
            static_cast<BYTE>(b * 255)
        );
        // Would need device
        // RDevice->ColorFill(Surface, nullptr, color);
    }
}

// DX9 Depth Stencil View implementation
DX9DepthStencilView::DX9DepthStencilView(IDirect3DSurface9* surface, IRHISurface* texture)
    : Surface(surface), Texture(texture)
{
}

DX9DepthStencilView::~DX9DepthStencilView()
{
    if (Surface) Surface->Release();
}

void* DX9DepthStencilView::GetRawDSV()
{
    return Surface;
}

void DX9DepthStencilView::AddRef()
{
    if (Surface) Surface->AddRef();
}

u32 DX9DepthStencilView::Release()
{
    if (Surface) return Surface->Release();
    return 0;
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

IRHISurface* DX9TextureFactory::CreateTextureFromFile(const char* filename, u32& memorySize)
{
    // Implementation would use D3DXCreateTextureFromFile or similar
    return nullptr;
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

    // Для DX9 создаем обычную 2D текстуру
    IDirect3DTexture9* texture = nullptr;
    HRESULT hr = Device->CreateTexture
    (
        desc.Width,
        desc.Height,
        desc.MipLevels,
        ConvertUsage(desc.Usage),
        ConvertFormat(desc.Format),
        D3DPOOL_DEFAULT,
        &texture,
        nullptr
    );

    if (FAILED(hr))
        return nullptr;

    return new DX9Surface(texture);
}

IRHISurface* DX9TextureFactory::CreateRenderTarget(const RHITextureDesc& desc)
{
    IDirect3DTexture9* texture = nullptr;
    HRESULT hr = Device->CreateTexture(
        desc.Width, desc.Height,
        desc.MipLevels,
        ConvertUsage(desc.Usage),
        ConvertFormat(desc.Format),
        ConvertPool(desc.Usage),
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
    // DX9 doesn't have separate depth stencil views like DX11
    // In DX9, depth stencil is typically handled as part of the surface itself
    // We can create a wrapper that provides the same interface
    DX9Surface* dx9Surface = static_cast<DX9Surface*>(surface);
    if (!dx9Surface)
        return nullptr;

    // For DX9, we create a depth stencil view that wraps the surface
    // The actual depth stencil functionality is handled by the surface
    IDirect3DSurface9* dx9SurfacePtr = nullptr;
    if (dx9Surface->GetRawTexture()) {
        // Get the surface from the texture
        IDirect3DTexture9* texture = static_cast<IDirect3DTexture9*>(dx9Surface->GetRawTexture());
        texture->GetSurfaceLevel(0, &dx9SurfacePtr);
    }
    return new DX9DepthStencilView(dx9SurfacePtr, surface);
}

// Helper methods
D3DFORMAT DX9TextureFactory::ConvertFormat(u32 format)
{
    return static_cast<D3DFORMAT>(format);
}

D3DPOOL DX9TextureFactory::ConvertPool(u32 usage)
{
    // Convert usage flags to D3DPOOL
    if (usage & D3DUSAGE_DYNAMIC)
        return D3DPOOL_DEFAULT;
    return D3DPOOL_DEFAULT;
}

DWORD DX9TextureFactory::ConvertUsage(u32 usage)
{
    return static_cast<DWORD>(usage);
}
