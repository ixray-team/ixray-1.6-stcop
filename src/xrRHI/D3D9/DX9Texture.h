#pragma once
#include <d3d9.h>

#include "../RHI.h"

class DX9Surface :
    public IRHISurface
{
private:
    IDirect3DTexture9* Texture2D = nullptr;
    IDirect3DVolumeTexture9* Texture3D = nullptr;
    IDirect3DCubeTexture9* TextureCube = nullptr;
    IDirect3DBaseTexture9* BaseTexture = nullptr;
    
    IRHIShaderResourceView* SRV = nullptr;
    IRHIRenderTargetView* RTV = nullptr;
    IRHIDepthStencilView* DSV = nullptr;
    
    RHITextureDesc m_desc;
    
public:
    DX9Surface(IDirect3DTexture9* texture);
    DX9Surface(const RHITextureDesc& texture);
    DX9Surface(IDirect3DVolumeTexture9* texture);
    DX9Surface(IDirect3DCubeTexture9* texture);
    virtual ~DX9Surface();
    
    virtual void* GetRawTexture() override;
    virtual u32 GetWidth() const override;
    virtual u32 GetHeight() const override;
    virtual u32 GetDepth() const override;
    virtual u32 GetMipLevels() const override;
    virtual u32 GetMiscFlags() const override;
    virtual u32 GetTextureType() const override;
    virtual u32 GetSampleDescCount() const override;
    virtual u32 GetArraySize() const override;
    virtual ERHI_FORMAT GetFormat() const override;
    virtual ERHI_USAGE GetUsage() const override;

    virtual void AddRef() override;
    virtual u32 Release() override;
    
    virtual IRHIShaderResourceView* GetShaderResourceView() override;
    virtual IRHIRenderTargetView* GetRenderTargetView() override;
    virtual IRHIDepthStencilView* GetDepthStencilView() override;
    virtual bool UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource) override;
    virtual void* Lock(u32 mipLevel = 0, u32* pitch = nullptr) override;
    virtual void Unlock() override;
    
    IDirect3DTexture9* GetDX9Texture2D() const { return Texture2D; }
    IDirect3DVolumeTexture9* GetDX9Texture3D() const { return Texture3D; }
    IDirect3DCubeTexture9* GetDX9TextureCube() const { return TextureCube; }
    IDirect3DBaseTexture9* GetDX9BaseTexture() const { return BaseTexture; }

private:
    template <typename T>
    RHITextureDesc ConvertResource(const T& Desc);
};

class DX9ShaderResourceView :
    public IRHIShaderResourceView
{
private:
    IRHISurface* Surface = nullptr;
    
public:
    DX9ShaderResourceView(IRHISurface* surface);
    virtual ~DX9ShaderResourceView();
    
    virtual void* GetRawSRV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindToPixelShader(u32 slot) override;
    virtual void BindToVertexShader(u32 slot) override;
    virtual void BindToGeometryShader(u32 slot) override;
    virtual void BindToComputeShader(u32 slot) override;
    virtual void AddRef() override;
    virtual u32 Release() override;
};

class DX9RenderTargetView :
    public IRHIRenderTargetView
{
private:
    IDirect3DSurface9* Surface = nullptr;
    IRHISurface* Texture = nullptr;
    
public:
    DX9RenderTargetView(IDirect3DSurface9* surface, IRHISurface* texture);
    virtual ~DX9RenderTargetView();
    
    virtual void* GetRawRTV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsRenderTarget(u32 slot = 0) override;
    virtual void UnbindRenderTarget() override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    IDirect3DSurface9* GetDX9Surface() const { return Surface; }
};

class DX9DepthStencilView :
    public IRHIDepthStencilView
{
private:
    IDirect3DSurface9* Surface = nullptr;
    IRHISurface* Texture = nullptr;
    
public:
    DX9DepthStencilView(IDirect3DSurface9* surface, IRHISurface* texture);
    virtual ~DX9DepthStencilView();
    
    virtual void* GetRawDSV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsDepthStencil() override;
    virtual void UnbindDepthStencil() override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    IDirect3DSurface9* GetDX9Surface() const { return Surface; }
    virtual ERHI_DSV_DIMENSION GetDimension() const override;
};

class DX9UnorderedAccessView :
    public IRHIUnorderedAccessView
{
public:
    DX9UnorderedAccessView() {}
    void* GetRaw() override { return nullptr; }
    virtual void AddRef() override {};
    virtual u32 Release() override { return 0; };
};

class DX9TextureFactory :
    public IRHITextureFactory
{
private:
    IDirect3DDevice9* Device = nullptr;
    
public:
    DX9TextureFactory(IDirect3DDevice9* device);
    virtual ~DX9TextureFactory();

    virtual IRHISurface* CreateTexture2D(const RHITextureDesc& Desc, const RHISubResource* SubResource) override;
    virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateDepthStencil(const RHITextureDesc& desc) override;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) override;
    virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) override;
    virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) override;
    virtual IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc) override;

private:
    u32 ConvertUsage(const RHITextureDesc& desc);
};

D3DFORMAT ConvertRHIFormatToDX9(ERHI_FORMAT rhiFormat);
ERHI_FORMAT ConvertDX9FormatToRHI(D3DFORMAT dx9Format);
u32 GetDX9FormatSize(D3DFORMAT dx9Format);