#pragma once
#include "../RHI.h"
#include <d3d9.h>

class DX9Surface :
    public IRHISurface
{
private:
    IDirect3DTexture9* m_pTexture2D = nullptr;
    IDirect3DVolumeTexture9* m_pTexture3D = nullptr;
    IDirect3DCubeTexture9* m_pTextureCube = nullptr;
    IDirect3DBaseTexture9* m_pBaseTexture = nullptr;
    
    IRHIShaderResourceView* m_pSRV = nullptr;
    IRHIRenderTargetView* m_pRTV = nullptr;
    IRHIDepthStencilView* m_pDSV = nullptr;
    
    RHITextureDesc m_desc;
    
public:
    DX9Surface(IDirect3DTexture9* texture);
    DX9Surface(IDirect3DVolumeTexture9* texture);
    DX9Surface(IDirect3DCubeTexture9* texture);
    virtual ~DX9Surface();
    
    virtual void* GetRawTexture() override;
    virtual u32 GetWidth() const override;
    virtual u32 GetHeight() const override;
    virtual u32 GetDepth() const override;
    virtual u32 GetMipLevels() const override;
    virtual u32 GetFormat() const override;
    virtual u32 GetTextureType() const override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    virtual IRHIShaderResourceView* GetShaderResourceView() override;
    virtual IRHIRenderTargetView* GetRenderTargetView() override;
    virtual IRHIDepthStencilView* GetDepthStencilView() override;
    virtual bool UpdateData(const void* data, u32 size) override;
    virtual void* Lock(u32 mipLevel = 0, u32* pitch = nullptr) override;
    virtual void Unlock() override;
    
    IDirect3DTexture9* GetDX9Texture2D() const { return m_pTexture2D; }
    IDirect3DVolumeTexture9* GetDX9Texture3D() const { return m_pTexture3D; }
    IDirect3DCubeTexture9* GetDX9TextureCube() const { return m_pTextureCube; }
    IDirect3DBaseTexture9* GetDX9BaseTexture() const { return m_pBaseTexture; }
};

class DX9ShaderResourceView :
    public IRHIShaderResourceView
{
private:
    IRHISurface* m_pSurface = nullptr;
    
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
    IDirect3DSurface9* m_pSurface = nullptr;
    IRHISurface* m_pTexture = nullptr;
    
public:
    DX9RenderTargetView(IDirect3DSurface9* surface, IRHISurface* texture);
    virtual ~DX9RenderTargetView();
    
    virtual void* GetRawRTV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsRenderTarget(u32 slot = 0) override;
    virtual void UnbindRenderTarget() override;
    virtual void Clear(float r, float g, float b, float a) override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    IDirect3DSurface9* GetDX9Surface() const { return m_pSurface; }
};

class DX9DepthStencilView :
    public IRHIDepthStencilView
{
private:
    IDirect3DSurface9* m_pSurface = nullptr;
    IRHISurface* m_pTexture = nullptr;
    
public:
    DX9DepthStencilView(IDirect3DSurface9* surface, IRHISurface* texture);
    virtual ~DX9DepthStencilView();
    
    virtual void* GetRawDSV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsDepthStencil() override;
    virtual void UnbindDepthStencil() override;
    virtual void ClearDepth(float depth = 1.0f) override;
    virtual void ClearStencil(u8 stencil = 0) override;
    virtual void ClearDepthStencil(float depth = 1.0f, u8 stencil = 0) override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    IDirect3DSurface9* GetDX9Surface() const { return m_pSurface; }
};

class DX9TextureFactory :
    public IRHITextureFactory
{
private:
    IDirect3DDevice9* m_pDevice = nullptr;
    
public:
    DX9TextureFactory(IDirect3DDevice9* device);
    virtual ~DX9TextureFactory();
    
    virtual IRHISurface* CreateTextureFromFile(const char* filename, u32& memorySize) override;
    virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateDepthStencil(const RHITextureDesc& desc) override;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) override;
    virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) override;
    virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) override;
    
private:
    D3DFORMAT ConvertFormat(u32 format);
    D3DPOOL ConvertPool(u32 usage);
    DWORD ConvertUsage(u32 usage);
};






