#pragma once
#include <d3d11.h>
#include "../RHI.h"

class DX11Surface :
    public IRHISurface
{
private:
    ID3D11Texture2D* Texture2D = nullptr;
    ID3D11Texture3D* Texture3D = nullptr;
    ID3D11Texture1D* Texture1D = nullptr;
    ID3D11Resource* Resource = nullptr;
    
    IRHIShaderResourceView* SRV = nullptr;
    IRHIRenderTargetView* RTV = nullptr;
    IRHIDepthStencilView* DSV = nullptr;
    
    RHITextureDesc m_desc;
    
public:
    DX11Surface(ID3D11Texture2D* texture);
    DX11Surface(ID3D11Texture3D* texture);
    DX11Surface(ID3D11Texture1D* texture);
    virtual ~DX11Surface();
    
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
    
    virtual IRHIShaderResourceView* GetShaderResourceView() override;
    virtual IRHIRenderTargetView* GetRenderTargetView() override;
    virtual IRHIDepthStencilView* GetDepthStencilView() override;
    virtual bool UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource) override;
    virtual void* Lock(u32 mipLevel = 0, u32* pitch = nullptr) override;
    virtual void Unlock() override;
    virtual void AddRef() override;
    virtual u32 Release() override;
    
    ID3D11Texture2D* GetDX11Texture2D() const { return Texture2D; }
    ID3D11Texture3D* GetDX11Texture3D() const { return Texture3D; }
    ID3D11Texture1D* GetDX11Texture1D() const { return Texture1D; }
    ID3D11Resource* GetDX11Resource() const { return Resource; }
};

class DX11ShaderResourceView :
    public IRHIShaderResourceView
{
private:
    ID3D11ShaderResourceView* SRV = nullptr;
    IRHISurface* Surface = nullptr;
    
public:
    DX11ShaderResourceView(ID3D11ShaderResourceView* srv, IRHISurface* surface);
    virtual ~DX11ShaderResourceView();
    
    virtual void* GetRawSRV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindToPixelShader(u32 slot) override;
    virtual void BindToVertexShader(u32 slot) override;
    virtual void BindToGeometryShader(u32 slot) override;
    virtual void BindToComputeShader(u32 slot) override;

    virtual void AddRef() override;
    virtual u32 Release() override;

    ID3D11ShaderResourceView* GetDX11SRV() const { return SRV; }
};

class DX11RenderTargetView :
    public IRHIRenderTargetView
{
private:
    ID3D11RenderTargetView* RTV = nullptr;
    IRHISurface* Surface = nullptr;
    
public:
    DX11RenderTargetView(ID3D11RenderTargetView* rtv, IRHISurface* surface);
    virtual ~DX11RenderTargetView();
    
    virtual void* GetRawRTV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsRenderTarget(u32 slot = 0) override;
    virtual void UnbindRenderTarget() override;
    virtual void AddRef() override;
    virtual u32 Release() override;

    ID3D11RenderTargetView* GetDX11RTV() const { return RTV; }
};

class DX11DepthStencilView :
    public IRHIDepthStencilView
{
private:
    ID3D11DepthStencilView* DSV = nullptr;
    IRHISurface* Surface = nullptr;
    
public:
    DX11DepthStencilView(ID3D11DepthStencilView* dsv, IRHISurface* surface);
    virtual ~DX11DepthStencilView();
    
    virtual void* GetRawDSV() override;
    virtual IRHISurface* GetSurface() override;
    virtual void BindAsDepthStencil() override;
    virtual void UnbindDepthStencil() override;

    virtual void AddRef() override;
    virtual u32 Release() override;

    ID3D11DepthStencilView* GetDX11DSV() const { return DSV; }
    virtual ERHI_DSV_DIMENSION GetDimension() const override;
};

class DX11UnorderedAccessView :
    public IRHIUnorderedAccessView
{
public:
    DX11UnorderedAccessView(ID3D11UnorderedAccessView* pUAV) : UAV(pUAV) {}
    ~DX11UnorderedAccessView() { if (UAV) UAV->Release(); }

    void* GetRaw() override { return UAV; }
    virtual void AddRef() override;
    virtual u32 Release() override;

private:
    ID3D11UnorderedAccessView* UAV = nullptr;
};

class DX11TextureFactory final :
    public IRHITextureFactory
{
private:
    ID3D11Device* Device = nullptr;
    ID3D11DeviceContext* Context = nullptr;
    
public:
    DX11TextureFactory(ID3D11Device* device, ID3D11DeviceContext* context);
    virtual ~DX11TextureFactory();

    virtual IRHISurface* CreateTexture2D(const RHITextureDesc& Desc, const RHISubResource* SubResource) override;
    virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateDepthStencil(const RHITextureDesc& desc) override;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) override;
    virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) override;
    virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) override;
    virtual IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc) override;
    
private:
    u32 ConvertBindFlags(u32 bindFlags);
    u32 ConvertCPUAccessFlags(u32 cpuAccessFlags);
    u32 ConvertMiscFlags(u32 miscFlags);
};
