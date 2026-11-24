/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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

    RHITextureDesc m_desc;
    // An adopted texture is shared with NRI: the surface counts its own references and holds one of the texture
    std::atomic<u32> Adopted = 0;

public:
    DX11Surface(ID3D11Texture2D* texture);
    DX11Surface(ID3D11Texture3D* texture);
    DX11Surface(ID3D11Texture1D* texture);
    // RHITextureDesc::Adopt
    static DX11Surface* Adopt(const RHITextureDesc& desc);
    virtual ~DX11Surface();
    
    virtual void* GetRawTexture() override;
    virtual u32 GetWidth() const override;
    virtual u32 GetHeight() const override;
    virtual u32 GetDepth() const override;
    virtual u32 GetMipLevels() const override;
    virtual u32 GetMiscFlags() const override;
    virtual ERHI_RESOURCE_DIMENSION GetTextureType() const override;
    virtual u32 GetSampleDescCount() const override;
    virtual u32 GetArraySize() const override;
    virtual ERHI_FORMAT GetFormat() const override;
    virtual ERHI_USAGE GetUsage() const override;
    
    virtual bool UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource, const RHIBox& Box) override;
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
    
public:
    DX11ShaderResourceView(ID3D11ShaderResourceView* srv);
    virtual ~DX11ShaderResourceView();
    
    virtual void* GetRawSRV() override;

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
    virtual IRHISurface* CreateTexture3D(const RHITextureDesc& Desc, const RHISubResource* SubResource) override;
    virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) override;
    virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) override;
    virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) override;
    virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) override;
    virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) override;
    virtual IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc) override;
    IRHIUnorderedAccessView* CreateUAV(ID3D11Resource* pResource, const RHIUAVDesc& desc);
    
private:
    u32 ConvertBindFlags(u32 bindFlags);
    u32 ConvertCPUAccessFlags(u32 cpuAccessFlags);
    u32 ConvertMiscFlags(u32 miscFlags);
};
