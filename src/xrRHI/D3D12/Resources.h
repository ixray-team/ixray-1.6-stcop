#pragma once
#include "../RHI.h"
#include <d3d12.h>
#include <dxgi1_6.h>

class InternalDevice12;

struct DX12Descriptor
{
    D3D12_CPU_DESCRIPTOR_HANDLE Cpu = {};
    D3D12_GPU_DESCRIPTOR_HANDLE Gpu = {};
    D3D12_DESCRIPTOR_HEAP_TYPE Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    u32 Index = UINT32_MAX;
    u64 Generation = 0;
};

struct DX12Resource
{
    ID3D12Resource* Native = nullptr;
    xr_vector<D3D12_RESOURCE_STATES> States;
    D3D12_RESOURCE_STATES UniformState = D3D12_RESOURCE_STATE_COMMON;
    bool Uniform = true;
};

static constexpr u8 DX12KindTyped = 0;
static constexpr u8 DX12KindStructured = 1;
static constexpr u8 DX12KindRaw = 2;

struct DX12Upload
{
    ID3D12Resource* Resource = nullptr;
    u8* Data = nullptr;
    u64 Offset = 0;
    u64 Size = 0;
    u64 Serial = 0;
    u32 Frame = 0;
    bool Owned = false;
};

class DX12Buffer final : public IRHIBuffer
{
public:
    DX12Buffer(InternalDevice12& device, const RHIBufferDesc& desc, const RHIBufferSubresource* data);
    ~DX12Buffer() override;
    void AddRef() override;
    u32 Release() override;
    u32 GetSize() const override;
    bool Map(ERHI_BUFFER_MAP type, u32 flags, RHIMappedSubresource* out_data) override;
    void Unmap() override;
    void UpdateSubresource(void* data, u32 size) override;
    void SetVertexBuffer(u32 slot, u32 stride, u32 offset) override;
    void SetIndexBuffer(bool is32Bit, u32 offset) override;
    D3D12_GPU_VIRTUAL_ADDRESS GetAddress();
    u64 PublishedAddress() const;
    DX12Resource& GetResource() { return _resource; }
    const RHIBufferDesc& GetDesc() const { return _desc; }

private:
    void DiscardStream();
    InternalDevice12& _device;
    RHIBufferDesc _desc;
    DX12Resource _resource;
    DX12Upload _upload;
    u64 _generation = 0;
    u64 _uploaded = 0;
    u64 _published = 0;
    xr_vector<u8> _data;
    std::atomic<u32> _references = 1;
    bool _isMapped = false;
    ERHI_BUFFER_MAP _mapType = ERHI_BUFFER_MAP::WRITE;
};

class DX12Surface final : public IRHISurface
{
public:
    DX12Surface(InternalDevice12& device, const RHITextureDesc& desc, D3D12_RESOURCE_DIMENSION dimension, ID3D12Resource* native, D3D12_RESOURCE_STATES state);
    ~DX12Surface() override;
    void AddRef() override;
    u32 Release() override;
    void* GetRawTexture() override { return _resource.Native; }
    u32 GetWidth() const override { return _desc.Width; }
    u32 GetHeight() const override { return _desc.Height; }
    u32 GetDepth() const override { return _desc.Depth; }
    u32 GetMipLevels() const override { return _desc.MipLevels; }
    u32 GetMiscFlags() const override { return _desc.MiscFlags; }
    u32 GetSampleDescCount() const override { return _desc.SampleDescCount; }
    u32 GetArraySize() const override { return _desc.ArraySize; }
    ERHI_FORMAT GetFormat() const override { return _desc.Format; }
    ERHI_USAGE GetUsage() const override { return _desc.Usage; }
    ERHI_RESOURCE_DIMENSION GetTextureType() const override;
    IRHIShaderResourceView* GetShaderResourceView() override { return nullptr; }
    IRHIRenderTargetView* GetRenderTargetView() override { return nullptr; }
    IRHIDepthStencilView* GetDepthStencilView() override { return nullptr; }
    bool UpdateData(u32 mip, u32 layer, const RHISubResource* data, const RHIBox& box) override;
    void* Lock(u32 mip = 0, u32* out_pitch = nullptr) override;
    void Unlock() override;
    DX12Resource& GetResource() { return _resource; }
    const RHITextureDesc& GetDesc() const { return _desc; }
    D3D12_RESOURCE_DIMENSION GetDimension() const { return _dimension; }

private:
    InternalDevice12& _device;
    RHITextureDesc _desc;
    D3D12_RESOURCE_DIMENSION _dimension;
    DX12Resource _resource;
    std::atomic<u32> _references = 1;
    ID3D12Resource* _readback = nullptr;
    xr_vector<u8> _writeData;
    u32 _writePitch = 0;
    u32 _writeSlicePitch = 0;
    u32 _lockedMip = UINT32_MAX;
    D3D12_PLACED_SUBRESOURCE_FOOTPRINT _footprint = {};
};

struct DX12View
{
    DX12Descriptor Descriptor;
    DX12Surface* Surface = nullptr;
    DX12Buffer* Buffer = nullptr;
    DXGI_FORMAT Format = DXGI_FORMAT_UNKNOWN;
    u32 Mip = 0;
    u32 MipCount = 1;
    u32 Plane = 0;
    u32 FirstSlice = 0;
    u32 SliceCount = 1;
    u32 Flags = 0;
    u8 Planes = 1;
    u8 Kind = DX12KindTyped;
};

class DX12ShaderResourceView final : public IRHIShaderResourceView
{
public:
    DX12ShaderResourceView(InternalDevice12& device, const DX12View& view);
    ~DX12ShaderResourceView() override;
    void* GetRawSRV() override;
    IRHISurface* GetSurface() override { return View.Surface; }
    void AddRef() override;
    u32 Release() override;
    DX12View View;

private:
    InternalDevice12& _device;
    std::atomic<u32> _references = 1;
};

class DX12RenderTargetView final : public IRHIRenderTargetView
{
public:
    DX12RenderTargetView(InternalDevice12& device, const DX12View& view);
    ~DX12RenderTargetView() override;
    void* GetRawRTV() override { return &View; }
    IRHISurface* GetSurface() override { return View.Surface; }
    void AddRef() override;
    u32 Release() override;
    DX12View View;

private:
    InternalDevice12& _device;
    std::atomic<u32> _references = 1;
};

class DX12DepthStencilView final : public IRHIDepthStencilView
{
public:
    DX12DepthStencilView(InternalDevice12& device, const DX12View& view, ERHI_DSV_DIMENSION dimension);
    ~DX12DepthStencilView() override;
    void* GetRawDSV() override { return &View; }
    IRHISurface* GetSurface() override { return View.Surface; }
    void AddRef() override;
    u32 Release() override;
    ERHI_DSV_DIMENSION GetDimension() const override { return _dimension; }
    DX12View View;

private:
    InternalDevice12& _device;
    ERHI_DSV_DIMENSION _dimension;
    std::atomic<u32> _references = 1;
};

class DX12UnorderedAccessView final : public IRHIUnorderedAccessView
{
public:
    DX12UnorderedAccessView(InternalDevice12& device, const DX12View& view);
    ~DX12UnorderedAccessView() override;
    void* GetRaw() override { return &View; }
    void AddRef() override;
    u32 Release() override;
    DX12View View;

private:
    InternalDevice12& _device;
    std::atomic<u32> _references = 1;
};

class DX12TextureFactory final : public IRHITextureFactory
{
public:
    explicit DX12TextureFactory(InternalDevice12& device);
    ~DX12TextureFactory() override = default;
    IRHISurface* CreateTexture2D(const RHITextureDesc& desc, const RHISubResource* data) override;
    IRHISurface* CreateTexture3D(const RHITextureDesc& desc, const RHISubResource* data) override;
    IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) override;
    IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) override;
    IRHISurface* CreateDepthStencil(const RHITextureDesc& desc) override;
    IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) override;
    IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) override;
    IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) override;
    IRHIUnorderedAccessView* CreateUAV(IRHISurface* surface, const RHIUAVDesc& desc) override;

private:
    InternalDevice12& _device;
};
