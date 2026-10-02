#include "Device.h"
#include "GPUEvents.h"
#include <DirectXTex.h>

static DXGI_FORMAT ViewFormat(DXGI_FORMAT format, bool depth)
{
    switch (format)
    {
    case DXGI_FORMAT_R32_TYPELESS: return depth ? DXGI_FORMAT_D32_FLOAT : DXGI_FORMAT_R32_FLOAT;
    case DXGI_FORMAT_R24G8_TYPELESS: return depth ? DXGI_FORMAT_D24_UNORM_S8_UINT : DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
    case DXGI_FORMAT_R32G8X24_TYPELESS: return depth ? DXGI_FORMAT_D32_FLOAT_S8X24_UINT : DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS;
    case DXGI_FORMAT_R16_TYPELESS: return depth ? DXGI_FORMAT_D16_UNORM : DXGI_FORMAT_R16_UNORM;
    default: return format;
    }
}

DX12TextureFactory::DX12TextureFactory(InternalDevice12& device) : _device(device)
{
}

DX12Surface* InternalDevice12::CreateTexture(const RHITextureDesc& requested, D3D12_RESOURCE_DIMENSION dimension,
    const RHISubResource* data, u32 count)
{
    PROF_EVENT("D3D12: CreateTexture");
    RHITextureDesc desc = requested;
    if (!desc.Width || !desc.Height || !desc.Depth || !desc.ArraySize || !desc.SampleDescCount)
    {
        return nullptr;
    }
    if (!desc.MipLevels)
    {
        u32 extent = std::max(desc.Width, std::max(desc.Height, dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? desc.Depth : 1));
        desc.MipLevels = 1;
        while (extent > 1)
        {
            extent >>= 1;
            ++desc.MipLevels;
        }
    }
    if (desc.MipLevels > UINT16_MAX || desc.ArraySize > UINT16_MAX || desc.Depth > UINT16_MAX)
    {
        return nullptr;
    }
    D3D12_RESOURCE_DESC native = {};
    native.Dimension = dimension;
    native.Width = desc.Width;
    native.Height = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE1D ? 1 : desc.Height;
    native.DepthOrArraySize = (UINT16)(dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? desc.Depth : desc.ArraySize);
    native.MipLevels = (UINT16)desc.MipLevels;
    native.Format = (DXGI_FORMAT)desc.Format;
    native.SampleDesc.Count = desc.SampleDescCount;
    if (desc.BindFlags & ERHI_BIND_FLAG::RENDER_TARGET || desc.MiscFlags & 1)
    {
        native.Flags |= D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET;
    }
    if (desc.BindFlags & ERHI_BIND_FLAG::DEPTH_STENCIL)
    {
        native.Flags |= D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;
    }
    if (desc.BindFlags & ERHI_BIND_FLAG::UNORDERED_ACCESS)
    {
        native.Flags |= D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS;
    }
    D3D12_HEAP_PROPERTIES properties = {};
    properties.Type = D3D12_HEAP_TYPE_DEFAULT;
    D3D12_CLEAR_VALUE clear = {};
    const bool isDepth = (native.Flags & D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL) != 0;
    const bool isTarget = (native.Flags & D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET) != 0;
    clear.Format = ViewFormat(native.Format, isDepth);
    bool hasClear = (isDepth || isTarget) && !DirectX::IsTypeless(clear.Format);
    if (isDepth)
    {
        clear.DepthStencil.Depth = 1;
    }
    else if (isTarget && desc.OptimizedColorClear)
    {
        memcpy(clear.Color, desc.ClearColor, sizeof(clear.Color));
    }
    else if (isTarget)
    {
        hasClear = false;
    }
    ID3D12Resource* resource = nullptr;
    HRESULT result;
    {
        ContextLock guard(*this);
        result = GetDevice()->CreateCommittedResource(&properties, D3D12_HEAP_FLAG_NONE, &native,
            D3D12_RESOURCE_STATE_COMMON, hasClear ? &clear : nullptr, IID_PPV_ARGS(&resource));
    }
    if (FAILED(result))
    {
        Msg("! D3D12 texture creation failed: 0x%08x", result);
        return nullptr;
    }
    _counters.Allocations.fetch_add(1, std::memory_order_relaxed);
    desc.Height = native.Height;
    DX12Surface* surface = new DX12Surface(*this, desc, dimension, resource, D3D12_RESOURCE_STATE_COMMON);
    for (u32 subresource_idx = 0; subresource_idx < count; ++subresource_idx)
    {
        if (data[subresource_idx].Data && !UploadTexture(*surface, subresource_idx, data[subresource_idx]))
        {
            surface->Release();
            return nullptr;
        }
    }
    return surface;
}

bool InternalDevice12::UploadTexture(DX12Surface& surface, u32 subresource, const RHISubResource& data, const RHIBox* box)
{
    PROF_EVENT("D3D12: UploadTexture");
    auto native = surface.GetResource().Native->GetDesc();
    if (!data.Data || subresource >= surface.GetResource().States.size() || native.SampleDesc.Count != 1)
    {
        return false;
    }
    const u32 mip = subresource % native.MipLevels;
    const u32 width = std::max(1u, (u32)native.Width >> mip);
    const u32 height = std::max(1u, native.Height >> mip);
    const u32 depth = native.Dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? std::max(1u, (u32)native.DepthOrArraySize >> mip) : 1;
    if (box && (box->left >= box->right || box->top >= box->bottom || box->front >= box->back ||
        box->right > width || box->bottom > height || box->back > depth))
    {
        return false;
    }
    if (box)
    {
        native.Width = box->right - box->left;
        native.Height = box->bottom - box->top;
        native.DepthOrArraySize = (UINT16)(native.Dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? box->back - box->front : 1);
        native.MipLevels = 1;
        if (DirectX::IsCompressed(native.Format))
        {
            if (box->left % 4 || box->top % 4 ||
                (box->right % 4 && box->right != width) || (box->bottom % 4 && box->bottom != height))
            {
                return false;
            }
            native.Width = (native.Width + 3) & ~u64(3);
            native.Height = (native.Height + 3) & ~3u;
        }
    }
    D3D12_PLACED_SUBRESOURCE_FOOTPRINT footprint = {};
    u32 rows = 0;
    u64 rowSize = 0;
    u64 totalSize = 0;
    GetDevice()->GetCopyableFootprints(&native, box ? 0 : subresource, 1, 0, &footprint, &rows, &rowSize, &totalSize);
    if (!rows || !rowSize || !totalSize || totalSize == UINT64_MAX)
    {
        return false;
    }
    const u64 rowPitch = data.RowPitch ? data.RowPitch : data.DataSize ? data.DataSize : rowSize;
    const u64 slicePitch = data.DepthPitch ? data.DepthPitch : rowPitch * rows;
    if (rowPitch < rowSize || slicePitch < rowPitch * rows)
    {
        return false;
    }
    DX12Upload upload = AllocateUpload(totalSize, D3D12_TEXTURE_DATA_PLACEMENT_ALIGNMENT);
    for (u32 slice_idx = 0; slice_idx < footprint.Footprint.Depth; ++slice_idx)
    {
        for (u32 row_idx = 0; row_idx < rows; ++row_idx)
        {
            memcpy(upload.Data + footprint.Offset + u64(slice_idx) * footprint.Footprint.RowPitch * rows +
                u64(row_idx) * footprint.Footprint.RowPitch,
                (const u8*)data.Data + u64(slice_idx) * slicePitch + u64(row_idx) * rowPitch, (size_t)rowSize);
        }
    }
    _counters.CopiedBytes.fetch_add(totalSize, std::memory_order_relaxed);
    PendingCopy copy;
    copy.Surface = &surface;
    copy.Subresource = subresource;
    copy.Source = upload;
    copy.Footprint = footprint;
    if (box)
    {
        copy.HasBox = true;
        copy.DestX = box->left;
        copy.DestY = box->top;
        copy.DestZ = box->front;
        copy.Box.right = box->right - box->left;
        copy.Box.bottom = box->bottom - box->top;
        copy.Box.back = box->back - box->front;
    }
    EnqueueCopy(std::move(copy));
    return true;
}

bool InternalDevice12::ReadTexture(DX12Surface& surface, u32 subresource, ID3D12Resource** out_readback,
    D3D12_PLACED_SUBRESOURCE_FOOTPRINT& out_footprint)
{
    PROF_EVENT("D3D12: ReadTexture");
    ContextLock guard(*this);
    *out_readback = nullptr;
    auto native = surface.GetResource().Native->GetDesc();
    if (subresource >= surface.GetResource().States.size())
    {
        return false;
    }
    DX12Resource resolved;
    if (native.SampleDesc.Count > 1)
    {
        D3D12_RESOURCE_DESC singleSample = native;
        singleSample.SampleDesc.Count = 1;
        singleSample.SampleDesc.Quality = 0;
        D3D12_HEAP_PROPERTIES heap = {};
        heap.Type = D3D12_HEAP_TYPE_DEFAULT;
        if (FAILED(GetDevice()->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &singleSample,
            D3D12_RESOURCE_STATE_RESOLVE_DEST, nullptr, IID_PPV_ARGS(&resolved.Native))))
        {
            return false;
        }
        resolved.States.assign(surface.GetResource().States.size(), D3D12_RESOURCE_STATE_RESOLVE_DEST);
        Transition(surface.GetResource(), D3D12_RESOURCE_STATE_RESOLVE_SOURCE, subresource);
        Commands()->ResolveSubresource(resolved.Native, subresource, surface.GetResource().Native, subresource, native.Format);
        Transition(resolved, D3D12_RESOURCE_STATE_COPY_SOURCE, subresource);
        native = singleSample;
    }
    u64 totalSize = 0;
    GetDevice()->GetCopyableFootprints(&native, subresource, 1, 0, &out_footprint, nullptr, nullptr, &totalSize);
    ID3D12Resource* readback = CreateNativeBuffer(totalSize, D3D12_HEAP_TYPE_READBACK);
    D3D12_TEXTURE_COPY_LOCATION destination = {};
    destination.pResource = readback;
    destination.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
    destination.PlacedFootprint = out_footprint;
    D3D12_TEXTURE_COPY_LOCATION source = {};
    source.pResource = resolved.Native ? resolved.Native : surface.GetResource().Native;
    source.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
    source.SubresourceIndex = subresource;
    if (!resolved.Native)
    {
        Transition(surface.GetResource(), D3D12_RESOURCE_STATE_COPY_SOURCE, subresource);
    }
    Commands()->CopyTextureRegion(&destination, 0, 0, 0, &source, nullptr);
    Retire(resolved.Native);
    Flush();
    *out_readback = readback;
    return true;
}

bool InternalDevice12::ReadRenderTargetPixels(IRHIRenderTargetView* target, void* data, u32 size, u32& out_width,
    u32& out_height, u32& out_pitch)
{
    ContextLock guard(*this);
    if (!target)
    {
        return false;
    }
    const auto& view = static_cast<DX12RenderTargetView*>(target)->View;
    out_width = std::max(1u, view.Surface->GetWidth() >> view.Mip);
    out_height = std::max(1u, view.Surface->GetHeight() >> view.Mip);
    const auto desc = view.Surface->GetResource().Native->GetDesc();
    D3D12_PLACED_SUBRESOURCE_FOOTPRINT footprint = {};
    GetDevice()->GetCopyableFootprints(&desc, view.Mip, 1, 0, &footprint, nullptr, nullptr, nullptr);
    out_pitch = footprint.Footprint.RowPitch;
    if (!data || u64(out_pitch) * out_height > size)
    {
        return false;
    }
    ID3D12Resource* readback = nullptr;
    if (!ReadTexture(*view.Surface, view.Mip, &readback, footprint))
    {
        return false;
    }
    void* mapped = nullptr;
    D3D12_RANGE range = { (SIZE_T)footprint.Offset, (SIZE_T)(footprint.Offset + u64(out_pitch) * out_height) };
    R_CHK(readback->Map(0, &range, &mapped));
    memcpy(data, (u8*)mapped + footprint.Offset, size_t(out_pitch) * out_height);
    D3D12_RANGE written = {};
    readback->Unmap(0, &written);
    readback->Release();
    return true;
}

IRHISurface* DX12TextureFactory::CreateTexture2D(const RHITextureDesc& desc, const RHISubResource* data)
{
    return _device.CreateTexture(desc, D3D12_RESOURCE_DIMENSION_TEXTURE2D, data, data && data->Data ? 1 : 0);
}

IRHISurface* DX12TextureFactory::CreateTexture3D(const RHITextureDesc& desc, const RHISubResource* data)
{
    return _device.CreateTexture(desc, D3D12_RESOURCE_DIMENSION_TEXTURE3D, data, data && data->Data ? 1 : 0);
}

IRHISurface* DX12TextureFactory::CreateRenderTarget(const RHITextureDesc& desc)
{
    return CreateTexture2D(desc, nullptr);
}

IRHISurface* DX12TextureFactory::CreateDepthStencil(const RHITextureDesc& desc)
{
    return CreateTexture2D(desc, nullptr);
}

IRHISurface* DX12TextureFactory::CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& requested)
{
    if (!data)
    {
        return CreateTexture2D(requested, nullptr);
    }
    if (size)
    {
        int lod = 0;
        IRHISurface* surface = nullptr;
        if (FAILED(_device.LoadDDS(data, size, requested.Usage, requested.BindFlags,
            (ERHI_CPU_ACCESS_FLAG)requested.CPUAccessFlags, lod, false, &surface)))
        {
            return nullptr;
        }
        return surface;
    }
    InternalDevice12::ContextLock guard(_device);
    {
        auto native = (ID3D12Resource*)data;
        if (auto surface = _device.FindSurface(native))
        {
            surface->AddRef();
            return surface;
        }
        const auto nativeDesc = native->GetDesc();
        RHITextureDesc desc = requested;
        desc.Width = (u32)nativeDesc.Width;
        desc.Height = nativeDesc.Height;
        desc.Depth = nativeDesc.Dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? nativeDesc.DepthOrArraySize : 1;
        desc.ArraySize = nativeDesc.Dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? 1 : nativeDesc.DepthOrArraySize;
        desc.MipLevels = nativeDesc.MipLevels;
        desc.Format = (ERHI_FORMAT)nativeDesc.Format;
        desc.SampleDescCount = nativeDesc.SampleDesc.Count;
        native->AddRef();
        return new DX12Surface(_device, desc, nativeDesc.Dimension, native, D3D12_RESOURCE_STATE_COMMON);
    }
}

HRESULT InternalDevice12::LoadDDS(const void* data, size_t size, ERHI_USAGE usage, u32 bindFlags,
    ERHI_CPU_ACCESS_FLAG cpuFlags, int& lod, bool fallback, IRHISurface** out_surface)
{
    PROF_EVENT("D3D12: LoadDDS");
    *out_surface = nullptr;
    DirectX::ScratchImage image;
    DirectX::TexMetadata metadata = {};
    HRESULT result = DirectX::LoadFromDDSMemory(data, size, fallback ? DirectX::DDS_FLAGS_NO_16BPP : DirectX::DDS_FLAGS_NONE,
        &metadata, image);
    if (FAILED(result))
    {
        return result;
    }
    size_t firstMip = 0;
    if (!metadata.IsCubemap() && !metadata.IsVolumemap())
    {
        while (lod > 0 && metadata.mipLevels - firstMip > 1 &&
            (metadata.width >> firstMip) > 4 && (metadata.height >> firstMip) > 4)
        {
            ++firstMip;
            --lod;
        }
    }
    if (metadata.width > UINT32_MAX || metadata.height > UINT32_MAX || metadata.depth > UINT16_MAX || metadata.arraySize > UINT16_MAX)
    {
        return E_INVALIDARG;
    }
    RHITextureDesc desc((u32)(metadata.width >> firstMip), (u32)(metadata.height >> firstMip), (ERHI_FORMAT)metadata.format);
    desc.Depth = (u32)metadata.depth;
    desc.ArraySize = (u32)metadata.arraySize;
    desc.MipLevels = (u32)(metadata.mipLevels - firstMip);
    desc.Usage = usage;
    desc.BindFlags = (ERHI_BIND_FLAG)bindFlags;
    desc.CPUAccessFlags = cpuFlags;
    desc.MiscFlags = (u32)metadata.miscFlags;
    DX12Surface* surface = CreateTexture(desc, (D3D12_RESOURCE_DIMENSION)metadata.dimension, nullptr, 0);
    if (!surface)
    {
        return E_FAIL;
    }
    for (u32 layer_idx = 0; layer_idx < desc.ArraySize; ++layer_idx)
    {
        for (u32 mip_idx = 0; mip_idx < desc.MipLevels; ++mip_idx)
        {
            const auto source = image.GetImage(mip_idx + firstMip, layer_idx, 0);
            if (!source || source->rowPitch > UINT32_MAX || source->slicePitch > UINT32_MAX)
            {
                surface->Release();
                return E_INVALIDARG;
            }
            RHISubResource subresource;
            subresource.Data = source->pixels;
            subresource.RowPitch = (u32)source->rowPitch;
            subresource.DepthPitch = (u32)source->slicePitch;
            if (!UploadTexture(*surface, mip_idx + layer_idx * desc.MipLevels, subresource))
            {
                surface->Release();
                return E_FAIL;
            }
        }
    }
    *out_surface = surface;
    return S_OK;
}

IRHIShaderResourceView* DX12TextureFactory::CreateShaderResourceView(IRHISurface* source, const RHIShaderResourceViewDesc* requested)
{
    InternalDevice12::ContextLock guard(_device);
    auto surface = static_cast<DX12Surface*>(source);
    const auto& textureDesc = surface->GetDesc();
    D3D12_SHADER_RESOURCE_VIEW_DESC desc = {};
    desc.Format = requested && requested->Format != ERHI_FORMAT::UNKNOWN ? (DXGI_FORMAT)requested->Format :
        ViewFormat((DXGI_FORMAT)textureDesc.Format, false);
    desc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    desc.ViewDimension = requested && requested->ViewDimension != ERHI_SRV_DIMENSION::UNKNOWN ?
        (D3D12_SRV_DIMENSION)requested->ViewDimension :
        surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? D3D12_SRV_DIMENSION_TEXTURE3D :
        surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE1D ?
        (textureDesc.ArraySize > 1 ? D3D12_SRV_DIMENSION_TEXTURE1DARRAY : D3D12_SRV_DIMENSION_TEXTURE1D) :
        textureDesc.MiscFlags & 4 ? (textureDesc.ArraySize > 6 ? D3D12_SRV_DIMENSION_TEXTURECUBEARRAY : D3D12_SRV_DIMENSION_TEXTURECUBE) :
        textureDesc.SampleDescCount > 1 ? (textureDesc.ArraySize > 1 ? D3D12_SRV_DIMENSION_TEXTURE2DMSARRAY : D3D12_SRV_DIMENSION_TEXTURE2DMS) :
        textureDesc.ArraySize > 1 ? D3D12_SRV_DIMENSION_TEXTURE2DARRAY : D3D12_SRV_DIMENSION_TEXTURE2D;
    const u32 mip = requested ? requested->MostDetailedMip : 0;
    const u32 mips = requested && requested->MipLevels ? requested->MipLevels : textureDesc.MipLevels - mip;
    const u32 plane = desc.Format == DXGI_FORMAT_X24_TYPELESS_G8_UINT ||
        desc.Format == DXGI_FORMAT_X32_TYPELESS_G8X24_UINT ? 1 : 0;
    const u32 first = requested ? requested->FirstArraySlice : 0;
    const u32 layers = requested ? requested->ArraySize : textureDesc.ArraySize;
    switch (desc.ViewDimension)
    {
    case D3D12_SRV_DIMENSION_TEXTURE1D: desc.Texture1D = { mip, mips, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURE1DARRAY: desc.Texture1DArray = { mip, mips, first, layers, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURE2D: desc.Texture2D = { mip, mips, plane, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURE2DARRAY: desc.Texture2DArray = { mip, mips, first, layers, plane, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURE2DMS: break;
    case D3D12_SRV_DIMENSION_TEXTURE2DMSARRAY: desc.Texture2DMSArray = { first, layers }; break;
    case D3D12_SRV_DIMENSION_TEXTURE3D: desc.Texture3D = { mip, mips, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURECUBE: desc.TextureCube = { mip, mips, 0 }; break;
    case D3D12_SRV_DIMENSION_TEXTURECUBEARRAY: desc.TextureCubeArray = { mip, mips, first, layers / 6, 0 }; break;
    default: return nullptr;
    }
    DX12View view;
    view.Surface = surface;
    view.Format = desc.Format;
    view.Mip = mip;
    view.MipCount = mips;
    view.Plane = plane;
    view.FirstSlice = first;
    view.SliceCount = desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURE3D || desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURE2D ||
        desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURE1D || desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURE2DMS ||
        desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURECUBE ? (desc.ViewDimension == D3D12_SRV_DIMENSION_TEXTURECUBE ? 6u : 1u) : layers;
    view.Descriptor = _device.AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    _device.GetDevice()->CreateShaderResourceView(surface->GetResource().Native, &desc, view.Descriptor.Cpu);
    _device._counters.SrvCreates.fetch_add(1, std::memory_order_relaxed);
    return new DX12ShaderResourceView(_device, view);
}

IRHIRenderTargetView* DX12TextureFactory::CreateRenderTargetView(IRHISurface* source, const RHIRenderTargetViewDesc& requested)
{
    InternalDevice12::ContextLock guard(_device);
    auto surface = static_cast<DX12Surface*>(source);
    const auto& textureDesc = surface->GetDesc();
    D3D12_RENDER_TARGET_VIEW_DESC desc = {};
    desc.Format = requested.Format != ERHI_FORMAT::UNKNOWN ? (DXGI_FORMAT)requested.Format : ViewFormat((DXGI_FORMAT)textureDesc.Format, false);
    desc.ViewDimension = requested.ViewDimension != ERHI_RTV_DIMENSION::UNKNOWN ? (D3D12_RTV_DIMENSION)requested.ViewDimension :
        surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? D3D12_RTV_DIMENSION_TEXTURE3D :
        surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE1D ? (textureDesc.ArraySize > 1 ? D3D12_RTV_DIMENSION_TEXTURE1DARRAY : D3D12_RTV_DIMENSION_TEXTURE1D) :
        textureDesc.SampleDescCount > 1 ? (textureDesc.ArraySize > 1 ? D3D12_RTV_DIMENSION_TEXTURE2DMSARRAY : D3D12_RTV_DIMENSION_TEXTURE2DMS) :
        textureDesc.ArraySize > 1 ? D3D12_RTV_DIMENSION_TEXTURE2DARRAY : D3D12_RTV_DIMENSION_TEXTURE2D;
    switch (desc.ViewDimension)
    {
    case D3D12_RTV_DIMENSION_TEXTURE1D: desc.Texture1D.MipSlice = requested.MipSlice; break;
    case D3D12_RTV_DIMENSION_TEXTURE1DARRAY: desc.Texture1DArray = { requested.MipSlice, requested.FirstArraySlice, requested.ArraySize }; break;
    case D3D12_RTV_DIMENSION_TEXTURE2D: desc.Texture2D = { requested.MipSlice, 0 }; break;
    case D3D12_RTV_DIMENSION_TEXTURE2DARRAY: desc.Texture2DArray = { requested.MipSlice, requested.FirstArraySlice, requested.ArraySize, 0 }; break;
    case D3D12_RTV_DIMENSION_TEXTURE2DMS: break;
    case D3D12_RTV_DIMENSION_TEXTURE2DMSARRAY: desc.Texture2DMSArray = { requested.FirstArraySlice, requested.ArraySize }; break;
    case D3D12_RTV_DIMENSION_TEXTURE3D: desc.Texture3D = { requested.MipSlice, requested.FirstArraySlice, requested.ArraySize }; break;
    default: return nullptr;
    }
    DX12View view;
    view.Surface = surface;
    view.Format = desc.Format;
    view.Mip = requested.MipSlice;
    view.MipCount = 1;
    view.FirstSlice = requested.FirstArraySlice;
    view.SliceCount = desc.ViewDimension == D3D12_RTV_DIMENSION_TEXTURE3D || desc.ViewDimension == D3D12_RTV_DIMENSION_TEXTURE2D ||
        desc.ViewDimension == D3D12_RTV_DIMENSION_TEXTURE1D || desc.ViewDimension == D3D12_RTV_DIMENSION_TEXTURE2DMS ? 1u : requested.ArraySize;
    view.Descriptor = _device.AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    _device.GetDevice()->CreateRenderTargetView(surface->GetResource().Native, &desc, view.Descriptor.Cpu);
    return new DX12RenderTargetView(_device, view);
}

IRHIDepthStencilView* DX12TextureFactory::CreateDepthStencilView(IRHISurface* source, const RHIDepthStencilViewDesc& requested)
{
    InternalDevice12::ContextLock guard(_device);
    auto surface = static_cast<DX12Surface*>(source);
    const auto& textureDesc = surface->GetDesc();
    D3D12_DEPTH_STENCIL_VIEW_DESC desc = {};
    desc.Format = requested.Format != ERHI_FORMAT::UNKNOWN ? (DXGI_FORMAT)requested.Format : ViewFormat((DXGI_FORMAT)textureDesc.Format, true);
    desc.Flags = (D3D12_DSV_FLAGS)requested.Flags;
    desc.ViewDimension = requested.ViewDimension != ERHI_DSV_DIMENSION::UNKNOWN ? (D3D12_DSV_DIMENSION)requested.ViewDimension :
        surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE1D ? (textureDesc.ArraySize > 1 ? D3D12_DSV_DIMENSION_TEXTURE1DARRAY : D3D12_DSV_DIMENSION_TEXTURE1D) :
        textureDesc.SampleDescCount > 1 ? (textureDesc.ArraySize > 1 ? D3D12_DSV_DIMENSION_TEXTURE2DMSARRAY : D3D12_DSV_DIMENSION_TEXTURE2DMS) :
        textureDesc.ArraySize > 1 ? D3D12_DSV_DIMENSION_TEXTURE2DARRAY : D3D12_DSV_DIMENSION_TEXTURE2D;
    switch (desc.ViewDimension)
    {
    case D3D12_DSV_DIMENSION_TEXTURE1D: desc.Texture1D.MipSlice = requested.MipSlice; break;
    case D3D12_DSV_DIMENSION_TEXTURE1DARRAY: desc.Texture1DArray = { requested.MipSlice, requested.FirstArraySlice, requested.ArraySize }; break;
    case D3D12_DSV_DIMENSION_TEXTURE2D: desc.Texture2D.MipSlice = requested.MipSlice; break;
    case D3D12_DSV_DIMENSION_TEXTURE2DARRAY: desc.Texture2DArray = { requested.MipSlice, requested.FirstArraySlice, requested.ArraySize }; break;
    case D3D12_DSV_DIMENSION_TEXTURE2DMS: break;
    case D3D12_DSV_DIMENSION_TEXTURE2DMSARRAY: desc.Texture2DMSArray = { requested.FirstArraySlice, requested.ArraySize }; break;
    default: return nullptr;
    }
    DX12View view;
    view.Surface = surface;
    view.Format = desc.Format;
    view.Mip = requested.MipSlice;
    view.MipCount = 1;
    view.FirstSlice = requested.FirstArraySlice;
    view.SliceCount = desc.ViewDimension == D3D12_DSV_DIMENSION_TEXTURE2D || desc.ViewDimension == D3D12_DSV_DIMENSION_TEXTURE1D ||
        desc.ViewDimension == D3D12_DSV_DIMENSION_TEXTURE2DMS ? 1u : requested.ArraySize;
    view.Flags = requested.Flags;
    view.Planes = desc.Format == DXGI_FORMAT_D24_UNORM_S8_UINT || desc.Format == DXGI_FORMAT_D32_FLOAT_S8X24_UINT ? 2u : 1u;
    view.Descriptor = _device.AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_DSV);
    _device.GetDevice()->CreateDepthStencilView(surface->GetResource().Native, &desc, view.Descriptor.Cpu);
    return new DX12DepthStencilView(_device, view, (ERHI_DSV_DIMENSION)desc.ViewDimension);
}

IRHIUnorderedAccessView* DX12TextureFactory::CreateUAV(IRHISurface* source, const RHIUAVDesc& requested)
{
    InternalDevice12::ContextLock guard(_device);
    auto surface = static_cast<DX12Surface*>(source);
    D3D12_UNORDERED_ACCESS_VIEW_DESC desc = {};
    desc.Format = requested.Format != ERHI_FORMAT::UNKNOWN ? (DXGI_FORMAT)requested.Format : ViewFormat((DXGI_FORMAT)surface->GetFormat(), false);
    desc.ViewDimension = requested.ViewDimension == ERHI_VIEW_DIMENSION::Texture3D ? D3D12_UAV_DIMENSION_TEXTURE3D : D3D12_UAV_DIMENSION_TEXTURE2D;
    if (desc.ViewDimension == D3D12_UAV_DIMENSION_TEXTURE3D)
    {
        desc.Texture3D = { requested.MipSlice, requested.FirstWSlice, requested.WSize };
    }
    else
    {
        desc.Texture2D = { requested.MipSlice, 0 };
    }
    DX12View view;
    view.Surface = surface;
    view.Format = desc.Format;
    view.Mip = requested.MipSlice;
    view.MipCount = 1;
    view.SliceCount = desc.ViewDimension == D3D12_UAV_DIMENSION_TEXTURE3D ? 1u : 1u;
    view.Descriptor = _device.AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    _device.GetDevice()->CreateUnorderedAccessView(surface->GetResource().Native, nullptr, &desc, view.Descriptor.Cpu);
    _device._counters.UavCreates.fetch_add(1, std::memory_order_relaxed);
    return new DX12UnorderedAccessView(_device, view);
}

IRHIShaderResourceView* InternalDevice12::CreateShaderResourceView(IRHIBuffer* source, const RHIShaderResourceViewDesc* requested)
{
    ContextLock guard(*this);
    R_ASSERT(source && requested);
    auto buffer = static_cast<DX12Buffer*>(source);
    D3D12_SHADER_RESOURCE_VIEW_DESC desc = {};
    desc.Format = (DXGI_FORMAT)requested->Format;
    desc.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
    desc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    desc.Buffer.NumElements = requested->ElementWidth;
    if (desc.Format == DXGI_FORMAT_UNKNOWN)
    {
        desc.Buffer.StructureByteStride = buffer->GetDesc().StructureByteStride;
    }
    DX12View view;
    view.Buffer = buffer;
    view.Format = desc.Format;
    view.Kind = desc.Format == DXGI_FORMAT_UNKNOWN ? DX12KindStructured :
        desc.Format == DXGI_FORMAT_R32_TYPELESS ? DX12KindRaw : DX12KindTyped;
    view.Descriptor = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    GetDevice()->CreateShaderResourceView(buffer->GetResource().Native, &desc, view.Descriptor.Cpu);
    _counters.SrvCreates.fetch_add(1, std::memory_order_relaxed);
    return new DX12ShaderResourceView(*this, view);
}
