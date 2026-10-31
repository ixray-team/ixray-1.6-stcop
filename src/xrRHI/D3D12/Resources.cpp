#include "Device.h"

DX12Buffer::DX12Buffer(InternalDevice12& device, const RHIBufferDesc& desc, const RHIBufferSubresource* data) :
    _device(device), _desc(desc), _data(desc.Usage == ERHI_USAGE::USAGE_DYNAMIC && desc.Type != ERHI_BUFFER_TYPE::STRUCTURED ? desc.Size : 0)
{
    R_ASSERT(desc.Size);
    {
        InternalDevice12::ContextLock guard(_device);
        _device._buffers.push_back(this);
        if (IsStream())
        {
            _resource.UniformState = D3D12_RESOURCE_STATE_GENERIC_READ;
            if (!data || !data->pSysMem)
            {
                DiscardStream();
            }
        }
        else
        {
            _resource.Native = device.CreateNativeBuffer(desc.Size, D3D12_HEAP_TYPE_DEFAULT,
                desc.Type == ERHI_BUFFER_TYPE::STRUCTURED_RW ? D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS : D3D12_RESOURCE_FLAG_NONE);
            _resource.States.assign(1, D3D12_RESOURCE_STATE_COMMON);
            _nativeAddress = _resource.Native->GetGPUVirtualAddress();
        }
    }
    if (data && data->pSysMem)
    {
        R_ASSERT(!data->SysMemSize || data->SysMemSize >= desc.Size);
        UpdateSubresource(data->pSysMem, desc.Size);
    }
}

DX12Buffer::~DX12Buffer()
{
    InternalDevice12::ContextLock guard(_device);
    auto found = std::find(_device._buffers.begin(), _device._buffers.end(), this);
    R_ASSERT(found != _device._buffers.end());
    *found = _device._buffers.back();
    _device._buffers.pop_back();
    if (_upload.Owned && _upload.Resource)
    {
        _upload.Resource->Unmap(0, nullptr);
        _device.Retire(_upload.Resource);
    }
    _device.Retire(_resource.Native);
}

void DX12Buffer::AddRef()
{
    ++_references;
}

u32 DX12Buffer::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}

u32 DX12Buffer::GetSize() const
{
    return _desc.Size;
}

bool DX12Buffer::Map(ERHI_BUFFER_MAP type, u32 flags, RHIMappedSubresource* out_data)
{
    R_ASSERT(out_data);
    if (type == ERHI_BUFFER_MAP::READ || type == ERHI_BUFFER_MAP::READ_AND_WRITE)
    {
        InternalDevice12::ContextLock guard(_device);
        R_ASSERT(!_isMapped);
        if (!_resource.Native)
        {
            return false;
        }
        ID3D12Resource* readback = _device.CreateNativeBuffer(_desc.Size, D3D12_HEAP_TYPE_READBACK);
        _device.Transition(_resource, D3D12_RESOURCE_STATE_COPY_SOURCE);
        _device.Commands()->CopyBufferRegion(readback, 0, _resource.Native, 0, _desc.Size);
        _device.Flush();
        void* mapped = nullptr;
        D3D12_RANGE range = { 0, _desc.Size };
        R_CHK(readback->Map(0, &range, &mapped));
        _data.resize(_desc.Size);
        memcpy(_data.data(), mapped, _desc.Size);
        D3D12_RANGE written = {};
        readback->Unmap(0, &written);
        readback->Release();
        _mapType = type;
        _isMapped = true;
        *out_data = { _data.data(), _desc.Size, _desc.Size };
        return true;
    }
    u8* mapped = nullptr;
    bool copyShadow = false;
    {
        InternalDevice12::ContextLock guard(_device);
        R_ASSERT(!_isMapped);
        _mapType = type;
        _isMapped = true;
        if (!IsStream())
        {
            _data.resize(_desc.Size);
            mapped = _data.data();
        }
        else
        {
            if (type != ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE)
            {
                DiscardStream();
                _device._counters.Discards.fetch_add(1, std::memory_order_relaxed);
                copyShadow = type != ERHI_BUFFER_MAP::WRITE_DISCARD && !_data.empty();
            }
            mapped = _upload.Data;
        }
    }
    if (copyShadow)
    {
        memcpy(mapped, _data.data(), _desc.Size);
        _device._counters.CopiedBytes.fetch_add(_desc.Size, std::memory_order_relaxed);
    }
    *out_data = { mapped, _desc.Size, _desc.Size };
    return true;
}

void DX12Buffer::Unmap()
{
    InternalDevice12::ContextLock guard(_device);
    R_ASSERT(_isMapped);
    _isMapped = false;
    if (!IsStream() && _mapType != ERHI_BUFFER_MAP::READ)
    {
        UpdateSubresource(_data.data(), _desc.Size);
    }
}

void DX12Buffer::UpdateSubresource(const void* data, u32 size, u32 offset)
{
    R_ASSERT(data && size <= _desc.Size && offset <= _desc.Size - size);
    if (IsStream())
    {
        VERIFY(!offset);
        u8* destination = nullptr;
        {
            InternalDevice12::ContextLock guard(_device);
            R_ASSERT(!_isMapped);
            DiscardStream();
            _device._counters.Discards.fetch_add(1, std::memory_order_relaxed);
            destination = _upload.Data;
            if (!_data.empty() && data != _data.data())
            {
                memcpy(_data.data(), data, size);
            }
        }
        const u32 copied = _data.empty() ? size : _desc.Size;
        memcpy(destination, _data.empty() ? data : _data.data(), copied);
        _device._counters.CopiedBytes.fetch_add(copied, std::memory_order_relaxed);
        return;
    }
    DX12Upload upload = size >= InternalDevice12::UploadChunkSize ? _device.CreateUpload(size) : _device.AllocateUpload(size);
    memcpy(upload.Data, data, size);
    _device._counters.CopiedBytes.fetch_add(size, std::memory_order_relaxed);
    {
        InternalDevice12::ContextLock guard(_device);
        R_ASSERT(!_isMapped);
        if (!_data.empty() && data != _data.data())
        {
            memcpy(_data.data() + offset, data, size);
        }
    }
    InternalDevice12::PendingCopy copy;
    copy.Buffer = this;
    copy.Source = upload;
    copy.DestOffset = offset;
    copy.CopySize = size;
    _device.EnqueueCopy(std::move(copy));
}

D3D12_GPU_VIRTUAL_ADDRESS DX12Buffer::GetAddress()
{
    if (_nativeAddress)
    {
        return _nativeAddress;
    }
    InternalDevice12::ContextLock guard(_device);
    if (!_published)
    {
        _published = _upload.Resource->GetGPUVirtualAddress();
    }
    return _published;
}

void DX12Buffer::DiscardStream()
{
    if (_upload.Owned && _upload.Resource)
    {
        _device.RecycleStream(_upload);
    }
    _upload = _device.AcquireStream(_desc.Size);
    _published = 0;
    for (auto view : _views)
    {
        _device.WriteBufferView(view->View);
    }
    if (!_views.empty())
    {
        _device.MarkViewsDirty();
    }
}

void DX12Buffer::SetVertexBuffer(u32 slot, u32 stride, u32 offset)
{
    _device.BindVertexBuffer(this, slot, stride, offset);
}

void DX12Buffer::SetIndexBuffer(bool is32Bit, u32 offset)
{
    _device.BindIndexBuffer(this, is32Bit, offset);
}

DX12Surface::DX12Surface(InternalDevice12& device, const RHITextureDesc& desc, D3D12_RESOURCE_DIMENSION dimension,
    ID3D12Resource* native, D3D12_RESOURCE_STATES state) :
    _device(device), _desc(desc), _dimension(dimension)
{
    InternalDevice12::ContextLock guard(_device);
    _resource.Native = native;
    D3D12_FEATURE_DATA_FORMAT_INFO format = { native->GetDesc().Format };
    R_CHK(device.GetDevice()->CheckFeatureSupport(D3D12_FEATURE_FORMAT_INFO, &format, sizeof(format)));
    const u32 count = desc.MipLevels * (dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? 1 : desc.ArraySize) * format.PlaneCount;
    _resource.States.assign(count, state);
    _resource.UniformState = state;
    _resource.Uniform = true;
    _device.RegisterSurface(this);
}

DX12Surface::~DX12Surface()
{
    InternalDevice12::ContextLock guard(_device);
    _device.UnregisterSurface(this);
    if (_readback)
    {
        _readback->Unmap(0, nullptr);
        _device.Retire(_readback);
    }
    _device.Retire(_resource.Native);
}

void DX12Surface::AddRef()
{
    ++_references;
}

u32 DX12Surface::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}

ERHI_RESOURCE_DIMENSION DX12Surface::GetTextureType() const
{
    return (ERHI_RESOURCE_DIMENSION)_dimension;
}

bool DX12Surface::UpdateData(u32 mip, u32 layer, const RHISubResource* data, const RHIBox& box)
{
    if (!data || mip >= _desc.MipLevels || layer >= _desc.ArraySize)
    {
        return false;
    }
    const bool hasBox = box.right > box.left && box.bottom > box.top && box.back > box.front;
    return _device.UploadTexture(*this, mip + layer * _desc.MipLevels, *data, hasBox ? &box : nullptr);
}

void* DX12Surface::Lock(u32 mip, u32* out_pitch)
{
    InternalDevice12::ContextLock guard(_device);
    R_ASSERT(!_readback && _lockedMip == UINT32_MAX);
    if (mip >= _desc.MipLevels)
    {
        return nullptr;
    }
    if (_desc.Usage == ERHI_USAGE::USAGE_DYNAMIC)
    {
        const auto native = _resource.Native->GetDesc();
        u32 rows = 0;
        u64 rowSize = 0;
        _device.GetDevice()->GetCopyableFootprints(&native, mip, 1, 0, &_footprint, &rows, &rowSize, nullptr);
        const u64 sliceSize = rowSize * rows;
        const u64 size = sliceSize * _footprint.Footprint.Depth;
        if (!size || rowSize > UINT32_MAX || sliceSize > UINT32_MAX || size > SIZE_MAX)
        {
            return nullptr;
        }
        _writePitch = u32(rowSize);
        _writeSlicePitch = u32(sliceSize);
        _writeData.resize(size_t(size));
        _lockedMip = mip;
        if (out_pitch)
        {
            *out_pitch = _writePitch;
        }
        return _writeData.data();
    }
    if (!_device.ReadTexture(*this, mip, &_readback, _footprint))
    {
        return nullptr;
    }
    void* data = nullptr;
    const size_t size = size_t(_readback->GetDesc().Width);
    D3D12_RANGE range = { 0, size };
    R_CHK(_readback->Map(0, &range, &data));
    if (out_pitch)
    {
        *out_pitch = _footprint.Footprint.RowPitch;
    }
    return (u8*)data + _footprint.Offset;
}

void DX12Surface::Unlock()
{
    u32 mip = UINT32_MAX;
    RHISubResource data;
    {
        InternalDevice12::ContextLock guard(_device);
        if (_lockedMip != UINT32_MAX)
        {
            mip = _lockedMip;
            data.Data = _writeData.data();
            data.RowPitch = _writePitch;
            data.DepthPitch = _writeSlicePitch;
            _lockedMip = UINT32_MAX;
        }
        else
        {
            R_ASSERT(_readback);
            D3D12_RANGE written = {};
            _readback->Unmap(0, &written);
            _readback->Release();
            _readback = nullptr;
            return;
        }
    }
    R_ASSERT2(_device.UploadTexture(*this, mip, data), "Unable to upload a dynamic texture");
}

static void RetainView(const DX12View& view)
{
    if (view.Surface)
    {
        view.Surface->AddRef();
    }
    if (view.Buffer)
    {
        view.Buffer->AddRef();
    }
}

static void ReleaseView(InternalDevice12& device, const DX12View& view)
{
    device.Retire(view.Descriptor);
    if (view.Surface)
    {
        view.Surface->Release();
    }
    if (view.Buffer)
    {
        view.Buffer->Release();
    }
}

DX12ShaderResourceView::DX12ShaderResourceView(InternalDevice12& device, const DX12View& view) :
    View(view), _device(device)
{
    InternalDevice12::ContextLock guard(_device);
    RetainView(View);
    _device.RegisterImage(this);
    if (View.Buffer && View.Buffer->IsStream())
    {
        View.Buffer->_views.push_back(this);
    }
}

DX12ShaderResourceView::~DX12ShaderResourceView()
{
    InternalDevice12::ContextLock guard(_device);
    _device.UnregisterImage(this);
    if (View.Buffer)
    {
        auto& views = View.Buffer->_views;
        views.erase(std::remove(views.begin(), views.end(), this), views.end());
    }
    ReleaseView(_device, View);
}

void* DX12ShaderResourceView::GetRawSRV()
{
    return _device.GetImageHandle(View.Descriptor.Index);
}

void DX12ShaderResourceView::AddRef()
{
    ++_references;
}

u32 DX12ShaderResourceView::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}

DX12RenderTargetView::DX12RenderTargetView(InternalDevice12& device, const DX12View& view) :
    View(view), _device(device)
{
    InternalDevice12::ContextLock guard(_device);
    RetainView(View);
}

DX12RenderTargetView::~DX12RenderTargetView()
{
    InternalDevice12::ContextLock guard(_device);
    ReleaseView(_device, View);
}

void DX12RenderTargetView::AddRef()
{
    ++_references;
}

u32 DX12RenderTargetView::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}

DX12UnorderedAccessView::DX12UnorderedAccessView(InternalDevice12& device, const DX12View& view) :
    View(view), _device(device)
{
    InternalDevice12::ContextLock guard(_device);
    RetainView(View);
}

DX12UnorderedAccessView::~DX12UnorderedAccessView()
{
    InternalDevice12::ContextLock guard(_device);
    ReleaseView(_device, View);
}

void DX12UnorderedAccessView::AddRef()
{
    ++_references;
}

u32 DX12UnorderedAccessView::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}

DX12DepthStencilView::DX12DepthStencilView(InternalDevice12& device, const DX12View& view, ERHI_DSV_DIMENSION dimension) :
    View(view), _device(device), _dimension(dimension)
{
    InternalDevice12::ContextLock guard(_device);
    RetainView(View);
}

DX12DepthStencilView::~DX12DepthStencilView()
{
    InternalDevice12::ContextLock guard(_device);
    ReleaseView(_device, View);
}

void DX12DepthStencilView::AddRef()
{
    ++_references;
}

u32 DX12DepthStencilView::Release()
{
    const u32 remaining = --_references;
    if (!remaining)
    {
        auto self = this;
        xr_delete(self);
    }
    return remaining;
}
