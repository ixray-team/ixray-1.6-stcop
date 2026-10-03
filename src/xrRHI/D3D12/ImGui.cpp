#include "Device.h"

void InternalDevice12::RegisterSurface(DX12Surface* surface)
{
    ContextLock guard(*this);
    _surfaces.push_back(surface);
}

void InternalDevice12::UnregisterSurface(DX12Surface* surface)
{
    ContextLock guard(*this);
    auto found = std::find(_surfaces.begin(), _surfaces.end(), surface);
    R_ASSERT(found != _surfaces.end());
    *found = _surfaces.back();
    _surfaces.pop_back();
}

DX12Surface* InternalDevice12::FindSurface(ID3D12Resource* resource)
{
    ContextLock guard(*this);
    for (auto surface : _surfaces)
    {
        if (surface->GetResource().Native == resource)
        {
            return surface;
        }
    }
    return nullptr;
}

void InternalDevice12::RegisterImage(DX12ShaderResourceView* view)
{
    ContextLock guard(*this);
    const u32 index = view->View.Descriptor.Index;
    R_ASSERT(index < _images.size());
    R_ASSERT(!_images[index]);
    PublishDescriptor(view->View.Descriptor);
    _images[index] = view;
}

void InternalDevice12::UnregisterImage(DX12ShaderResourceView* view)
{
    ContextLock guard(*this);
    const u32 index = view->View.Descriptor.Index;
    R_ASSERT(index < _images.size() && _images[index] == view);
    _images[index] = nullptr;
    for (u32 stage_idx = 0; stage_idx < 6; ++stage_idx)
    {
        for (auto& resource : _resources[stage_idx])
        {
            if (resource == view)
            {
                resource = nullptr;
                _viewsDirty[stage_idx] = true;
            }
        }
    }
}

void InternalDevice12::PrepareImage(u64 texture)
{
    ContextLock guard(*this);
    const u32 index = ImageIndex(texture);
    if (index == UINT32_MAX || !_images[index])
    {
        return;
    }
    auto& view = _images[index]->View;
    if (view.Surface)
    {
        auto state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE;
        const u32 mips = view.Surface->GetMipLevels();
        const u32 layers = view.Surface->GetDimension() == D3D12_RESOURCE_DIMENSION_TEXTURE3D ? 1u : view.Surface->GetArraySize();
        const u32 subresource = view.Mip + view.FirstSlice * mips + view.Plane * mips * layers;
        if (subresource < view.Surface->GetResource().States.size() &&
            (view.Surface->GetResource().States[subresource] & D3D12_RESOURCE_STATE_DEPTH_READ))
        {
            state |= D3D12_RESOURCE_STATE_DEPTH_READ;
        }
        TransitionView(*view.Surface, state, view);
    }
    else if (view.Buffer && view.Buffer->GetResource().Native)
    {
        Transition(view.Buffer->GetResource(), D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE |
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
    }
}

void InternalDevice12::PrepareImGuiTarget()
{
    ContextLock guard(*this);
    auto target = static_cast<DX12RenderTargetView*>(SwapChainRTV);
    R_ASSERT(target && target->View.Format == DXGI_FORMAT_B8G8R8A8_UNORM);
    Transition(target->View.Surface->GetResource(), D3D12_RESOURCE_STATE_RENDER_TARGET);
    InvalidateBindings();
    Commands()->SetDescriptorHeaps(1, &_resourceHeap);
    _commands->OMSetRenderTargets(1, &target->View.Descriptor.Cpu, FALSE, nullptr);
}

void InternalDevice12::FinishExternalWork()
{
    ContextLock guard(*this);
    R_ASSERT(!_isRecording);
    const u64 fence = _nextFence++;
    R_CHK(_queue->Signal(_fence, fence));
    _frames[_frame].Fence = fence;
    InvalidateBindings();
}

void InternalDevice12::PrepareUpscale(IRHISurface* source, bool output)
{
    ContextLock guard(*this);
    if (!source)
    {
        return;
    }
    auto& surface = *static_cast<DX12Surface*>(source);
    const auto& desc = surface.GetDesc();
    Transition(surface.GetResource(), output ? D3D12_RESOURCE_STATE_UNORDERED_ACCESS :
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
}

void* InternalDevice12::GetImageHandle(u32 index)
{
    ContextLock guard(*this);
    R_ASSERT(index < _images.size() && _images[index]);
    return &_images[index];
}

u32 InternalDevice12::ImageIndex(u64 texture) const
{
    const u64 base = u64(reinterpret_cast<uintptr_t>(_images.data()));
    const u64 stride = sizeof(_images[0]);
    if (texture < base || texture - base >= _images.size() * stride || (texture - base) % stride)
    {
        return UINT32_MAX;
    }
    return u32((texture - base) / stride);
}

u64 InternalDevice12::ResolveImageHandle(u64 texture) const
{
    ContextLock guard(*this);
    const u32 index = ImageIndex(texture);
    if (index == UINT32_MAX)
    {
        return texture;
    }
    R_ASSERT(_images[index]);
    return _images[index]->View.Descriptor.Gpu.ptr;
}
