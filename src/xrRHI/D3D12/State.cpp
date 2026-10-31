#include "Device.h"
#include "../Layout/ImGui/imgui_impl_dx12.h"

static D3D12_RASTERIZER_DESC RasterizerDesc(const RHIRasterizerDesc& desc)
{
    return { (D3D12_FILL_MODE)desc.FillMode, (D3D12_CULL_MODE)desc.CullMode, desc.FrontCounterClockwise,
        desc.DepthBias, desc.DepthBiasClamp, desc.SlopeScaledDepthBias, desc.DepthClipEnable,
        desc.MultisampleEnable, desc.AntialiasedLineEnable, 0, D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF };
}

static D3D12_DEPTH_STENCIL_DESC DepthDesc(const RHIDepthStencilDesc& desc)
{
    auto face = [](const RHIStencilOpDesc& stencil)
    {
        return D3D12_DEPTH_STENCILOP_DESC{ (D3D12_STENCIL_OP)stencil.StencilFailOp,
            (D3D12_STENCIL_OP)stencil.StencilDepthFailOp, (D3D12_STENCIL_OP)stencil.StencilPassOp,
            (D3D12_COMPARISON_FUNC)stencil.StencilFunc };
    };
    return { desc.DepthEnable, (D3D12_DEPTH_WRITE_MASK)desc.DepthWriteMask, (D3D12_COMPARISON_FUNC)desc.DepthFunc,
        desc.StencilEnable, desc.StencilReadMask, desc.StencilWriteMask, face(desc.FrontFace), face(desc.BackFace) };
}

static D3D12_BLEND_DESC BlendDesc(const RHIBlendDesc& desc)
{
    D3D12_BLEND_DESC native = {};
    native.AlphaToCoverageEnable = desc.AlphaToCoverageEnable;
    native.IndependentBlendEnable = desc.IndependentBlendEnable;
    for (u32 target_idx = 0; target_idx < 8; ++target_idx)
    {
        const auto& source = desc.RenderTarget[target_idx];
        native.RenderTarget[target_idx] = { source.BlendEnable, FALSE, (D3D12_BLEND)source.SrcBlend,
            (D3D12_BLEND)source.DestBlend, (D3D12_BLEND_OP)source.BlendOp, (D3D12_BLEND)source.SrcBlendAlpha,
            (D3D12_BLEND)source.DestBlendAlpha, (D3D12_BLEND_OP)source.BlendOpAlpha, D3D12_LOGIC_OP_NOOP,
            source.RenderTargetWriteMask };
    }
    return native;
}

class DX12StateManager final : public IRHIStateManager
{
public:
    explicit DX12StateManager(InternalDevice12& device) : _device(device)
    {
        Reset();
    }
    void Apply() override
    {
        InternalDevice12::ContextLock guard(_device);
        if (_dirty)
        {
            _nativeRaster = RasterizerDesc(_rasterDesc);
            _nativeDepth = DepthDesc(_depthDesc);
            _nativeBlend = BlendDesc(_blendDesc);
            _dirty = false;
            _appliedSerial = 0;
        }
        if (_appliedSerial != _device._stateSerial)
        {
            auto& state = _device._graphicsState;
            if (state.SampleMask != UINT32_MAX || memcmp(&state.Rasterizer, &_nativeRaster, sizeof(_nativeRaster)) ||
                memcmp(&state.Depth, &_nativeDepth, sizeof(_nativeDepth)) || memcmp(&state.Blend, &_nativeBlend, sizeof(_nativeBlend)))
            {
                state.Rasterizer = _nativeRaster;
                state.Depth = _nativeDepth;
                state.Blend = _nativeBlend;
                state.SampleMask = UINT32_MAX;
                _device._pipelineDirty = true;
                ++_device._stateSerial;
            }
            _appliedSerial = _device._stateSerial;
        }

        _device._stencilRef = _stencilRef;
        _device._scissorEnabled = _rasterDesc.ScissorEnable;
    }
    void Reset() override
    {
        ResetRDesc();
        ResetDDesc();
        ResetBDesc();
        _stencilRef = 0;
        Apply();
    }
    void SetRasterizerState(void* state) override
    {
        _dirty = true;
        if (state)
        {
            _rasterDesc = *(RHIRasterizerDesc*)state;
        }
        else
        {
            ResetRDesc();
        }
    }
    void SetDepthStencilState(void* state) override
    {
        _dirty = true;
        if (state)
        {
            _depthDesc = *(RHIDepthStencilDesc*)state;
        }
        else
        {
            ResetDDesc();
        }
    }
    void SetBlendState(void* state) override
    {
        _dirty = true;
        if (state)
        {
            _blendDesc = *(RHIBlendDesc*)state;
        }
        else
        {
            ResetBDesc();
        }
    }
    void SetStencilRef(u32 value) override { _stencilRef = value; }

private:
    InternalDevice12& _device;
    RHIRasterizerDesc _rasterDesc = {};
    RHIDepthStencilDesc _depthDesc = {};
    RHIBlendDesc _blendDesc = {};
    u32 _stencilRef = 0;
    D3D12_RASTERIZER_DESC _nativeRaster = {};
    D3D12_DEPTH_STENCIL_DESC _nativeDepth = {};
    D3D12_BLEND_DESC _nativeBlend = {};
    u64 _appliedSerial = 0;
    bool _dirty = true;
    void ResetRDesc();
    void ResetDDesc();
    void ResetBDesc();
};

void DX12StateManager::ResetBDesc()
{
	_dirty = true;
	ZeroMemory(&_blendDesc, sizeof(_blendDesc));

	_blendDesc.AlphaToCoverageEnable = false;
	_blendDesc.IndependentBlendEnable = false;

	for (u32 target_idx = 0; target_idx < 8; ++target_idx)
	{
		_blendDesc.RenderTarget[target_idx].SrcBlend = RHI_BLEND_ONE;
		_blendDesc.RenderTarget[target_idx].DestBlend = RHI_BLEND_ZERO;
		_blendDesc.RenderTarget[target_idx].BlendOp = RHI_BLEND_OP_ADD;
		_blendDesc.RenderTarget[target_idx].SrcBlendAlpha = RHI_BLEND_ONE;
		_blendDesc.RenderTarget[target_idx].DestBlendAlpha = RHI_BLEND_ZERO;
		_blendDesc.RenderTarget[target_idx].BlendOpAlpha = RHI_BLEND_OP_ADD;
		_blendDesc.RenderTarget[target_idx].BlendEnable = false;
		_blendDesc.RenderTarget[target_idx].RenderTargetWriteMask = RHI_COLOR_WRITE_ENABLE_ALL;
	}
}

void DX12StateManager::ResetDDesc()
{
	_dirty = true;
	ZeroMemory(&_depthDesc, sizeof(_depthDesc));

	_depthDesc.DepthEnable = true;
	_depthDesc.DepthWriteMask = RHI_DEPTH_WRITE_MASK_ALL;
	_depthDesc.DepthFunc = RHI_COMPARISON_LESS;
	_depthDesc.StencilEnable = true;
	_depthDesc.StencilReadMask = 0xFF;
	_depthDesc.StencilWriteMask = 0xFF;

	_depthDesc.FrontFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.FrontFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.FrontFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.FrontFace.StencilFunc = RHI_COMPARISON_ALWAYS;

	_depthDesc.BackFace.StencilFailOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.BackFace.StencilDepthFailOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.BackFace.StencilPassOp = RHI_STENCIL_OP_KEEP;
	_depthDesc.BackFace.StencilFunc = RHI_COMPARISON_ALWAYS;
}

void DX12StateManager::ResetRDesc()
{
	_dirty = true;
	ZeroMemory(&_rasterDesc, sizeof(_rasterDesc));
	_rasterDesc.FillMode = RHI_FILL_SOLID;
	_rasterDesc.CullMode = RHI_CULL_BACK;
	_rasterDesc.FrontCounterClockwise = false;
	_rasterDesc.DepthBias = 0;
	_rasterDesc.DepthBiasClamp = 0.0f;
	_rasterDesc.SlopeScaledDepthBias = 0.0f;
	_rasterDesc.DepthClipEnable = true;
	_rasterDesc.ScissorEnable = false;
	_rasterDesc.MultisampleEnable = false;
	_rasterDesc.AntialiasedLineEnable = false;
}

template <typename TDesc>
static void* CacheState(const TDesc& desc, xr_hash_map<u64, xr_vector<TDesc*>>& cache)
{
    u64 hash = 14695981039346656037ull;
    for (const u8 byte : std::span(reinterpret_cast<const u8*>(&desc), sizeof(desc)))
    {
        hash = (hash ^ byte) * 1099511628211ull;
    }
    auto& bucket = cache[hash];
    for (auto state : bucket)
    {
        if (!memcmp(state, &desc, sizeof(desc)))
        {
            return state;
        }
    }
    return bucket.emplace_back(new TDesc(desc));
}

void* InternalDevice12::GetState(const RHIRasterizerDesc& desc)
{
    ContextLock guard(*this);
    return CacheState(desc, _rasterStates);
}

void* InternalDevice12::GetState(const RHIDepthStencilDesc& desc)
{
    ContextLock guard(*this);
    return CacheState(desc, _depthStates);
}

void* InternalDevice12::GetState(const RHIBlendDesc& desc)
{
    ContextLock guard(*this);
    return CacheState(desc, _blendStates);
}

static void DeleteBlend(void* payload)
{
    auto desc = static_cast<RHIBlendDesc*>(payload);
    xr_delete(desc);
}

HRESULT InternalDevice12::CreateBlendState(const RHIBlendDesc& desc, RHIObject** out_state)
{
    *out_state = new RHIObject(new RHIBlendDesc(desc), DeleteBlend);
    return S_OK;
}

void InternalDevice12::SetBlendState(RHIObject* state, const float* factor, u32 mask)
{
    ContextLock guard(*this);
    SetRawBlendState(state ? state->resource : nullptr, factor, mask);
}

void InternalDevice12::SetRawBlendState(void* state, const float* factor, u32 mask)
{
    ContextLock guard(*this);
    if (state)
    {
        const auto blend = BlendDesc(*(RHIBlendDesc*)state);
        if (ImGui_ImplDX12_SetBlendState(blend, factor, mask))
        {
            return;
        }
        _graphicsState.Blend = blend;
    }
    _graphicsState.SampleMask = mask;
    _pipelineDirty = true;
    ++_stateSerial;
    for (u32 factor_idx = 0; factor_idx < 4; ++factor_idx)
    {
        _blendFactor[factor_idx] = factor ? factor[factor_idx] : 1;
    }
}

IRHIStateManager* InternalDevice12::CreateStateManager()
{
    return new DX12StateManager(*this);
}

class DX12ResourceCache final : public IRHIShaderResourceStateCache
{
public:
    explicit DX12ResourceCache(InternalDevice12& device) : _device(device) {}
    ~DX12ResourceCache() override = default;
    void ResetDeviceState() override
    {
        for (u32 stage_idx = 0; stage_idx < 6; ++stage_idx)
        {
            for (u32 slot_idx = 0; slot_idx < DX12ViewSlots; ++slot_idx)
            {
                _device.BindResource(stage_idx, slot_idx, nullptr);
            }
        }
    }
    void Apply() override {}
    void SetPSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(0, slot, resource); }
    void SetVSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(1, slot, resource); }
    void SetGSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(2, slot, resource); }
    void SetHSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(3, slot, resource); }
    void SetDSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(4, slot, resource); }
    void SetCSResource(u32 slot, IRHIShaderResourceView* resource) override { _device.BindResource(5, slot, resource); }

private:
    InternalDevice12& _device;
};

IRHIShaderResourceStateCache* InternalDevice12::CreateResourceCache()
{
    return new DX12ResourceCache(*this);
}
