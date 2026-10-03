#include "Device.h"
#include "../Layout/ImGui/imgui_impl_dx12.h"
#include "../../xrCore/D3DLegacy.h"

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
    ~DX12StateManager() override = default;
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
            if (state.SampleMask != _sampleMask || memcmp(&state.Rasterizer, &_nativeRaster, sizeof(_nativeRaster)) ||
                memcmp(&state.Depth, &_nativeDepth, sizeof(_nativeDepth)) || memcmp(&state.Blend, &_nativeBlend, sizeof(_nativeBlend)))
            {
                state.Rasterizer = _nativeRaster;
                state.Depth = _nativeDepth;
                state.Blend = _nativeBlend;
                state.SampleMask = _sampleMask;
                _device._pipelineDirty = true;
                ++_device._stateSerial;
            }
            _appliedSerial = _device._stateSerial;
        }

        _device._stencilRef = _stencilRef;
        _device._scissorEnabled = _isScissorOverride ? _overrideScissorValue : _rasterDesc.ScissorEnable;
    }
    void Reset() override
    {
        ResetRDesc();
        ResetDDesc();
        ResetBDesc();
        _sampleMask = UINT32_MAX;
        _stencilRef = 0;
        _isScissorOverride = false;
        CacheCullMode = ERHI_CULLMODE::BACK;
        Apply();
    }
    void EnableScissoring(bool enabled = true) override
    {
        _rasterDesc.ScissorEnable = enabled;
    }
    void OverrideScissoring(bool overrideValue = true, bool value = true) override
    {
        _isScissorOverride = overrideValue;
        _overrideScissorValue = value;
    }
    void SetRasterizerState(void* state) override
    {
        _dirty = true;
        if (state)
        {
            _rasterDesc = *(RHIRasterizerDesc*)state;
            CacheCullMode = (ERHI_CULLMODE)_rasterDesc.CullMode;
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
    void UnmapConstants() override { BindAlphaCallback = nullptr; }
    void SetSampleMask(u32 value)
    {
        _sampleMask = value;
        _dirty = true;
    }
    void* GetCache(ERHI_STATE_CACHE_TYPE type, void* desc) override
    {
        switch (type)
        {
        case ERHI_STATE_CACHE_TYPE::RS: return _device.GetState(*(RHIRasterizerDesc*)desc);
        case ERHI_STATE_CACHE_TYPE::DS: return _device.GetState(*(RHIDepthStencilDesc*)desc);
        case ERHI_STATE_CACHE_TYPE::BS: return _device.GetState(*(RHIBlendDesc*)desc);
        }
        return nullptr;
    }
    void SetAlphaRef(u32 NewAlphaRef) override;
    void SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail) override;
    void SetRenderState(u32 p1, u32 p2) override;
    void SetDepthFunc(u32 Func) override;
    void SetDepthEnable(u32 Enable) override;
    void SetColorWriteEnable(u32 WriteMask) override;
    void SetCullMode(ERHI_CULLMODE Mode) override;
    void BindAlphaRefCallback(const BindAlphaCallbackDecl& Callback) override;
    void SetMultisample(u32 Enable);

private:
    InternalDevice12& _device;
    RHIRasterizerDesc _rasterDesc = {};
    RHIDepthStencilDesc _depthDesc = {};
    RHIBlendDesc _blendDesc = {};
    u32 _sampleMask = UINT32_MAX;
    u32 _stencilRef = 0;
    u32 _alphaRef = 0;
    bool _isScissorOverride = false;
    bool _overrideScissorValue = false;
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

void DX12StateManager::SetAlphaRef(u32 NewAlphaRef)
{
	_alphaRef = NewAlphaRef;

	if (BindAlphaCallback)
	{
		BindAlphaCallback(_alphaRef);
	}
}

void DX12StateManager::SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail)
{
	_dirty = true;
	_depthDesc.StencilEnable = Enable;
	_depthDesc.StencilReadMask = Mask;
	_depthDesc.StencilWriteMask = WriteMask;

	_depthDesc.FrontFace.StencilFailOp = (ERHI_STENCIL_OP)Fail;
	_depthDesc.FrontFace.StencilDepthFailOp = (ERHI_STENCIL_OP)ZFail;
	_depthDesc.FrontFace.StencilPassOp = (ERHI_STENCIL_OP)Pass;
	_depthDesc.FrontFace.StencilFunc = (ERHI_COMPARISON)Func;

	_depthDesc.BackFace.StencilFailOp = (ERHI_STENCIL_OP)Fail;
	_depthDesc.BackFace.StencilDepthFailOp = (ERHI_STENCIL_OP)ZFail;
	_depthDesc.BackFace.StencilPassOp = (ERHI_STENCIL_OP)Pass;
	_depthDesc.BackFace.StencilFunc = (ERHI_COMPARISON)Func;

	SetStencilRef(Ref);
}

void DX12StateManager::SetRenderState(u32 p1, u32 p2)
{
	_dirty = true;
	switch (p1)
	{
	case D3DRS_ZENABLE:
		SetDepthEnable(p2 ? TRUE : FALSE);
		break;

	case D3DRS_FILLMODE:
		_rasterDesc.FillMode = (ERHI_FILL_MODE)p2;
		break;

	case D3DRS_ZWRITEENABLE:
		_depthDesc.DepthWriteMask = p2 ? RHI_DEPTH_WRITE_MASK_ALL : RHI_DEPTH_WRITE_MASK_ZERO;
		break;

	case D3DRS_ALPHATESTENABLE:

		break;

	case D3DRS_TEXTUREFACTOR:

		break;

	case D3DRS_CULLMODE:
		switch (p2)
		{
		case 1:
			SetCullMode(ERHI_CULLMODE::NONE);
			break;
		case 2:
			SetCullMode(ERHI_CULLMODE::FRONT);
			break;
		case 3:
			SetCullMode(ERHI_CULLMODE::BACK);
			break;
		}
		break;

	case D3DRS_SRCBLEND:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].SrcBlend = (ERHI_BLEND)p2;
		}
		break;

	case D3DRS_DESTBLEND:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].DestBlend = (ERHI_BLEND)p2;
		}
		break;

	case D3DRS_ZFUNC:
		SetDepthFunc(p2);
		break;

	case D3DRS_ALPHAREF:
		SetAlphaRef(p2);
		break;

	case D3DRS_ALPHAFUNC:
		
		break;

	case D3DRS_ALPHABLENDENABLE:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].BlendEnable = (BOOL)p2;
		}
		break;

	case D3DRS_STENCILENABLE:
		_depthDesc.StencilEnable = (BOOL)p2;
		break;

	case D3DRS_STENCILFAIL:
		_depthDesc.FrontFace.StencilFailOp = (ERHI_STENCIL_OP)p2;
		_depthDesc.BackFace.StencilFailOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_STENCILZFAIL:
		_depthDesc.FrontFace.StencilDepthFailOp = (ERHI_STENCIL_OP)p2;
		_depthDesc.BackFace.StencilDepthFailOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_STENCILPASS:
		_depthDesc.FrontFace.StencilPassOp = (ERHI_STENCIL_OP)p2;
		_depthDesc.BackFace.StencilPassOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_STENCILFUNC:
		_depthDesc.FrontFace.StencilFunc = (ERHI_COMPARISON)p2;
		_depthDesc.BackFace.StencilFunc = (ERHI_COMPARISON)p2;
		break;

	case D3DRS_CCW_STENCILFAIL:
		_depthDesc.BackFace.StencilFailOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_CCW_STENCILZFAIL:
		_depthDesc.BackFace.StencilDepthFailOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_CCW_STENCILPASS:
		_depthDesc.BackFace.StencilPassOp = (ERHI_STENCIL_OP)p2;
		break;

	case D3DRS_CCW_STENCILFUNC:
		_depthDesc.BackFace.StencilFunc = (ERHI_COMPARISON)p2;
		break;

	case D3DRS_STENCILREF:
		SetStencilRef(p2);
		break;

	case D3DRS_STENCILMASK:
		_depthDesc.StencilReadMask = (UINT8)p2;
		break;

	case D3DRS_STENCILWRITEMASK:
		_depthDesc.StencilWriteMask = (UINT8)p2;
		break;

	case D3DRS_SCISSORTESTENABLE:
		EnableScissoring(p2 ? TRUE : FALSE);
		break;

	case D3DRS_SLOPESCALEDEPTHBIAS:
		_rasterDesc.SlopeScaledDepthBias = *((float*)&p2);
		break;

	case D3DRS_DEPTHBIAS:
		_rasterDesc.DepthBias = (INT)p2;
		break;

	case D3DRS_COLORWRITEENABLE:
		SetColorWriteEnable(p2);
		break;

	case D3DRS_COLORWRITEENABLE1:
	case D3DRS_COLORWRITEENABLE2:
	case D3DRS_COLORWRITEENABLE3:
	{
		int targetIndex = p1 - D3DRS_COLORWRITEENABLE1;
		if (targetIndex >= 0 && targetIndex < 8)
		{
			_blendDesc.RenderTarget[targetIndex].RenderTargetWriteMask = p2;
		}
	}
	break;

	case D3DRS_BLENDOP:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].BlendOp = (ERHI_BLEND_OP)p2;
		}
		break;

	case D3DRS_SRCBLENDALPHA:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].SrcBlendAlpha = (ERHI_BLEND)p2;
		}
		break;

	case D3DRS_DESTBLENDALPHA:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].DestBlendAlpha = (ERHI_BLEND)p2;
		}
		break;

	case D3DRS_BLENDOPALPHA:
		for (u32 target_idx = 0; target_idx < 8; ++target_idx)
		{
			_blendDesc.RenderTarget[target_idx].BlendOpAlpha = (ERHI_BLEND_OP)p2;
		}
		break;

	case D3DRS_SEPARATEALPHABLENDENABLE:
		
		_blendDesc.IndependentBlendEnable = TRUE;
		break;

	case D3DRS_MULTISAMPLEANTIALIAS:
		SetMultisample(p2);
		break;

	case D3DRS_MULTISAMPLEMASK:
		SetSampleMask(p2);
		break;

	case D3DRS_ANTIALIASEDLINEENABLE:
		_rasterDesc.AntialiasedLineEnable = (BOOL)p2;
		break;

	default:
		break;
	}
}

void DX12StateManager::SetDepthFunc(u32 Func)
{
	_dirty = true;
	_depthDesc.DepthFunc = (ERHI_COMPARISON)Func;
}

void DX12StateManager::SetDepthEnable(u32 Enable)
{
	_dirty = true;
	_depthDesc.DepthEnable = Enable;
}

void DX12StateManager::SetColorWriteEnable(u32 WriteMask)
{
	_dirty = true;
	for (u32 target_idx = 0; target_idx < 8; ++target_idx)
	{
		_blendDesc.RenderTarget[target_idx].RenderTargetWriteMask = WriteMask;
	}
}

void DX12StateManager::SetCullMode(ERHI_CULLMODE Mode)
{
	_dirty = true;
	_rasterDesc.CullMode = (ERHI_CULL_MODE)Mode;
	CacheCullMode = Mode;
}

void DX12StateManager::BindAlphaRefCallback(const BindAlphaCallbackDecl& Callback)
{
	BindAlphaCallback = Callback;

	if (BindAlphaCallback)
	{
		BindAlphaCallback(_alphaRef);
	}
}

void DX12StateManager::SetMultisample(u32 Enable)
{
	_dirty = true;
	_rasterDesc.MultisampleEnable = Enable;
}

template <typename TDesc>
static void* CacheState(const TDesc& desc, xr_vector<TDesc*>& cache)
{
    for (auto state : cache)
    {
        if (!memcmp(state, &desc, sizeof(desc)))
        {
            return state;
        }
    }
    auto state = new TDesc(desc);
    cache.push_back(state);
    return state;
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
            for (u32 slot_idx = 0; slot_idx < 16; ++slot_idx)
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

class DX12ShaderDeclaration final : public IRHIShaderDeclaration
{
public:
    DX12ShaderDeclaration(const RHIInputElementDesc* desc, size_t count) : IRHIShaderDeclaration(desc, count) {}
    ~DX12ShaderDeclaration() override
    {
        if (_layout)
        {
            _layout->Release();
        }
    }
    void GenerateLayerDescriptors(RHIBlob* signature) override
    {
        if (!_layout)
        {
            R_CHK(GRHI->CreateInputLayout(Descriptors.data(), Descriptors.size(), signature->GetBufferPointer(),
                signature->GetBufferSize(), &_layout));
        }
    }
    void ApplyLayout() override { GRHI->SetInputLayout(_layout); }

private:
    RHIObject* _layout = nullptr;
};

IRHIShaderDeclaration* InternalDevice12::CreateDecl(const RHIInputElementDesc* desc, size_t count)
{
    return new DX12ShaderDeclaration(desc, count);
}
