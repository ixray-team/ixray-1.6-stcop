#include "Device.h"
#include <d3dcompiler.h>
#include <d3d11shader.h>
#include "../RHIDXC.h"

template <typename T>
static void DeletePayload(void* payload)
{
    auto value = static_cast<T*>(payload);
    xr_delete(value);
}

static void DeleteSampler(void* payload)
{
    auto sampler = static_cast<DX12Sampler*>(payload);
    sampler->Device->Retire(sampler->Descriptor);
    xr_delete(sampler);
}

static void DescribeSampler(void* payload, RHISampleDesc* out_desc)
{
    *out_desc = static_cast<DX12Sampler*>(payload)->Desc;
}

static void DeleteQuery(void* payload)
{
    auto query = static_cast<DX12Query*>(payload);
    query->Device->RecycleQuery(*query);
    xr_delete(query);
}

template <class TReflection, class TShader, class TBinding>
static HRESULT ReflectBindings(TReflection* reflection, DX12Shader* shader)
{
    TShader desc = {};
    reflection->GetDesc(&desc);
    for (u32 resource_idx = 0; resource_idx < desc.BoundResources; ++resource_idx)
    {
        TBinding binding = {};
        const HRESULT bindingResult = reflection->GetResourceBindingDesc(resource_idx, &binding);
        if (FAILED(bindingResult))
        {
            return bindingResult;
        }
        const u8 kind = binding.Type == D3D_SIT_STRUCTURED || binding.Type == D3D_SIT_UAV_RWSTRUCTURED ||
            binding.Type == D3D_SIT_UAV_APPEND_STRUCTURED || binding.Type == D3D_SIT_UAV_CONSUME_STRUCTURED ||
            binding.Type == D3D_SIT_UAV_RWSTRUCTURED_WITH_COUNTER ? DX12KindStructured :
            binding.Type == D3D_SIT_BYTEADDRESS || binding.Type == D3D_SIT_UAV_RWBYTEADDRESS ? DX12KindRaw : DX12KindTyped;
        if (binding.Type == D3D_SIT_CBUFFER || binding.Type == D3D_SIT_SAMPLER)
        {
            auto& mask = binding.Type == D3D_SIT_CBUFFER ? shader->ConstantMask : shader->SamplerMask;
            const u32 limit = binding.Type == D3D_SIT_CBUFFER ? 14u : 16u;
            for (u32 binding_idx = binding.BindPoint;
                binding_idx < limit && binding_idx - binding.BindPoint < binding.BindCount; ++binding_idx)
            {
                mask |= 1u << binding_idx;
            }
            continue;
        }
        const bool isUAV = binding.Type == D3D_SIT_UAV_RWTYPED || binding.Type == D3D_SIT_UAV_RWSTRUCTURED ||
            binding.Type == D3D_SIT_UAV_RWBYTEADDRESS || binding.Type == D3D_SIT_UAV_APPEND_STRUCTURED ||
            binding.Type == D3D_SIT_UAV_CONSUME_STRUCTURED || binding.Type == D3D_SIT_UAV_RWSTRUCTURED_WITH_COUNTER;
        if (isUAV)
        {
            for (u32 binding_idx = binding.BindPoint; binding_idx < std::min(8u, binding.BindPoint + binding.BindCount); ++binding_idx)
            {
                shader->UAVMask |= 1u << binding_idx;
                shader->UAVKinds[binding_idx] = kind;
                switch (binding.Dimension)
                {
                case D3D_SRV_DIMENSION_BUFFER: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_BUFFER; break;
                case D3D_SRV_DIMENSION_TEXTURE1D: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_TEXTURE1D; break;
                case D3D_SRV_DIMENSION_TEXTURE1DARRAY: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_TEXTURE1DARRAY; break;
                case D3D_SRV_DIMENSION_TEXTURE2DARRAY: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_TEXTURE2DARRAY; break;
                case D3D_SRV_DIMENSION_TEXTURE3D: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_TEXTURE3D; break;
                default: shader->UAVDimensions[binding_idx] = D3D12_UAV_DIMENSION_TEXTURE2D; break;
                }
            }
        }
        else if (binding.Type != D3D_SIT_CBUFFER && binding.Type != D3D_SIT_SAMPLER)
        {
            for (u32 binding_idx = binding.BindPoint; binding_idx < std::min(16u, binding.BindPoint + binding.BindCount); ++binding_idx)
            {
                shader->Dimensions[binding_idx] = binding.Dimension == D3D_SRV_DIMENSION_BUFFEREX ?
                    D3D12_SRV_DIMENSION_BUFFER : (D3D12_SRV_DIMENSION)binding.Dimension;
                shader->ReturnTypes[binding_idx] = binding.ReturnType;
                shader->ResourceKinds[binding_idx] = kind;
            }
        }
    }
    return S_OK;
}

HRESULT InternalDevice12::CreateShader(const void* code, size_t size, ERHI_SHADER_TYPE type, RHIObject** out_shader)
{
    ContextLock guard(*this);
    *out_shader = nullptr;
    if (!code || !size || (u32)type >= 6)
    {
        return E_INVALIDARG;
    }
    auto shader = new DX12Shader;
    shader->Id = _nextObject++;
    shader->Code.assign((const u8*)code, (const u8*)code + size);
    shader->ConstantMask = 0x3FFFu;
    shader->SamplerMask = 0xFFFFu;
    shader->UAVMask = 0xFFu;
    for (u32 slot = 0; slot < 16; ++slot)
    {
        shader->Dimensions[slot] = D3D12_SRV_DIMENSION_TEXTURE2D;
        shader->ReturnTypes[slot] = D3D_RETURN_TYPE_FLOAT;
    }
    for (u32 slot = 0; slot < 8; ++slot)
        shader->UAVDimensions[slot] = D3D12_UAV_DIMENSION_TEXTURE2D;
    *out_shader = new RHIObject(shader, DeletePayload<DX12Shader>);
    return S_OK;
}

void InternalDevice12::DropShaderPipelines(u64 shaderId)
{
    if (!shaderId)
    {
        return;
    }
    xr_vector<GraphicsPipeline> graphics;
    graphics.reserve(_graphicsPipelines.size());
    for (auto& pipeline : _graphicsPipelines)
    {
        bool usesShader = false;
        for (u64 id : pipeline.Key.ShaderIds)
        {
            usesShader |= id == shaderId;
        }
        if (usesShader)
        {
            pipeline.Pipeline->Release();
        }
        else
        {
            graphics.push_back(pipeline);
        }
    }
    _graphicsPipelines.swap(graphics);
    ZeroMemory(_pipelineBuckets, sizeof(_pipelineBuckets));
    for (u32 pipeline_idx = 0; pipeline_idx < _graphicsPipelines.size(); ++pipeline_idx)
    {
        const u64 pipelineHash = crc32(&_graphicsPipelines[pipeline_idx].Key, sizeof(GraphicsPipelineKey));
        _graphicsPipelines[pipeline_idx].Next = _pipelineBuckets[pipelineHash & 255];
        _pipelineBuckets[pipelineHash & 255] = pipeline_idx + 1;
    }

    xr_vector<ComputePipeline> compute;
    compute.reserve(_computePipelines.size());
    for (auto& pipeline : _computePipelines)
    {
        if (pipeline.ShaderId == shaderId)
        {
            pipeline.Pipeline->Release();
        }
        else
        {
            compute.push_back(pipeline);
        }
    }
    _computePipelines.swap(compute);
    _boundPipeline = nullptr;
    _boundPipelineHash = 0;
    _boundGraphicsPipeline = false;
    _pipelineDirty = true;
}

HRESULT InternalDevice12::ReplaceShader(RHIObject* shader, const void* code, size_t size)
{
    ContextLock guard(*this);
    if (!shader || !shader->resource || !code || !size)
    {
        return E_INVALIDARG;
    }
    auto native = static_cast<DX12Shader*>(shader->resource);
    const u64 previous = native->Id;
    native->Code.assign((const u8*)code, (const u8*)code + size);
    native->Id = _nextObject++;
    native->ConstantMask = 0x3FFFu;
    native->SamplerMask = 0xFFFFu;
    native->UAVMask = 0xFFu;
    for (u32 slot = 0; slot < 16; ++slot)
    {
        native->Dimensions[slot] = D3D12_SRV_DIMENSION_TEXTURE2D;
        native->ReturnTypes[slot] = D3D_RETURN_TYPE_FLOAT;
    }
    for (u32 slot = 0; slot < 8; ++slot)
    {
        native->UAVDimensions[slot] = D3D12_UAV_DIMENSION_TEXTURE2D;
    }
    for (u32 stage = 0; stage < 5; ++stage)
    {
        if (_graphicsState.Shaders[stage] == native)
        {
            _graphicsState.ShaderIds[stage] = native->Id;
        }
    }
    DropShaderPipelines(previous);
    return S_OK;
}

HRESULT InternalDevice12::CreateInputLayout(const RHIInputElementDesc* elements, size_t count, const void* code,
    size_t size, RHIObject** out_layout)
{
    ContextLock guard(*this);
    *out_layout = nullptr;
    if ((!elements && count) || count > 32 || !code || !size)
    {
        return E_INVALIDARG;
    }
    auto layout = new DX12InputLayout;
    layout->Id = _nextObject++;
    layout->Elements.resize(count);
    layout->Semantics.resize(count);
    for (u32 element_idx = 0; element_idx < count; ++element_idx)
    {
        const auto& source = elements[element_idx];
        layout->Semantics[element_idx] = source.SemanticName;
        layout->Elements[element_idx] = { layout->Semantics[element_idx].c_str(), source.SemanticIndex, (DXGI_FORMAT)source.Format,
            source.InputSlot, source.AlignedByteOffset, (D3D12_INPUT_CLASSIFICATION)source.InputSlotClass, source.InstanceDataStepRate };
    }
    *out_layout = new RHIObject(layout, DeletePayload<DX12InputLayout>);
    return S_OK;
}

void InternalDevice12::SetInputLayout(RHIObject* layout)
{
    ContextLock guard(*this);
    auto native = layout ? static_cast<DX12InputLayout*>(layout->resource) : nullptr;
    if (_layout != native)
    {
        _layout = native;
        _pipelineDirty = true;
    }
}

void InternalDevice12::SetShader(RHIObject* shader, ERHI_SHADER_TYPE type)
{
    ContextLock guard(*this);
    R_ASSERT((u32)type < 6);
    auto native = shader ? static_cast<DX12Shader*>(shader->resource) : nullptr;
    if (type == ERHI_SHADER_TYPE::CS)
    {
        _computeShader = native;
    }
    else if (const u64 id = native ? native->Id : 0; _graphicsState.Shaders[(u32)type] != native || _graphicsState.ShaderIds[(u32)type] != id)
    {
        _graphicsState.Shaders[(u32)type] = native;
        _graphicsState.ShaderIds[(u32)type] = id;
        _pipelineDirty = true;
    }
}

HRESULT InternalDevice12::CreateSamplerState(const RHISampleDesc& desc, RHIObject** out_state)
{
    ContextLock guard(*this);
    auto sampler = new DX12Sampler;
    sampler->Device = this;
    sampler->Desc = desc;
    sampler->Descriptor = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER);
    D3D12_SAMPLER_DESC native = { (D3D12_FILTER)desc.Filter, (D3D12_TEXTURE_ADDRESS_MODE)desc.AddressU,
        (D3D12_TEXTURE_ADDRESS_MODE)desc.AddressV, (D3D12_TEXTURE_ADDRESS_MODE)desc.AddressW, desc.MipLODBias,
        desc.MaxAnisotropy, (D3D12_COMPARISON_FUNC)desc.ComparisonFunc,
        { desc.BorderColor[0], desc.BorderColor[1], desc.BorderColor[2], desc.BorderColor[3] }, desc.MinLOD, desc.MaxLOD };
    GetDevice()->CreateSampler(&native, sampler->Descriptor.Cpu);
    _counters.SamplerCreates.fetch_add(1, std::memory_order_relaxed);
    *out_state = new RHIObject(sampler, DeleteSampler, DescribeSampler);
    return S_OK;
}

void InternalDevice12::SetSamplers(u32 start, u32 count, RHIObject* const* states, ERHI_SHADER_TYPE type)
{
    ContextLock guard(*this);
    R_ASSERT((u32)type < 6 && start <= 16 && count <= 16 - start);
    for (u32 sampler_idx = 0; sampler_idx < count; ++sampler_idx)
    {
        auto native = states[sampler_idx] ? static_cast<DX12Sampler*>(states[sampler_idx]->resource) : nullptr;
        auto& slot = _samplers[(u32)type][start + sampler_idx];
        _samplersDirty[(u32)type] |= slot != native;
        slot = native;
    }
}

void InternalDevice12::SetConstantBuffers(u32 start, u32 count, IRHIBuffer* const* buffers, ERHI_SHADER_TYPE type)
{
    ContextLock guard(*this);
    R_ASSERT((u32)type < 6 && start <= 14 && count <= 14 - start);
    for (u32 buffer_idx = 0; buffer_idx < count; ++buffer_idx)
    {
        _constants[(u32)type][start + buffer_idx] = static_cast<DX12Buffer*>(buffers[buffer_idx]);
    }
}

void InternalDevice12::BindResource(u32 stage, u32 slot, IRHIShaderResourceView* resource)
{
    ContextLock guard(*this);
    R_ASSERT(stage < 6 && slot < 16);
    auto native = static_cast<DX12ShaderResourceView*>(resource);
    _viewsDirty[stage] |= _resources[stage][slot] != native;
    _resources[stage][slot] = native;
}

void InternalDevice12::SetComputeResources(u32 start, u32 count, IRHIShaderResourceView* const* views)
{
    ContextLock guard(*this);
    R_ASSERT(start <= 16 && count <= 16 - start);
    for (u32 view_idx = 0; view_idx < count; ++view_idx)
    {
        BindResource(5, start + view_idx, views[view_idx]);
    }
}

void InternalDevice12::SetComputeUAVs(u32 start, u32 count, IRHIUnorderedAccessView* const* views, const u32* initialCounts)
{
    ContextLock guard(*this);
    R_ASSERT(start <= 8 && count <= 8 - start);
    if (initialCounts)
    {
        for (u32 count_idx = 0; count_idx < count; ++count_idx)
        {
            R_ASSERT2(initialCounts[count_idx] == 0 || initialCounts[count_idx] == UINT32_MAX, "D3D12 UAV counters are not supported");
        }
    }
    for (u32 view_idx = 0; view_idx < count; ++view_idx)
    {
        _computeUAVs[start + view_idx] = static_cast<DX12UnorderedAccessView*>(views[view_idx]);
    }
}

void InternalDevice12::BindVertexBuffer(DX12Buffer* buffer, u32 slot, u32 stride, u32 offset)
{
    ContextLock guard(*this);
    R_ASSERT(slot < 32 && (!buffer || offset <= buffer->GetSize()));
    _vertices[slot] = { buffer, stride, offset };
    _vertexMask = buffer ? _vertexMask | (1u << slot) : _vertexMask & ~(1u << slot);
}

void InternalDevice12::BindIndexBuffer(DX12Buffer* buffer, bool is32Bit, u32 offset)
{
    ContextLock guard(*this);
    R_ASSERT(!buffer || offset <= buffer->GetSize());
    _index = buffer;
    _index32 = is32Bit;
    _indexOffset = offset;
}

void InternalDevice12::ClearVertexBuffer(u32 stride)
{
    ContextLock guard(*this);
    BindVertexBuffer(nullptr, 0, stride, 0);
}

void InternalDevice12::ClearIndexBuffer()
{
    ContextLock guard(*this);
    BindIndexBuffer(nullptr, false, 0);
}

void InternalDevice12::RecycleQuery(DX12Query& query)
{
    ContextLock guard(*this);
    const u64 fence = query.Fence ? query.Fence : _isRecording ? _nextFence : _nextFence - 1;
    _occlusionFence[query.Slot] = fence;
    _occlusionFree[_occlusionFreeCount++] = query.Slot;
}

HRESULT InternalDevice12::CreateOcclusionQuery(RHIObject** out_query)
{
    ContextLock guard(*this);
    *out_query = nullptr;
    u32 slot = UINT32_MAX;
    for (u32 attempt = 0; attempt < 2 && slot == UINT32_MAX; ++attempt)
    {
        for (u32 free_idx = 0; free_idx < _occlusionFreeCount; ++free_idx)
        {
            const u32 candidate = _occlusionFree[free_idx];
            if (_occlusionFence[candidate] && !IsComplete(_occlusionFence[candidate]))
            {
                continue;
            }
            _occlusionFree[free_idx] = _occlusionFree[--_occlusionFreeCount];
            slot = candidate;
            break;
        }
        if (slot == UINT32_MAX && !attempt)
        {
            Flush();
        }
    }
    if (slot == UINT32_MAX)
    {
        return E_FAIL;
    }
    auto query = new DX12Query;
    query->Device = this;
    query->Slot = slot;
    *out_query = new RHIObject(query, DeleteQuery);
    return S_OK;
}

void InternalDevice12::BeginQuery(RHIObject* object)
{
    ContextLock guard(*this);
    auto query = static_cast<DX12Query*>(object->resource);
    Commands()->BeginQuery(_occlusionHeap, D3D12_QUERY_TYPE_OCCLUSION, query->Slot);
    query->IsPending = false;
}

void InternalDevice12::EndQuery(RHIObject* object)
{
    ContextLock guard(*this);
    auto query = static_cast<DX12Query*>(object->resource);
    Commands()->EndQuery(_occlusionHeap, D3D12_QUERY_TYPE_OCCLUSION, query->Slot);
    Commands()->ResolveQueryData(_occlusionHeap, D3D12_QUERY_TYPE_OCCLUSION, query->Slot, 1, _occlusionReadback,
        u64(query->Slot) * sizeof(u64));
    query->Fence = _nextFence;
    query->IsPending = true;
}

HRESULT InternalDevice12::GetQueryData(RHIObject* object, void* data, u32 size, u32 flags)
{
    ContextLock guard(*this);
    if (!object || !object->resource)
    {
        return E_INVALIDARG;
    }
    auto query = static_cast<DX12Query*>(object->resource);
    if (!query->IsPending || size != sizeof(u64) || !data)
    {
        return E_INVALIDARG;
    }
    (void)flags;
    if (!IsComplete(query->Fence))
    {
        // The frame owns one command allocator. Submitting here makes the next
        // Commands() wait until the GPU finishes the first half of the frame.
        return S_FALSE;
    }
    void* mapped = nullptr;
    const u64 offset = u64(query->Slot) * sizeof(u64);
    D3D12_RANGE range = { SIZE_T(offset), SIZE_T(offset + sizeof(u64)) };
    const HRESULT result = _occlusionReadback->Map(0, &range, &mapped);
    if (SUCCEEDED(result))
    {
        memcpy(data, (u8*)mapped + offset, sizeof(u64));
        D3D12_RANGE written = {};
        _occlusionReadback->Unmap(0, &written);
    }
    return result;
}

u32 InternalDevice12::RootConstantParameter(bool compute, u32 stage, u32 slot) const
{
    return (compute ? ComputeTableCount : GraphicsTableCount) + (compute ? 0u : stage) * RootConstantSlots + slot;
}

void InternalDevice12::CreateRootSignatures()
{
    auto create = [&](bool compute, ID3D12RootSignature** out_signature)
    {
        const u32 stages = compute ? 1u : 5u;
        const D3D12_SHADER_VISIBILITY visibility[5] = { D3D12_SHADER_VISIBILITY_PIXEL, D3D12_SHADER_VISIBILITY_VERTEX,
            D3D12_SHADER_VISIBILITY_GEOMETRY, D3D12_SHADER_VISIBILITY_HULL, D3D12_SHADER_VISIBILITY_DOMAIN };
        D3D12_DESCRIPTOR_RANGE ranges[GraphicsTableCount] = {};
        D3D12_ROOT_PARAMETER parameters[GraphicsTableCount + 5 * RootConstantSlots] = {};
        for (u32 stage_idx = 0; stage_idx < stages; ++stage_idx)
        {
            const u32 base = stage_idx * TablesPerStage;
            const auto stageVisibility = compute ? D3D12_SHADER_VISIBILITY_ALL : visibility[stage_idx];
            ranges[base] = { D3D12_DESCRIPTOR_RANGE_TYPE_CBV, TableConstantCount, RootConstantSlots, 0, 0 };
            ranges[base + 1] = { D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 16, 0, 0, 0 };
            ranges[base + 2] = { D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER, 16, 0, 0, 0 };
            for (u32 table_idx = 0; table_idx < TablesPerStage; ++table_idx)
            {
                auto& parameter = parameters[base + table_idx];
                parameter.ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
                parameter.DescriptorTable = { 1, &ranges[base + table_idx] };
                parameter.ShaderVisibility = stageVisibility;
            }
            for (u32 slot_idx = 0; slot_idx < RootConstantSlots; ++slot_idx)
            {
                auto& root = parameters[RootConstantParameter(compute, stage_idx, slot_idx)];
                root.ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
                root.Descriptor = { slot_idx, 0 };
                root.ShaderVisibility = stageVisibility;
            }
        }
        const u32 uavParameter = stages * TablesPerStage;
        ranges[uavParameter] = { D3D12_DESCRIPTOR_RANGE_TYPE_UAV, 8, 0, 0, 0 };
        parameters[uavParameter].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
        parameters[uavParameter].DescriptorTable = { 1, &ranges[uavParameter] };
        parameters[uavParameter].ShaderVisibility = compute ? D3D12_SHADER_VISIBILITY_ALL : D3D12_SHADER_VISIBILITY_PIXEL;
        D3D12_ROOT_SIGNATURE_DESC desc = {};
        desc.NumParameters = uavParameter + 1 + stages * RootConstantSlots;
        desc.pParameters = parameters;
        desc.Flags = compute ? D3D12_ROOT_SIGNATURE_FLAG_NONE : D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ID3DBlob* blob = nullptr;
        ID3DBlob* errors = nullptr;
        const HRESULT result = D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1, &blob, &errors);
        if (errors)
        {
            Msg("! D3D12 root signature: %s", (const char*)errors->GetBufferPointer());
            errors->Release();
        }
        R_CHK(result);
        R_CHK(GetDevice()->CreateRootSignature(0, blob->GetBufferPointer(), blob->GetBufferSize(), IID_PPV_ARGS(out_signature)));
        blob->Release();
    };
    create(false, &_graphicsRoot);
    create(true, &_computeRoot);
}

static u32 HashKeys(const u64* keys, u32 count)
{
    u64 hash = 0x9E3779B97F4A7C15ull ^ count;
    for (u32 key_idx = 0; key_idx < count; ++key_idx)
    {
        hash = (hash ^ keys[key_idx]) * 0xFF51AFD7ED558CCDull;
        hash ^= hash >> 32;
    }
    return u32(hash ^ (hash >> 29));
}

D3D12_GPU_DESCRIPTOR_HANDLE InternalDevice12::FindTable(const DescriptorTable& table)
{
    const auto& frame = _frames[_frame];
    const u32 bucket_idx = HashKeys(table.Keys, table.Count) % u32(std::size(frame.TableBuckets));
    for (u32 table_idx = frame.TableBuckets[bucket_idx]; table_idx; table_idx = frame.Tables[table_idx - 1].Next)
    {
        const auto& cached = frame.Tables[table_idx - 1];
        if (cached.Count == table.Count && cached.IsSampler == table.IsSampler &&
            !memcmp(cached.Keys, table.Keys, table.Count * sizeof(table.Keys[0])))
        {
            _counters.TableHits.fetch_add(1, std::memory_order_relaxed);
            return cached.Handle;
        }
    }
    _counters.TableMisses.fetch_add(1, std::memory_order_relaxed);
    return {};
}

void InternalDevice12::CacheTable(DescriptorTable& table)
{
    auto& frame = _frames[_frame];
    const u32 bucket_idx = HashKeys(table.Keys, table.Count) % u32(std::size(frame.TableBuckets));
    table.Next = frame.TableBuckets[bucket_idx];
    frame.Tables.push_back(table);
    frame.TableBuckets[bucket_idx] = u32(frame.Tables.size());
}

DX12Descriptor InternalDevice12::AllocateTable(u32 count, bool sampler)
{
    Commands();
    Frame& frame = _frames[_frame];
    const u32 start = sampler ? StaticSamplerDescriptors : StaticResourceDescriptors;
    const u32 capacity = ((sampler ? SamplerDescriptorCount : ResourceDescriptorCount) - start) / FrameCount;
    u32& used = sampler ? frame.SamplersUsed : frame.ResourcesUsed;
    R_ASSERT(count <= capacity);
    if (used > capacity - count)
    {
        _counters.DescriptorFlushes.fetch_add(1, std::memory_order_relaxed);
        Flush();
        ++_tableGeneration;
        frame.Tables.clear();
        std::fill(std::begin(frame.TableBuckets), std::end(frame.TableBuckets), 0);
        frame.ResourcesUsed = 0;
        frame.SamplersUsed = 0;
        Commands();
    }
    DX12Descriptor descriptor;
    descriptor.Type = sampler ? D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER : D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    descriptor.Index = start + _frame * capacity + used;
    used += count;
    const u64 offset = u64(descriptor.Index) * _descriptorStride[descriptor.Type];
    descriptor.Cpu = Heap(descriptor.Type)->GetCPUDescriptorHandleForHeapStart();
    descriptor.Cpu.ptr += offset;
    descriptor.Gpu = Heap(descriptor.Type)->GetGPUDescriptorHandleForHeapStart();
    descriptor.Gpu.ptr += offset;
    return descriptor;
}

D3D12_GPU_DESCRIPTOR_HANDLE InternalDevice12::ConstantTable(u32 stage)
{
    auto shader = stage == 5 ? _computeShader : _graphicsState.Shaders[stage];
    auto& cache = _constantCaches[stage];
    u64 keys[TableConstantCount] = {};
    bool same = cache.Handle.ptr && cache.Generation == _tableGeneration;
    for (u32 buffer_idx = 0; buffer_idx < TableConstantCount; ++buffer_idx)
    {
        auto buffer = shader && (shader->ConstantMask & (1u << (buffer_idx + RootConstantSlots))) ?
            _constants[stage][buffer_idx + RootConstantSlots] : nullptr;
        u64 key = 0;
        if (buffer)
        {
            key = buffer->PublishedAddress();
            if (!key)
            {
                key = buffer->GetAddress();
            }
        }
        keys[buffer_idx] = key;
        same &= key == cache.Keys[buffer_idx];
    }
    if (same)
    {
        return cache.Handle;
    }
    DescriptorTable table;
    table.Count = TableConstantCount;
    memcpy(table.Keys, keys, sizeof(keys));
    auto handle = FindTable(table);
    if (!handle.ptr)
    {
        if (!_nullCbv.Cpu.ptr)
        {
            _nullCbv = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
            const D3D12_CONSTANT_BUFFER_VIEW_DESC nullDesc = { 0, 256 };
            GetDevice()->CreateConstantBufferView(&nullDesc, _nullCbv.Cpu);
        }
        const auto descriptor = AllocateTable(table.Count, false);
        table.Handle = descriptor.Gpu;
        const u32 stride = _descriptorStride[D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV];
        D3D12_CPU_DESCRIPTOR_HANDLE sources[TableConstantCount];
        for (u32 buffer_idx = 0; buffer_idx < TableConstantCount; ++buffer_idx)
        {
            sources[buffer_idx] = _nullCbv.Cpu;
        }
        auto destination = descriptor.Cpu;
        const UINT count = TableConstantCount;
        GetDevice()->CopyDescriptors(1, &destination, &count, count, sources, nullptr, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
        u32 created = 0;
        for (u32 buffer_idx = 0; buffer_idx < TableConstantCount; ++buffer_idx)
        {
            if (!keys[buffer_idx])
            {
                continue;
            }
            const D3D12_CONSTANT_BUFFER_VIEW_DESC desc = { keys[buffer_idx],
                (_constants[stage][buffer_idx + RootConstantSlots]->GetSize() + 255u) & ~255u };
            D3D12_CPU_DESCRIPTOR_HANDLE slot = descriptor.Cpu;
            slot.ptr += u64(buffer_idx) * stride;
            GetDevice()->CreateConstantBufferView(&desc, slot);
            ++created;
        }
        _counters.DescriptorCopies.fetch_add(TableConstantCount, std::memory_order_relaxed);
        _counters.CbvCreates.fetch_add(created, std::memory_order_relaxed);
        CacheTable(table);
        handle = table.Handle;
    }
    memcpy(cache.Keys, keys, sizeof(keys));
    cache.Handle = handle;
    cache.Generation = _tableGeneration;
    return handle;
}

D3D12_GPU_DESCRIPTOR_HANDLE InternalDevice12::ViewTable(u32 stage)
{
    auto shader = stage == 5 ? _computeShader : _graphicsState.Shaders[stage];
    auto& cache = _viewCaches[stage];
    const u64 shaderId = shader ? shader->Id : 0;
    if (cache.Handle.ptr && cache.Generation == _tableGeneration && cache.ShaderId == shaderId &&
        cache.Epoch == _barrierEpoch && !_viewsDirty[stage])
    {
        return cache.Handle;
    }
    DescriptorTable table;
    table.Count = 16;
    for (u32 view_idx = 0; view_idx < 16; ++view_idx)
    {
        auto view = shader && shader->Dimensions[view_idx] != D3D12_SRV_DIMENSION_UNKNOWN ?
            _resources[stage][view_idx] : nullptr;
        bool isFeedback = false;
        if (view && view->View.Surface && stage != 5)
        {
            for (auto target : _targets)
            {
                isFeedback |= target && target->View.Surface == view->View.Surface;
            }
            isFeedback |= _depth && _depth->View.Surface == view->View.Surface &&
                !(_depth->View.Flags & D3D12_DSV_FLAG_READ_ONLY_DEPTH);
        }
        if (view && !isFeedback)
        {
            auto state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE;
            if (view->View.Surface)
            {
                if (stage != 5 && _depth && _depth->View.Surface == view->View.Surface)
                {
                    state |= D3D12_RESOURCE_STATE_DEPTH_READ;
                }
                auto& resource = view->View.Surface->GetResource();
                if (!(resource.Uniform && StateCovers(resource.UniformState, state)))
                {
                    TransitionView(*view->View.Surface, state, view->View);
                }
            }
            else
            {
                auto& resource = view->View.Buffer->GetResource();
                if (!(resource.Uniform && StateCovers(resource.UniformState, state)))
                {
                    Transition(resource, state);
                }
            }
            table.Keys[view_idx] = view->View.Descriptor.Generation;
        }
        else
        {
            table.Keys[view_idx] = (1ull << 63) | u64(shader->Dimensions[view_idx]) |
                (u64(shader->ReturnTypes[view_idx]) << 8) | (u64(shader->ResourceKinds[view_idx]) << 16);
        }
    }
    bool same = cache.Handle.ptr && cache.Generation == _tableGeneration && cache.ShaderId == shaderId && cache.Epoch == _barrierEpoch;
    same = same && !memcmp(cache.Keys, table.Keys, sizeof(cache.Keys));
    if (!same)
    {
        auto handle = FindTable(table);
        if (!handle.ptr)
        {
            const auto descriptor = AllocateTable(table.Count, false);
            table.Handle = descriptor.Gpu;
            if (!shader->NullReady)
            {
                for (u32 view_idx = 0; view_idx < 16; ++view_idx)
                {
                    shader->NullSrvs[view_idx] = NullSrv(u32(shader->Dimensions[view_idx]), u32(shader->ReturnTypes[view_idx]),
                        shader->ResourceKinds[view_idx]).Cpu;
                }
                shader->NullReady = true;
            }
            D3D12_CPU_DESCRIPTOR_HANDLE sources[16];
            for (u32 view_idx = 0; view_idx < 16; ++view_idx)
            {
                sources[view_idx] = table.Keys[view_idx] >> 63 ? shader->NullSrvs[view_idx] :
                    _resources[stage][view_idx]->View.Descriptor.Cpu;
            }
            auto destination = descriptor.Cpu;
            const UINT count = 16;
            GetDevice()->CopyDescriptors(1, &destination, &count, count, sources, nullptr, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
            _counters.DescriptorCopies.fetch_add(16, std::memory_order_relaxed);
            CacheTable(table);
            handle = table.Handle;
        }
        memcpy(cache.Keys, table.Keys, sizeof(cache.Keys));
        cache.Handle = handle;
        cache.ShaderId = shaderId;
        cache.Generation = _tableGeneration;
    }
    cache.Epoch = _barrierEpoch;
    _viewsDirty[stage] = false;
    return cache.Handle;
}

D3D12_GPU_DESCRIPTOR_HANDLE InternalDevice12::SamplerTable(u32 stage)
{
    auto shader = stage == 5 ? _computeShader : _graphicsState.Shaders[stage];
    auto& cache = _samplerCaches[stage];
    const u64 shaderId = shader ? shader->Id : 0;
    if (cache.Handle.ptr && cache.Generation == _tableGeneration && cache.ShaderId == shaderId && !_samplersDirty[stage])
    {
        return cache.Handle;
    }
    DescriptorTable table;
    table.Count = 16;
    table.IsSampler = true;
    for (u32 sampler_idx = 0; sampler_idx < 16; ++sampler_idx)
    {
        auto sampler = shader && (shader->SamplerMask & (1u << sampler_idx)) ? _samplers[stage][sampler_idx] : nullptr;
        table.Keys[sampler_idx] = sampler ? sampler->Descriptor.Generation : 0;
    }
    auto handle = FindTable(table);
    if (!handle.ptr)
    {
        const auto descriptor = AllocateTable(16, true);
        table.Handle = descriptor.Gpu;
        D3D12_CPU_DESCRIPTOR_HANDLE sources[16];
        const auto nullSampler = NullSampler();
        for (u32 sampler_idx = 0; sampler_idx < 16; ++sampler_idx)
        {
            auto sampler = table.Keys[sampler_idx] ? _samplers[stage][sampler_idx] : nullptr;
            sources[sampler_idx] = sampler ? sampler->Descriptor.Cpu : nullSampler.Cpu;
        }
        auto destination = descriptor.Cpu;
        const UINT count = 16;
        GetDevice()->CopyDescriptors(1, &destination, &count, 16, sources, nullptr, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER);
        _counters.DescriptorCopies.fetch_add(16, std::memory_order_relaxed);
        CacheTable(table);
        handle = table.Handle;
    }
    cache.Handle = handle;
    cache.ShaderId = shaderId;
    cache.Generation = _tableGeneration;
    _samplersDirty[stage] = false;
    return handle;
}

D3D12_GPU_DESCRIPTOR_HANDLE InternalDevice12::UAVTable(bool compute)
{
    auto& views = compute ? _computeUAVs : _renderUAVs;
    auto shader = compute ? _computeShader : _graphicsState.Shaders[0];
    auto& cache = _uavCaches[compute];
    const u64 shaderId = shader ? shader->Id : 0;
    const bool unused = !shader || !shader->UAVMask;
    if (unused && cache.Handle.ptr && cache.Generation == _tableGeneration && cache.ShaderId == shaderId)
    {
        return cache.Handle;
    }
    DescriptorTable table;
    table.Count = 8;
    for (u32 view_idx = 0; view_idx < 8; ++view_idx)
    {
        auto view = views[view_idx];
        if (view && shader && (shader->UAVMask & (1u << view_idx)))
        {
            if (view->View.Surface)
            {
                TransitionView(*view->View.Surface, D3D12_RESOURCE_STATE_UNORDERED_ACCESS, view->View);
            }
            else
            {
                Transition(view->View.Buffer->GetResource(), D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
            }
            table.Keys[view_idx] = view->View.Descriptor.Generation;
        }
        else
        {
            const u32 dimension = shader ? u32(shader->UAVDimensions[view_idx]) : 0;
            const u32 kind = shader ? shader->UAVKinds[view_idx] : 0;
            table.Keys[view_idx] = (u64(1) << 63) | dimension | (u64(kind) << 8);
        }
    }
    auto handle = FindTable(table);
    if (!handle.ptr)
    {
        const auto descriptor = AllocateTable(8, false);
        table.Handle = descriptor.Gpu;
        D3D12_CPU_DESCRIPTOR_HANDLE sources[8];
        for (u32 view_idx = 0; view_idx < 8; ++view_idx)
        {
            sources[view_idx] = !(table.Keys[view_idx] >> 63) ? views[view_idx]->View.Descriptor.Cpu :
                NullUav(u32(table.Keys[view_idx] & 0xff), u8((table.Keys[view_idx] >> 8) & 0xff)).Cpu;
        }
        auto destination = descriptor.Cpu;
        const UINT count = 8;
        GetDevice()->CopyDescriptors(1, &destination, &count, 8, sources, nullptr, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
        _counters.DescriptorCopies.fetch_add(8, std::memory_order_relaxed);
        CacheTable(table);
        handle = table.Handle;
    }
    if (unused)
    {
        cache.Handle = handle;
        cache.ShaderId = shaderId;
        cache.Generation = _tableGeneration;
    }
    return handle;
}

template <D3D12_PIPELINE_STATE_SUBOBJECT_TYPE TKind, typename TValue>
struct alignas(void*) DX12PipelineSubobject
{
    D3D12_PIPELINE_STATE_SUBOBJECT_TYPE Kind = TKind;
    TValue Value = {};
};

struct DX12GraphicsStream
{
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_ROOT_SIGNATURE, ID3D12RootSignature*> Root;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_VS, D3D12_SHADER_BYTECODE> VS;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_PS, D3D12_SHADER_BYTECODE> PS;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_GS, D3D12_SHADER_BYTECODE> GS;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_HS, D3D12_SHADER_BYTECODE> HS;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DS, D3D12_SHADER_BYTECODE> DS;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_BLEND, D3D12_BLEND_DESC> Blend;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_MASK, UINT> Mask;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RASTERIZER, D3D12_RASTERIZER_DESC> Rasterizer;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL1, D3D12_DEPTH_STENCIL_DESC1> Depth;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_INPUT_LAYOUT, D3D12_INPUT_LAYOUT_DESC> Layout;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_PRIMITIVE_TOPOLOGY, D3D12_PRIMITIVE_TOPOLOGY_TYPE> Topology;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RENDER_TARGET_FORMATS, D3D12_RT_FORMAT_ARRAY> Targets;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL_FORMAT, DXGI_FORMAT> DepthFormat;
    DX12PipelineSubobject<D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_DESC, DXGI_SAMPLE_DESC> Samples;
};

void InternalDevice12::InvalidateBindings()
{
    ContextLock guard(*this);
    _boundHeaps = false;
    _boundGraphicsRoot = false;
    _boundComputeRoot = false;
    _drawBindingsValid = false;
    _boundPipeline = nullptr;
    _boundGraphicsPipeline = false;
    ZeroMemory(_boundGraphicsTables, sizeof(_boundGraphicsTables));
    ZeroMemory(_boundComputeTables, sizeof(_boundComputeTables));
    ZeroMemory(_rootConstants, sizeof(_rootConstants));
    ++_tableGeneration;
}

bool InternalDevice12::PreparePipeline()
{
    GraphicsState state = _graphicsState;
    state.Layout = _layout;
    state.LayoutId = _layout ? _layout->Id : 0;
    state.TargetCount = 0;
    state.Samples = 1;
    ZeroMemory(state.Formats, sizeof(state.Formats));
    D3D12_CPU_DESCRIPTOR_HANDLE targets[8];
    std::fill(std::begin(targets), std::end(targets), _nullTarget.Cpu);
    for (u32 target_idx = 0; target_idx < 8; ++target_idx)
    {
        auto target = _targets[target_idx];
        if (!target)
        {
            continue;
        }
        state.TargetCount = target_idx + 1;
        state.Formats[target_idx] = target->View.Format;
        state.Samples = target->View.Surface->GetSampleDescCount();
        targets[target_idx] = target->View.Descriptor.Cpu;
        auto& targetResource = target->View.Surface->GetResource();
        if (!(targetResource.Uniform && StateCovers(targetResource.UniformState, D3D12_RESOURCE_STATE_RENDER_TARGET)))
        {
            TransitionView(*target->View.Surface, D3D12_RESOURCE_STATE_RENDER_TARGET, target->View);
        }
    }
    state.DepthFormat = _depth ? _depth->View.Format : DXGI_FORMAT_UNKNOWN;
    if (_depth)
    {
        state.Samples = _depth->View.Surface->GetSampleDescCount();
        for (u32 plane = 0; plane < _depth->View.Planes; ++plane)
        {
            const u32 viewPlane = _depth->View.Plane + plane;
            D3D12_RESOURCE_STATES required = (_depth->View.Flags & (1u << viewPlane)) ?
                D3D12_RESOURCE_STATE_DEPTH_READ : D3D12_RESOURCE_STATE_DEPTH_WRITE;
            if (required == D3D12_RESOURCE_STATE_DEPTH_READ)
            {
                for (u32 stage_idx = 0; stage_idx < 5; ++stage_idx)
                {
                    for (auto view : _resources[stage_idx])
                    {
                        if (view && view->View.Surface == _depth->View.Surface && view->View.Plane == viewPlane)
                        {
                            required |= D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE;
                        }
                    }
                }
            }
            auto& depthResource = _depth->View.Surface->GetResource();
            if (!(depthResource.Uniform && StateCovers(depthResource.UniformState, required)))
            {
                DX12View range = _depth->View;
                range.Plane = viewPlane;
                range.Planes = 1;
                TransitionView(*_depth->View.Surface, required, range);
            }
        }
    }
    else
    {
        state.Depth.DepthEnable = FALSE;
        state.Depth.StencilEnable = FALSE;
    }
    switch (_topology)
    {
    case ERHI_PRIMITIVE_TOPOLOGY::POINT_LIST: state.Topology = D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT; break;
    case ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST:
    case ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP: state.Topology = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE; break;
    case ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH: state.Topology = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH; break;
    default: state.Topology = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE; break;
    }
    GraphicsPipelineKey key = {};
    ZeroMemory(&key, sizeof(key));
    memcpy(key.ShaderIds, state.ShaderIds, sizeof(key.ShaderIds));
    key.LayoutId = state.LayoutId;
    key.Rasterizer = state.Rasterizer;
    key.Depth = state.Depth;
    key.Blend = state.Blend;
    memcpy(key.Formats, state.Formats, sizeof(key.Formats));
    key.DepthFormat = state.DepthFormat;
    key.TargetCount = state.TargetCount;
    key.Samples = state.Samples;
    key.SampleMask = state.SampleMask;
    key.Topology = u32(state.Topology);
    key.DepthBounds = state.DepthBounds ? 1u : 0u;
    const u64 pipelineHash = crc32(&key, sizeof(key));
    ID3D12PipelineState* pipeline = nullptr;
    if (_boundGraphicsPipeline && _boundPipeline && _boundPipelineHash == pipelineHash && !memcmp(&_boundKey, &key, sizeof(key)))
    {
        pipeline = _boundPipeline;
        _counters.PipelineHits.fetch_add(1, std::memory_order_relaxed);
    }
    else
    {
        for (u32 pipeline_idx = _pipelineBuckets[pipelineHash & 255]; pipeline_idx; pipeline_idx = _graphicsPipelines[pipeline_idx - 1].Next)
        {
            if (!memcmp(&_graphicsPipelines[pipeline_idx - 1].Key, &key, sizeof(key)))
            {
                pipeline = _graphicsPipelines[pipeline_idx - 1].Pipeline;
                _counters.PipelineHits.fetch_add(1, std::memory_order_relaxed);
                break;
            }
        }
    }
    if (!pipeline)
    {
        PROF_EVENT("D3D12: CreateGraphicsPipeline");
        D3D12_GRAPHICS_PIPELINE_STATE_DESC desc = {};
        desc.pRootSignature = _graphicsRoot;
        D3D12_SHADER_BYTECODE* shaders[] = { &desc.PS, &desc.VS, &desc.GS, &desc.HS, &desc.DS };
        R_ASSERT(state.Shaders[1]);
        for (u32 shader_idx = 0; shader_idx < 5; ++shader_idx)
        {
            if (state.Shaders[shader_idx])
            {
                *shaders[shader_idx] = { state.Shaders[shader_idx]->Code.data(), state.Shaders[shader_idx]->Code.size() };
            }
        }
        desc.BlendState = state.Blend;
        desc.RasterizerState = state.Rasterizer;
        desc.DepthStencilState = state.Depth;
        desc.SampleMask = state.SampleMask;
        desc.InputLayout = state.Layout ? D3D12_INPUT_LAYOUT_DESC{ state.Layout->Elements.data(), (UINT)state.Layout->Elements.size() } :
            D3D12_INPUT_LAYOUT_DESC{};
        desc.PrimitiveTopologyType = state.Topology;
        desc.NumRenderTargets = state.TargetCount;
        memcpy(desc.RTVFormats, state.Formats, sizeof(desc.RTVFormats));
        desc.DSVFormat = state.DepthFormat;
        desc.SampleDesc.Count = state.Samples;
        HRESULT result = S_OK;
        if (state.DepthBounds)
        {
            DX12GraphicsStream stream;
            stream.Root.Value = desc.pRootSignature;
            stream.VS.Value = desc.VS;
            stream.PS.Value = desc.PS;
            stream.GS.Value = desc.GS;
            stream.HS.Value = desc.HS;
            stream.DS.Value = desc.DS;
            stream.Blend.Value = desc.BlendState;
            stream.Mask.Value = desc.SampleMask;
            stream.Rasterizer.Value = desc.RasterizerState;
            const auto& depth = desc.DepthStencilState;
            stream.Depth.Value = { depth.DepthEnable, depth.DepthWriteMask, depth.DepthFunc, depth.StencilEnable,
                depth.StencilReadMask, depth.StencilWriteMask, depth.FrontFace, depth.BackFace, TRUE };
            stream.Layout.Value = desc.InputLayout;
            stream.Topology.Value = desc.PrimitiveTopologyType;
            stream.Targets.Value.NumRenderTargets = desc.NumRenderTargets;
            memcpy(stream.Targets.Value.RTFormats, desc.RTVFormats, sizeof(desc.RTVFormats));
            stream.DepthFormat.Value = desc.DSVFormat;
            stream.Samples.Value = desc.SampleDesc;
            ID3D12Device2* device = nullptr;
            result = GetDevice()->QueryInterface(IID_PPV_ARGS(&device));
            if (SUCCEEDED(result))
            {
                D3D12_PIPELINE_STATE_STREAM_DESC streamDesc = { sizeof(stream), &stream };
                result = device->CreatePipelineState(&streamDesc, IID_PPV_ARGS(&pipeline));
                device->Release();
            }
        }
        else
        {
            result = GetDevice()->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&pipeline));
        }
        if (FAILED(result))
        {
            Msg("! D3D12 graphics PSO creation failed: 0x%08x, VS %llu, PS %llu, GS %llu, HS %llu, DS %llu, layout %llu",
                u32(result), state.ShaderIds[1], state.ShaderIds[0], state.ShaderIds[2],
                state.ShaderIds[3], state.ShaderIds[4], state.LayoutId);
            Msg("! D3D12 PSO attachments: count %u, formats %u/%u/%u/%u/%u/%u/%u/%u, depth %u, samples %u, topology %u",
                state.TargetCount, u32(state.Formats[0]), u32(state.Formats[1]), u32(state.Formats[2]),
                u32(state.Formats[3]), u32(state.Formats[4]), u32(state.Formats[5]), u32(state.Formats[6]),
                u32(state.Formats[7]), u32(state.DepthFormat), state.Samples, u32(state.Topology));
            if (FAILED(GetDevice()->GetDeviceRemovedReason()))
            {
                ReportDeviceRemoval();
            }
            R_CHK(result);
            return false;
        }
        GraphicsPipeline created;
        created.Key = key;
        created.Next = _pipelineBuckets[pipelineHash & 255];
        created.Pipeline = pipeline;
        _graphicsPipelines.push_back(created);
        _pipelineBuckets[pipelineHash & 255] = u32(_graphicsPipelines.size());
        _counters.PipelineMisses.fetch_add(1, std::memory_order_relaxed);
    }
    if (_boundPipeline != pipeline)
    {
        _commands->SetPipelineState(pipeline);
        _boundPipeline = pipeline;
    }
    _boundGraphicsPipeline = true;
    _boundPipelineHash = pipelineHash;
    _boundKey = key;
    D3D12_CPU_DESCRIPTOR_HANDLE depth = _depth ? _depth->View.Descriptor.Cpu : D3D12_CPU_DESCRIPTOR_HANDLE{};
    auto& bound = _drawBindings;
    if (!_drawBindingsValid || bound.TargetCount != state.TargetCount || bound.Depth.ptr != depth.ptr ||
        memcmp(bound.Targets, targets, sizeof(targets)))
    {
        _commands->OMSetRenderTargets(state.TargetCount, targets, FALSE, _depth ? &depth : nullptr);
        bound.TargetCount = state.TargetCount;
        bound.Depth = depth;
        memcpy(bound.Targets, targets, sizeof(targets));
    }
    _pipelineDirty = false;
    _attachmentEpoch = _barrierEpoch;
    return true;
}

void InternalDevice12::BindNullConstants(bool compute)
{
    if (_nullConstantsEpoch != _epoch)
    {
        const auto upload = AllocateUpload(256);
        memset(upload.Data, 0, 256);
        _nullConstants = upload.Resource->GetGPUVirtualAddress() + upload.Offset;
        _nullConstantsEpoch = _epoch;
    }
    for (u32 stage_idx = 0; stage_idx < (compute ? 1u : 5u); ++stage_idx)
    {
        for (u32 slot_idx = 0; slot_idx < RootConstantSlots; ++slot_idx)
        {
            _rootConstants[compute ? 5 : stage_idx][slot_idx] = _nullConstants;
            if (compute)
            {
                _commands->SetComputeRootConstantBufferView(RootConstantParameter(true, 0, slot_idx), _nullConstants);
            }
            else
            {
                _commands->SetGraphicsRootConstantBufferView(RootConstantParameter(false, stage_idx, slot_idx), _nullConstants);
            }
        }
    }
}

void InternalDevice12::BindRootConstants(bool compute)
{
    for (u32 stage_idx = 0; stage_idx < (compute ? 1u : 5u); ++stage_idx)
    {
        auto shader = compute ? _computeShader : _graphicsState.Shaders[stage_idx];
        if (!shader)
        {
            continue;
        }
        const u32 stage = compute ? 5 : stage_idx;
        for (u32 slot_idx = 0; slot_idx < RootConstantSlots; ++slot_idx)
        {
            if (!(shader->ConstantMask & (1u << slot_idx)))
            {
                continue;
            }
            auto buffer = _constants[stage][slot_idx];
            u64 address = _nullConstants;
            if (buffer)
            {
                address = buffer->PublishedAddress();
                if (!address)
                {
                    address = buffer->GetAddress();
                }
            }
            if (_rootConstants[stage][slot_idx] == address)
            {
                continue;
            }
            _rootConstants[stage][slot_idx] = address;
            if (compute)
            {
                _commands->SetComputeRootConstantBufferView(RootConstantParameter(true, 0, slot_idx), address);
            }
            else
            {
                _commands->SetGraphicsRootConstantBufferView(RootConstantParameter(false, stage_idx, slot_idx), address);
            }
            _counters.RootBinds.fetch_add(1, std::memory_order_relaxed);
        }
    }
}

bool InternalDevice12::PrepareDraw(bool compute)
{
    for (;;)
    {
        Commands();
        const u64 epoch = _epoch;
        D3D12_GPU_DESCRIPTOR_HANDLE tables[GraphicsTableCount] = {};
        if (compute)
        {
            R_ASSERT(_computeShader);
            tables[0] = ConstantTable(5);
            tables[1] = ViewTable(5);
            tables[2] = SamplerTable(5);
            tables[3] = UAVTable(true);
        }
        else
        {
            for (u32 stage_idx = 0; stage_idx < 5; ++stage_idx)
            {
                if (!_graphicsState.Shaders[stage_idx])
                {
                    continue;
                }
                const u32 base = stage_idx * TablesPerStage;
                tables[base] = ConstantTable(stage_idx);
                tables[base + 1] = ViewTable(stage_idx);
                tables[base + 2] = SamplerTable(stage_idx);
            }
            tables[GraphicsTableCount - 1] = UAVTable(false);
        }
        if (epoch != _epoch)
        {
            continue;
        }
        if (!_boundHeaps)
        {
            ID3D12DescriptorHeap* heaps[] = { _resourceHeap, _samplerHeap };
            _commands->SetDescriptorHeaps(2, heaps);
            _boundHeaps = true;
        }
        if (compute && !_boundComputeRoot)
        {
            _commands->SetComputeRootSignature(_computeRoot);
            _boundComputeRoot = true;
            BindNullConstants(true);
        }
        if (!compute && !_boundGraphicsRoot)
        {
            _commands->SetGraphicsRootSignature(_graphicsRoot);
            _boundGraphicsRoot = true;
            BindNullConstants(false);
        }
        BindRootConstants(compute);
        auto bound = compute ? _boundComputeTables : _boundGraphicsTables;
        for (u32 table_idx = 0; table_idx < (compute ? ComputeTableCount : GraphicsTableCount); ++table_idx)
        {
            if (!tables[table_idx].ptr || tables[table_idx].ptr == bound[table_idx].ptr)
            {
                continue;
            }
            if (compute)
            {
                _commands->SetComputeRootDescriptorTable(table_idx, tables[table_idx]);
            }
            else
            {
                _commands->SetGraphicsRootDescriptorTable(table_idx, tables[table_idx]);
            }
            _counters.RootBinds.fetch_add(1, std::memory_order_relaxed);
            bound[table_idx] = tables[table_idx];
        }
        break;
    }
    if (compute)
    {
        ID3D12PipelineState* pipeline = nullptr;
        for (const auto& cached : _computePipelines)
        {
            if (cached.ShaderId == _computeShader->Id)
            {
                pipeline = cached.Pipeline;
                break;
            }
        }
        if (!pipeline)
        {
            PROF_EVENT("D3D12: CreateComputePipeline");
            D3D12_COMPUTE_PIPELINE_STATE_DESC desc = {};
            desc.pRootSignature = _computeRoot;
            desc.CS = { _computeShader->Code.data(), _computeShader->Code.size() };
            const HRESULT result = GetDevice()->CreateComputePipelineState(&desc, IID_PPV_ARGS(&pipeline));
            if (FAILED(result))
            {
                Msg("! D3D12 compute PSO creation failed: 0x%08x, shader %llu", u32(result), _computeShader->Id);
                if (FAILED(GetDevice()->GetDeviceRemovedReason()))
                {
                    ReportDeviceRemoval();
                }
                R_CHK(result);
                return false;
            }
            _computePipelines.push_back({ _computeShader, _computeShader->Id, pipeline });
        }
        if (_boundPipeline != pipeline)
        {
            _commands->SetPipelineState(pipeline);
            _boundPipeline = pipeline;
        }
        _boundGraphicsPipeline = false;
        return true;
    }
    if (_pipelineDirty || !_boundGraphicsPipeline || !_boundPipeline || !_drawBindingsValid || _attachmentEpoch != _barrierEpoch)
    {
        if (!PreparePipeline())
        {
            return false;
        }
    }
    auto& bound = _drawBindings;
    const bool fresh = !_drawBindingsValid;
    const auto topology = (D3D_PRIMITIVE_TOPOLOGY)_topology;
    if (fresh || topology != bound.Topology)
    {
        _commands->IASetPrimitiveTopology(topology);
        bound.Topology = topology;
    }
    const D3D12_VIEWPORT viewport = { _viewport.TopLeftX, _viewport.TopLeftY, _viewport.Width, _viewport.Height, _viewport.MinDepth, _viewport.MaxDepth };
    if (fresh || memcmp(&viewport, &bound.Viewport, sizeof(viewport)))
    {
        _commands->RSSetViewports(1, &viewport);
        bound.Viewport = viewport;
    }
    const D3D12_RECT scissor = _hasScissor && _scissorEnabled ? D3D12_RECT{ _scissor.x1, _scissor.y1, _scissor.x2, _scissor.y2 } :
        D3D12_RECT{ (LONG)viewport.TopLeftX, (LONG)viewport.TopLeftY, (LONG)(viewport.TopLeftX + viewport.Width), (LONG)(viewport.TopLeftY + viewport.Height) };
    if (fresh || memcmp(&scissor, &bound.Scissor, sizeof(scissor)))
    {
        _commands->RSSetScissorRects(1, &scissor);
        bound.Scissor = scissor;
    }
    if (fresh || bound.StencilRef != _stencilRef)
    {
        _commands->OMSetStencilRef(_stencilRef);
        bound.StencilRef = _stencilRef;
    }
    if (fresh || memcmp(bound.BlendFactor, _blendFactor, sizeof(_blendFactor)))
    {
        _commands->OMSetBlendFactor(_blendFactor);
        memcpy(bound.BlendFactor, _blendFactor, sizeof(_blendFactor));
    }
    D3D12_VERTEX_BUFFER_VIEW vertices[32];
    unsigned long last = 0;
    const u32 vertexCount = _BitScanReverse(&last, _vertexMask) ? last + 1 : 0;
    for (u32 slot_idx = 0; slot_idx < vertexCount; ++slot_idx)
    {
        const auto& binding = _vertices[slot_idx];
        auto& view = vertices[slot_idx];
        view = {};
        if (binding.Buffer)
        {
            view.BufferLocation = binding.Buffer->GetAddress() + binding.Offset;
            view.SizeInBytes = binding.Buffer->GetSize() - binding.Offset;
            view.StrideInBytes = binding.Stride;
            if (binding.Buffer->GetResource().Native)
            {
                Transition(binding.Buffer->GetResource(), D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER);
            }
        }
    }
    if (fresh || vertexCount != bound.VertexCount || memcmp(vertices, bound.Vertices, vertexCount * sizeof(vertices[0])))
    {
        if (vertexCount)
        {
            _commands->IASetVertexBuffers(0, vertexCount, vertices);
            memcpy(bound.Vertices, vertices, vertexCount * sizeof(vertices[0]));
        }
        bound.VertexCount = vertexCount;
    }
    D3D12_INDEX_BUFFER_VIEW index = {};
    if (_index)
    {
        index.BufferLocation = _index->GetAddress() + _indexOffset;
        index.SizeInBytes = _index->GetSize() - _indexOffset;
        index.Format = _index32 ? DXGI_FORMAT_R32_UINT : DXGI_FORMAT_R16_UINT;
        if (_index->GetResource().Native)
        {
            Transition(_index->GetResource(), D3D12_RESOURCE_STATE_INDEX_BUFFER);
        }
    }
    if (fresh || index.BufferLocation != bound.Index.BufferLocation ||
        index.SizeInBytes != bound.Index.SizeInBytes || index.Format != bound.Index.Format)
    {
        _commands->IASetIndexBuffer(_index ? &index : nullptr);
        bound.Index = index;
    }
    _drawBindingsValid = true;
    if (_commands1 && _graphicsState.DepthBounds)
    {
        _commands1->OMSetDepthBounds(_depthMinimum, _depthMaximum);
    }
    return true;
}

bool InternalDevice12::SetDepthBounds(bool enable, float minimum, float maximum)
{
    ContextLock guard(*this);
    if (!_canUseDepthBounds || !_commands1)
    {
        return false;
    }
    Commands();
    _graphicsState.DepthBounds = enable;
    _pipelineDirty = true;
    _depthMinimum = minimum;
    _depthMaximum = maximum;
    _commands1->OMSetDepthBounds(minimum, maximum);
    return true;
}
