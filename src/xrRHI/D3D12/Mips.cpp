#include "Device.h"
#include <d3dcompiler.h>

void InternalDevice12::GenerateMips(IRHIShaderResourceView* source)
{
    PROF_EVENT("D3D12: GenerateMips");
    ContextLock guard(*this);
    R_ASSERT(source);
    auto& view = static_cast<DX12ShaderResourceView*>(source)->View;
    R_ASSERT(view.Surface);
    auto& surface = *view.Surface;
    const auto& texture = surface.GetDesc();
    if (texture.MipLevels <= 1)
    {
        return;
    }
    R_ASSERT(texture.SampleDescCount == 1);
    const auto dimension = surface.GetDimension();
    ID3D12PipelineState* pipeline = nullptr;
    for (auto& cached : _mipPipelines)
    {
        if (cached.Format == view.Format && cached.Dimension == dimension)
        {
            pipeline = cached.Pipeline;
            break;
        }
    }
    if (!_mipRoot)
    {
        D3D12_DESCRIPTOR_RANGE range = {};
        range.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
        range.NumDescriptors = 1;
        D3D12_ROOT_PARAMETER parameters[2] = {};
        parameters[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
        parameters[0].DescriptorTable = { 1, &range };
        parameters[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;
        parameters[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        parameters[1].Constants = { 0, 0, 1 };
        parameters[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;
        D3D12_STATIC_SAMPLER_DESC sampler = {};
        sampler.Filter = D3D12_FILTER_MIN_MAG_LINEAR_MIP_POINT;
        sampler.AddressU = sampler.AddressV = sampler.AddressW = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
        sampler.MaxAnisotropy = 1;
        sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
        sampler.MaxLOD = D3D12_FLOAT32_MAX;
        sampler.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;
        D3D12_ROOT_SIGNATURE_DESC desc = { 2, parameters, 1, &sampler };
        ID3DBlob* code = nullptr;
        ID3DBlob* errors = nullptr;
        const HRESULT result = D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1, &code, &errors);
        if (errors)
        {
            Msg("! D3D12 mip root signature: %s", (const char*)errors->GetBufferPointer());
            errors->Release();
        }
        R_CHK(result);
        R_CHK(GetDevice()->CreateRootSignature(0, code->GetBufferPointer(), code->GetBufferSize(), IID_PPV_ARGS(&_mipRoot)));
        code->Release();
    }
    if (!pipeline)
    {
        const char* vertexSource =
            "struct V {float4 pos:SV_Position; float2 uv:TEXCOORD0;};"
            "V main(uint i:SV_VertexID) {V o; o.uv=float2((i<<1)&2,i&2);"
            "o.pos=float4(o.uv*float2(2,-2)+float2(-1,1),0,1); return o;}";
        const char* declarations = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE1D ?
            "Texture1DArray<float4> tex:register(t0);" : dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ?
            "Texture3D<float4> tex:register(t0);" : "Texture2DArray<float4> tex:register(t0);";
        const char* coordinates = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE1D ? "float2(uv.x,layer)" : "float3(uv,layer)";
        xr_string pixelSource = declarations;
        pixelSource += "SamplerState samp:register(s0); cbuffer Params:register(b0) {float layer;};"
            "float4 main(float4 pos:SV_Position,float2 uv:TEXCOORD0):SV_Target {return tex.SampleLevel(samp,";
        pixelSource += coordinates;
        pixelSource += ",0);}";
        ID3DBlob* vertex = nullptr;
        ID3DBlob* pixel = nullptr;
        ID3DBlob* errors = nullptr;
        HRESULT result = D3DCompile(vertexSource, strlen(vertexSource), nullptr, nullptr, nullptr, "main", "vs_5_0", 0, 0, &vertex, &errors);
        if (errors)
        {
            Msg("! D3D12 mip shader: %s", (const char*)errors->GetBufferPointer());
            errors->Release();
            errors = nullptr;
        }
        R_CHK(result);
        result = D3DCompile(pixelSource.data(), pixelSource.size(), nullptr, nullptr, nullptr, "main", "ps_5_0", 0, 0, &pixel, &errors);
        if (errors)
        {
            Msg("! D3D12 mip shader: %s", (const char*)errors->GetBufferPointer());
            errors->Release();
        }
        R_CHK(result);
        D3D12_GRAPHICS_PIPELINE_STATE_DESC desc = {};
        desc.pRootSignature = _mipRoot;
        desc.VS = { vertex->GetBufferPointer(), vertex->GetBufferSize() };
        desc.PS = { pixel->GetBufferPointer(), pixel->GetBufferSize() };
        desc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        desc.RasterizerState.DepthClipEnable = TRUE;
        desc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_ALWAYS;
        desc.DepthStencilState.FrontFace = desc.DepthStencilState.BackFace =
            { D3D12_STENCIL_OP_KEEP, D3D12_STENCIL_OP_KEEP, D3D12_STENCIL_OP_KEEP, D3D12_COMPARISON_FUNC_ALWAYS };
        desc.BlendState.RenderTarget[0] = { FALSE, FALSE, D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
            D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD, D3D12_LOGIC_OP_NOOP, D3D12_COLOR_WRITE_ENABLE_ALL };
        desc.SampleMask = UINT32_MAX;
        desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        desc.NumRenderTargets = 1;
        desc.RTVFormats[0] = view.Format;
        desc.SampleDesc.Count = 1;
        R_CHK(GetDevice()->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&pipeline)));
        vertex->Release();
        pixel->Release();
        _mipPipelines.push_back({ view.Format, dimension, pipeline });
    }
    auto& resource = surface.GetResource();
    for (u32 mip_idx = view.Mip + 1; mip_idx < texture.MipLevels; ++mip_idx)
    {
        auto descriptor = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
        D3D12_SHADER_RESOURCE_VIEW_DESC srv = {};
        srv.Format = view.Format;
        srv.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
        if (dimension == D3D12_RESOURCE_DIMENSION_TEXTURE1D)
        {
            srv.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE1DARRAY;
            srv.Texture1DArray = { mip_idx - 1, 1, 0, texture.ArraySize, 0 };
        }
        else if (dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D)
        {
            srv.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE3D;
            srv.Texture3D = { mip_idx - 1, 1, 0 };
        }
        else
        {
            srv.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2DARRAY;
            srv.Texture2DArray = { mip_idx - 1, 1, 0, texture.ArraySize, 0, 0 };
        }
        GetDevice()->CreateShaderResourceView(resource.Native, &srv, descriptor.Cpu);
        PublishDescriptor(descriptor);
        const u32 width = std::max(1u, texture.Width >> mip_idx);
        const u32 height = std::max(1u, texture.Height >> mip_idx);
        const u32 slices = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ?
            std::max(1u, texture.Depth >> mip_idx) : texture.ArraySize;
        for (u32 layer_idx = 0; layer_idx < texture.ArraySize; ++layer_idx)
        {
            Transition(resource, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE,
                mip_idx - 1 + layer_idx * texture.MipLevels);
        }
        for (u32 slice_idx = 0; slice_idx < slices; ++slice_idx)
        {
            const u32 subresource = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ?
                mip_idx : mip_idx + slice_idx * texture.MipLevels;
            Transition(resource, D3D12_RESOURCE_STATE_RENDER_TARGET, subresource);
            auto target = AllocateDescriptor(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
            D3D12_RENDER_TARGET_VIEW_DESC rtv = {};
            rtv.Format = view.Format;
            if (dimension == D3D12_RESOURCE_DIMENSION_TEXTURE1D)
            {
                rtv.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE1DARRAY;
                rtv.Texture1DArray = { mip_idx, slice_idx, 1 };
            }
            else if (dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D)
            {
                rtv.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE3D;
                rtv.Texture3D = { mip_idx, slice_idx, 1 };
            }
            else
            {
                rtv.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2DARRAY;
                rtv.Texture2DArray = { mip_idx, slice_idx, 1, 0 };
            }
            GetDevice()->CreateRenderTargetView(resource.Native, &rtv, target.Cpu);
            auto commands = Commands();
            commands->SetDescriptorHeaps(1, &_resourceHeap);
            commands->SetGraphicsRootSignature(_mipRoot);
            commands->SetPipelineState(pipeline);
            commands->SetGraphicsRootDescriptorTable(0, descriptor.Gpu);
            const float layer = dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ?
                (float(slice_idx) + 0.5f) / slices : float(slice_idx);
            commands->SetGraphicsRoot32BitConstants(1, 1, &layer, 0);
            D3D12_VIEWPORT viewport = { 0, 0, float(width), float(height), 0, 1 };
            D3D12_RECT scissor = { 0, 0, LONG(width), LONG(height) };
            commands->RSSetViewports(1, &viewport);
            commands->RSSetScissorRects(1, &scissor);
            commands->OMSetRenderTargets(1, &target.Cpu, FALSE, nullptr);
            commands->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
            commands->DrawInstanced(3, 1, 0, 0);
            Retire(target);
        }
        Retire(descriptor);
    }
    InvalidateBindings();
}
