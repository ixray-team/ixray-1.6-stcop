/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include <dxgi1_5.h>

#include "SharedD3D12.h"
#include "CommandQueueD3D12.h"
#include "CommandAllocatorD3D12.h"
#include "DescriptorPoolD3D12.h"
#include "FenceD3D12.h"
#include "FrameBufferD3D12.h"
#include "MemoryD3D12.h"
#include "BufferD3D12.h"
#include "TextureD3D12.h"
#include "DescriptorD3D12.h"
#include "PipelineLayoutD3D12.h"
#include "PipelineD3D12.h"
#include "QueryPoolD3D12.h"
#include "SwapChainD3D12.h"
#include "AccelerationStructureD3D12.h"
#include "CommandBufferD3D12.h"

#include <dxgidebug.h>

using namespace nri;

extern MemoryType GetMemoryType(MemoryLocation memoryLocation, const D3D12_RESOURCE_DESC& resourceDesc);
extern bool RequiresDedicatedAllocation(MemoryType memoryType);

inline Vendor GetVendor(ID3D12Device* device)
{
    ComPtr<IDXGIFactory4> DXGIFactory;
    CreateDXGIFactory(IID_PPV_ARGS(&DXGIFactory));

    DXGI_ADAPTER_DESC desc = {};
    if (DXGIFactory)
    {
        LUID luid = device->GetAdapterLuid();

        ComPtr<IDXGIAdapter> adapter;
        DXGIFactory->EnumAdapterByLuid(luid, IID_PPV_ARGS(&adapter));
        if (adapter)
            adapter->GetDesc(&desc);
    }

    return GetVendorFromID(desc.VendorId);
}

Result CreateDeviceD3D12(const DeviceCreationD3D12Desc& deviceCreationDesc, DeviceBase*& device)
{
    Log log(GraphicsAPI::D3D12, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceD3D12* implementation = Allocate<DeviceD3D12>(allocator, log, allocator);
    const Result res = implementation->Create(deviceCreationDesc);

    if (res == Result::SUCCESS)
    {
        device = implementation;
        return Result::SUCCESS;
    }

    Deallocate(allocator, implementation);
    return res;
}

Result CreateDeviceD3D12(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device)
{
    Log log(GraphicsAPI::D3D12, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    ComPtr<IDXGIFactory4> factory;
    HRESULT hr = CreateDXGIFactory2(0, IID_PPV_ARGS(&factory));
    RETURN_ON_BAD_HRESULT(log, hr, "CreateDXGIFactory2() failed, error code: 0x%X.", hr);

    ComPtr<IDXGIAdapter> adapter;
    if (deviceCreationDesc.physicalDeviceGroup != nullptr)
    {
        LUID luid = *(LUID*)&deviceCreationDesc.physicalDeviceGroup->luid;
        hr = factory->EnumAdapterByLuid(luid, IID_PPV_ARGS(&adapter));
        RETURN_ON_BAD_HRESULT(log, hr, "IDXGIFactory4::EnumAdapterByLuid() failed, error code: 0x%X.", hr);
    }
    else
    {
        hr = factory->EnumAdapters(0, &adapter);
        RETURN_ON_BAD_HRESULT(log, hr, "IDXGIFactory4::EnumAdapters() failed, error code: 0x%X.", hr);
    }

    DeviceD3D12* implementation = Allocate<DeviceD3D12>(allocator, log, allocator);
    const nri::Result result = implementation->Create(adapter, deviceCreationDesc);
    if (result != nri::Result::SUCCESS)
    {
        Deallocate(allocator, implementation);
        return result;
    }

    device = (DeviceBase*)implementation;

    return nri::Result::SUCCESS;
}

DeviceD3D12::DeviceD3D12(const Log& log, StdAllocator<uint8_t>& stdAllocator)
    : DeviceBase(log, stdAllocator)
    , m_DescriptorHeaps(GetStdAllocator())
    , m_FreeDescriptors(GetStdAllocator())
    , m_DrawCommandSignatures(GetStdAllocator())
    , m_DrawIndexedCommandSignatures(GetStdAllocator())
{
    m_FreeDescriptors.resize(DESCRIPTOR_HEAP_TYPE_NUM, Vector<DescriptorHandle>(GetStdAllocator()));

    if (FillFunctionTable(m_CoreInterface) != Result::SUCCESS)
        REPORT_ERROR(GetLog(), "Failed to get 'CoreInterface' interface in DeviceD3D12().");
}

DeviceD3D12::~DeviceD3D12()
{
    for (auto& commandQueueD3D12 : m_CommandQueues)
    {
        if (commandQueueD3D12)
            Deallocate(GetStdAllocator(), commandQueueD3D12);
    }
}

template<typename Implementation, typename Interface, typename ... Args>
Result DeviceD3D12::CreateImplementation(Interface*& entity, const Args&... args)
{
    Implementation* implementation = Allocate<Implementation>(GetStdAllocator(), *this);
    const Result result = implementation->Create(args...);

    if (result == Result::SUCCESS)
    {
        entity = (Interface*)implementation;
        return Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return result;
}

bool DeviceD3D12::GetOutput(Display* display, ComPtr<IDXGIOutput>& output) const
{
    if (display == nullptr)
        return false;

    const uint32_t index = (*(uint32_t*)&display) - 1;
    const HRESULT result = m_Adapter->EnumOutputs(index, &output);

    return SUCCEEDED(result);
}

Result DeviceD3D12::Create(const DeviceCreationD3D12Desc& deviceCreationDesc)
{
    m_SkipLiveObjectsReporting = true;
    m_Adapter = deviceCreationDesc.d3d12PhysicalAdapter;
    m_Device = (ID3D12Device*)deviceCreationDesc.d3d12Device;

    if (m_Adapter == nullptr)
    {
        const LUID luid = m_Device->GetAdapterLuid();

        ComPtr<IDXGIFactory4> DXGIFactory;
        HRESULT result = CreateDXGIFactory(IID_PPV_ARGS(&DXGIFactory));
        RETURN_ON_BAD_HRESULT(GetLog(), result, "Failed to create IDXGIFactory4");

        result = DXGIFactory->EnumAdapterByLuid(luid, IID_PPV_ARGS(&m_Adapter));
        RETURN_ON_BAD_HRESULT(GetLog(), result, "Failed to find IDXGIAdapter by LUID");
    }

    m_Device->QueryInterface(IID_PPV_ARGS(&m_Device5));

    if (deviceCreationDesc.d3d12GraphicsQueue)
        CreateCommandQueue((ID3D12CommandQueue*)deviceCreationDesc.d3d12GraphicsQueue, m_CommandQueues[(uint32_t)CommandQueueType::GRAPHICS]);
    if (deviceCreationDesc.d3d12ComputeQueue)
        CreateCommandQueue((ID3D12CommandQueue*)deviceCreationDesc.d3d12ComputeQueue, m_CommandQueues[(uint32_t)CommandQueueType::COMPUTE]);
    if (deviceCreationDesc.d3d12CopyQueue)
        CreateCommandQueue((ID3D12CommandQueue*)deviceCreationDesc.d3d12CopyQueue, m_CommandQueues[(uint32_t)CommandQueueType::COPY]);

    CommandQueue* commandQueue;
    Result result = GetCommandQueue(CommandQueueType::GRAPHICS, commandQueue);
    if (result != Result::SUCCESS)
        return result;

    UpdateDeviceDesc(deviceCreationDesc.enableAPIValidation);

    return Result::SUCCESS;
}

Result DeviceD3D12::Create(IDXGIAdapter* dxgiAdapter, const DeviceCreationDesc& deviceCreationDesc)
{
    m_SkipLiveObjectsReporting = deviceCreationDesc.skipLiveObjectsReporting;
    m_Adapter = dxgiAdapter;

    // Enable the debug layer (requires the Graphics Tools "optional feature").
    // NOTE: Enabling the debug layer after device creation will invalidate the active device.
    if (deviceCreationDesc.enableAPIValidation)
    {
        ComPtr<ID3D12Debug> debugController;
        if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debugController))))
            debugController->EnableDebugLayer();

        // GPU-based validation
        //ComPtr<ID3D12Debug1> debugController1;
        //if (SUCCEEDED(debugController->QueryInterface(IID_PPV_ARGS(&debugController1))))
        //    debugController1->SetEnableGPUBasedValidation(true);
    }

    HRESULT hr = D3D12CreateDevice(dxgiAdapter, D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_Device));
    if (FAILED(hr))
    {
        REPORT_ERROR(GetLog(), "D3D12CreateDevice() failed, error code: 0x%X.", hr);
        return Result::FAILURE;
    }

    // TODO: this code is currently needed to disable known false-positive errors reported by the debug layer
    if (deviceCreationDesc.enableAPIValidation)
    {
        ComPtr<ID3D12InfoQueue> pInfoQueue;
        m_Device->QueryInterface(&pInfoQueue);

        if (pInfoQueue)
        {
            #ifdef _DEBUG
                pInfoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_CORRUPTION, true);
                pInfoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_ERROR, true);
            #endif

            D3D12_MESSAGE_ID disableMessageIDs[] = {
                D3D12_MESSAGE_ID_COMMAND_LIST_STATIC_DESCRIPTOR_RESOURCE_DIMENSION_MISMATCH, // TODO: descriptor validation doesn't understand acceleration structures used outside of RAYGEN shaders
            };

            D3D12_INFO_QUEUE_FILTER filter = {};
            filter.DenyList.pIDList = disableMessageIDs;
            filter.DenyList.NumIDs = GetCountOf(disableMessageIDs);
            pInfoQueue->AddStorageFilterEntries(&filter);
        }
    }

    m_Device->QueryInterface(IID_PPV_ARGS(&m_Device5));

    CommandQueue* commandQueue;
    Result result = GetCommandQueue(CommandQueueType::GRAPHICS, commandQueue);
    if (result != Result::SUCCESS)
        return result;

    D3D12_INDIRECT_ARGUMENT_DESC indirectArgumentDesc = {};
    indirectArgumentDesc.Type = D3D12_INDIRECT_ARGUMENT_TYPE_DISPATCH;

    D3D12_COMMAND_SIGNATURE_DESC commandSignatureDesc = {};
    commandSignatureDesc.NumArgumentDescs = 1;
    commandSignatureDesc.pArgumentDescs = &indirectArgumentDesc;
    commandSignatureDesc.NodeMask = NRI_TEMP_NODE_MASK;
    commandSignatureDesc.ByteStride = 12;

    hr = m_Device->CreateCommandSignature(&commandSignatureDesc, nullptr, IID_PPV_ARGS(&m_DispatchCommandSignature));
    if (FAILED(hr))
    {
        REPORT_ERROR(GetLog(), "ID3D12Device::CreateCommandSignature() failed, error code: 0x%X.", hr);
        return Result::FAILURE;
    }

    UpdateDeviceDesc(deviceCreationDesc.enableAPIValidation);

    return Result::SUCCESS;
}

Result DeviceD3D12::CreateCpuOnlyVisibleDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type)
{
    // IMPORTANT: m_FreeDescriptorLocks[type] must be acquired before calling this function
    ExclusiveScope lock(m_DescriptorHeapLock);

    size_t heapIndex = m_DescriptorHeaps.size();
    if (heapIndex >= HeapIndexType(-1))
        return Result::OUT_OF_MEMORY;

    ComPtr<ID3D12DescriptorHeap> descriptorHeap;
    D3D12_DESCRIPTOR_HEAP_DESC desc = {type, DESCRIPTORS_BATCH_SIZE, D3D12_DESCRIPTOR_HEAP_FLAG_NONE, NRI_TEMP_NODE_MASK};
    HRESULT hr = ((ID3D12Device*)m_Device)->CreateDescriptorHeap(&desc, IID_PPV_ARGS(&descriptorHeap));
    if (FAILED(hr))
    {
        REPORT_ERROR(GetLog(), "ID3D12Device::CreateDescriptorHeap() failed, return code %d.", hr);
        return Result::FAILURE;
    }

    DescriptorHeapDesc descriptorHeapDesc = {};
    descriptorHeapDesc.descriptorHeap = descriptorHeap;
    descriptorHeapDesc.descriptorPointerCPU = descriptorHeap->GetCPUDescriptorHandleForHeapStart().ptr;
    descriptorHeapDesc.descriptorSize = m_Device->GetDescriptorHandleIncrementSize(type);
    m_DescriptorHeaps.push_back(descriptorHeapDesc);

    auto& freeDescriptors = m_FreeDescriptors[type];
    for (uint32_t i = 0; i < desc.NumDescriptors; i++)
        freeDescriptors.push_back( {(HeapIndexType)heapIndex, (HeapOffsetType)i} );

    return Result::SUCCESS;
}

Result DeviceD3D12::GetDescriptorHandle(D3D12_DESCRIPTOR_HEAP_TYPE type, DescriptorHandle& descriptorHandle)
{
    ExclusiveScope lock(m_FreeDescriptorLocks[type]);

    auto& freeDescriptors = m_FreeDescriptors[type];
    if (freeDescriptors.empty())
    {
        Result result = CreateCpuOnlyVisibleDescriptorHeap(type);
        if (result != Result::SUCCESS)
            return result;
    }

    descriptorHandle = freeDescriptors.back();
    freeDescriptors.pop_back();

    return Result::SUCCESS;
}

DescriptorPointerCPU DeviceD3D12::GetDescriptorPointerCPU(const DescriptorHandle& descriptorHandle)
{
    ExclusiveScope lock(m_DescriptorHeapLock);

    const DescriptorHeapDesc& descriptorHeapDesc = m_DescriptorHeaps[descriptorHandle.heapIndex];
    DescriptorPointerCPU descriptorPointer = descriptorHeapDesc.descriptorPointerCPU + descriptorHandle.heapOffset * descriptorHeapDesc.descriptorSize;

    return descriptorPointer;
}

void DeviceD3D12::GetMemoryInfo(MemoryLocation memoryLocation, const D3D12_RESOURCE_DESC& resourceDesc, MemoryDesc& memoryDesc) const
{
    memoryDesc.type = GetMemoryType(memoryLocation, resourceDesc);

    D3D12_RESOURCE_ALLOCATION_INFO resourceAllocationInfo = m_Device->GetResourceAllocationInfo(NRI_TEMP_NODE_MASK, 1, &resourceDesc);
    memoryDesc.size = (uint64_t)resourceAllocationInfo.SizeInBytes;
    memoryDesc.alignment = (uint32_t)resourceAllocationInfo.Alignment;

    memoryDesc.mustBeDedicated = RequiresDedicatedAllocation(memoryDesc.type);
}

ID3D12CommandSignature* DeviceD3D12::CreateCommandSignature(D3D12_INDIRECT_ARGUMENT_TYPE indirectArgumentType, uint32_t stride)
{
    D3D12_INDIRECT_ARGUMENT_DESC indirectArgumentDesc = {};
    indirectArgumentDesc.Type = indirectArgumentType;

    D3D12_COMMAND_SIGNATURE_DESC commandSignatureDesc = {};
    commandSignatureDesc.NumArgumentDescs = 1;
    commandSignatureDesc.pArgumentDescs = &indirectArgumentDesc;
    commandSignatureDesc.NodeMask = NRI_TEMP_NODE_MASK;
    commandSignatureDesc.ByteStride = stride;

    ID3D12CommandSignature* commandSignature = nullptr;
    HRESULT hr = m_Device->CreateCommandSignature(&commandSignatureDesc, nullptr, IID_PPV_ARGS(&commandSignature));
    if (FAILED(hr))
        REPORT_ERROR(GetLog(), "ID3D12Device::CreateCommandSignature() failed, error code: 0x%X.", hr);

    return commandSignature;
}

ID3D12CommandSignature* DeviceD3D12::GetDrawCommandSignature(uint32_t stride)
{
    auto commandSignatureIt = m_DrawCommandSignatures.find(stride);
    if (commandSignatureIt != m_DrawCommandSignatures.end())
        return commandSignatureIt->second;

    ID3D12CommandSignature* commandSignature = CreateCommandSignature(D3D12_INDIRECT_ARGUMENT_TYPE_DRAW, stride);
    m_DrawCommandSignatures[stride] = commandSignature;

    return commandSignature;
}

ID3D12CommandSignature* DeviceD3D12::GetDrawIndexedCommandSignature(uint32_t stride)
{
    auto commandSignatureIt = m_DrawIndexedCommandSignatures.find(stride);
    if (commandSignatureIt != m_DrawIndexedCommandSignatures.end())
        return commandSignatureIt->second;

    ID3D12CommandSignature* commandSignature = CreateCommandSignature(D3D12_INDIRECT_ARGUMENT_TYPE_DRAW_INDEXED, stride);
    m_DrawIndexedCommandSignatures[stride] = commandSignature;

    return commandSignature;
}

ID3D12CommandSignature* DeviceD3D12::GetDispatchCommandSignature() const
{
    return m_DispatchCommandSignature.GetInterface();
}

MemoryType DeviceD3D12::GetMemoryType(MemoryLocation memoryLocation, const D3D12_RESOURCE_DESC& resourceDesc) const
{
    return ::GetMemoryType(memoryLocation, resourceDesc);
}

void DeviceD3D12::UpdateDeviceDesc(bool enableValidation)
{
    D3D12_FEATURE_DATA_D3D12_OPTIONS options = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS, &options, sizeof(options));

    D3D12_FEATURE_DATA_D3D12_OPTIONS1 options1 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS1, &options1, sizeof(options1));

    D3D12_FEATURE_DATA_D3D12_OPTIONS2 options2 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS2, &options2, sizeof(options2));

    D3D12_FEATURE_DATA_D3D12_OPTIONS3 options3 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS3, &options3, sizeof(options3));

    D3D12_FEATURE_DATA_D3D12_OPTIONS4 options4 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS4, &options4, sizeof(options4));

    D3D12_FEATURE_DATA_D3D12_OPTIONS5 options5 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS5, &options5, sizeof(options5));

    D3D12_FEATURE_DATA_D3D12_OPTIONS7 options7 = {};
    m_Device->CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS7, &options7, sizeof(options7));

    m_IsRaytracingSupported = options5.RaytracingTier >= D3D12_RAYTRACING_TIER_1_0;
    m_IsMeshShaderSupported = options7.MeshShaderTier >= D3D12_MESH_SHADER_TIER_1;

    D3D12_FEATURE_DATA_FEATURE_LEVELS levels = {};
     const std::array<D3D_FEATURE_LEVEL, 4> levelsList = {
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_12_0,
        D3D_FEATURE_LEVEL_12_1,
    };
    levels.NumFeatureLevels = (uint32_t)levelsList.size();
    levels.pFeatureLevelsRequested = levelsList.data();
    m_Device->CheckFeatureSupport(D3D12_FEATURE_FEATURE_LEVELS, &levels, sizeof(levels));

    uint64_t timestampFrequency = 0;

    CommandQueue* commandQueue = nullptr;
    const Result result = GetCommandQueue(CommandQueueType::GRAPHICS, commandQueue);

    if (result == Result::SUCCESS)
    {
        ID3D12CommandQueue* commandQueueD3D12 = *(CommandQueueD3D12*)commandQueue;
        commandQueueD3D12->GetTimestampFrequency(&timestampFrequency);
    }
    else
    {
        REPORT_ERROR(GetLog(), "Failed to get command queue to update device desc, result: %d.", (int32_t)result);
    }

    m_DeviceDesc.graphicsAPI = GraphicsAPI::D3D12;
    m_DeviceDesc.vendor = GetVendor(m_Device);
    m_DeviceDesc.nriVersionMajor = NRI_VERSION_MAJOR;
    m_DeviceDesc.nriVersionMinor = NRI_VERSION_MINOR;

    m_DeviceDesc.viewportMaxNum = D3D12_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE;
    m_DeviceDesc.viewportSubPixelBits = D3D12_SUBPIXEL_FRACTIONAL_BIT_COUNT;
    m_DeviceDesc.viewportBoundsRange[0] = D3D12_VIEWPORT_BOUNDS_MIN;
    m_DeviceDesc.viewportBoundsRange[1] = D3D12_VIEWPORT_BOUNDS_MAX;

    m_DeviceDesc.frameBufferMaxDim = D3D12_REQ_RENDER_TO_BUFFER_WINDOW_WIDTH;
    m_DeviceDesc.frameBufferLayerMaxNum = D3D12_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION;
    m_DeviceDesc.framebufferColorAttachmentMaxNum = D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT;

    m_DeviceDesc.frameBufferColorSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.frameBufferDepthSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.frameBufferStencilSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.frameBufferNoAttachmentsSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.textureColorSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.textureIntegerSampleMaxNum = 1;
    m_DeviceDesc.textureDepthSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.textureStencilSampleMaxNum = D3D12_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_DeviceDesc.storageTextureSampleMaxNum = 1;

    m_DeviceDesc.texture1DMaxDim = D3D12_REQ_TEXTURE1D_U_DIMENSION;
    m_DeviceDesc.texture2DMaxDim = D3D12_REQ_TEXTURE2D_U_OR_V_DIMENSION;
    m_DeviceDesc.texture3DMaxDim = D3D12_REQ_TEXTURE3D_U_V_OR_W_DIMENSION;
    m_DeviceDesc.textureArrayMaxDim = D3D12_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION;
    m_DeviceDesc.texelBufferMaxDim = (1 << D3D12_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP) - 1;

    m_DeviceDesc.memoryAllocationMaxNum = 0xFFFFFFFF;
    m_DeviceDesc.samplerAllocationMaxNum = D3D12_REQ_SAMPLER_OBJECT_COUNT_PER_DEVICE;
    m_DeviceDesc.uploadBufferTextureRowAlignment = D3D12_TEXTURE_DATA_PITCH_ALIGNMENT;
    m_DeviceDesc.uploadBufferTextureSliceAlignment = D3D12_TEXTURE_DATA_PLACEMENT_ALIGNMENT;
    m_DeviceDesc.typedBufferOffsetAlignment = D3D12_RAW_UAV_SRV_BYTE_ALIGNMENT;
    m_DeviceDesc.constantBufferOffsetAlignment = D3D12_CONSTANT_BUFFER_DATA_PLACEMENT_ALIGNMENT;
    m_DeviceDesc.constantBufferMaxRange = D3D12_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT * 16;
    m_DeviceDesc.storageBufferOffsetAlignment = D3D12_RAW_UAV_SRV_BYTE_ALIGNMENT;
    m_DeviceDesc.storageBufferMaxRange = (1 << D3D12_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP) - 1;
    m_DeviceDesc.bufferTextureGranularity = 1; // TODO: 64KB?
    m_DeviceDesc.bufferMaxSize = D3D12_REQ_RESOURCE_SIZE_IN_MEGABYTES_EXPRESSION_C_TERM * 1024ull * 1024ull;
    m_DeviceDesc.pushConstantsMaxSize = D3D12_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT * 16;

    m_DeviceDesc.boundDescriptorSetMaxNum = D3D12_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;
    m_DeviceDesc.perStageDescriptorSamplerMaxNum = D3D12_COMMONSHADER_SAMPLER_SLOT_COUNT;
    m_DeviceDesc.perStageDescriptorConstantBufferMaxNum = D3D12_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT;
    m_DeviceDesc.perStageDescriptorStorageBufferMaxNum = levels.MaxSupportedFeatureLevel >= D3D_FEATURE_LEVEL_11_1 ? D3D12_UAV_SLOT_COUNT : D3D12_PS_CS_UAV_REGISTER_COUNT;
    m_DeviceDesc.perStageDescriptorTextureMaxNum = D3D12_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;
    m_DeviceDesc.perStageDescriptorStorageTextureMaxNum = levels.MaxSupportedFeatureLevel >= D3D_FEATURE_LEVEL_11_1 ? D3D12_UAV_SLOT_COUNT : D3D12_PS_CS_UAV_REGISTER_COUNT;
    m_DeviceDesc.perStageResourceMaxNum = D3D12_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;

    m_DeviceDesc.descriptorSetSamplerMaxNum = m_DeviceDesc.perStageDescriptorSamplerMaxNum;
    m_DeviceDesc.descriptorSetConstantBufferMaxNum = m_DeviceDesc.perStageDescriptorConstantBufferMaxNum;
    m_DeviceDesc.descriptorSetStorageBufferMaxNum = m_DeviceDesc.perStageDescriptorStorageBufferMaxNum;
    m_DeviceDesc.descriptorSetTextureMaxNum = m_DeviceDesc.perStageDescriptorTextureMaxNum;
    m_DeviceDesc.descriptorSetStorageTextureMaxNum = m_DeviceDesc.perStageDescriptorStorageTextureMaxNum;

    m_DeviceDesc.vertexShaderAttributeMaxNum = D3D12_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT;
    m_DeviceDesc.vertexShaderStreamMaxNum = D3D12_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT;
    m_DeviceDesc.vertexShaderOutputComponentMaxNum = D3D12_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT * 4;

    m_DeviceDesc.tessControlShaderGenerationMaxLevel = D3D12_HS_MAXTESSFACTOR_UPPER_BOUND;
    m_DeviceDesc.tessControlShaderPatchPointMaxNum = D3D12_IA_PATCH_MAX_CONTROL_POINT_COUNT;
    m_DeviceDesc.tessControlShaderPerVertexInputComponentMaxNum = D3D12_HS_CONTROL_POINT_PHASE_INPUT_REGISTER_COUNT * D3D12_HS_CONTROL_POINT_REGISTER_COMPONENTS;
    m_DeviceDesc.tessControlShaderPerVertexOutputComponentMaxNum = D3D12_HS_CONTROL_POINT_PHASE_OUTPUT_REGISTER_COUNT * D3D12_HS_CONTROL_POINT_REGISTER_COMPONENTS;
    m_DeviceDesc.tessControlShaderPerPatchOutputComponentMaxNum = D3D12_HS_OUTPUT_PATCH_CONSTANT_REGISTER_SCALAR_COMPONENTS;
    m_DeviceDesc.tessControlShaderTotalOutputComponentMaxNum = m_DeviceDesc.tessControlShaderPatchPointMaxNum * m_DeviceDesc.tessControlShaderPerVertexOutputComponentMaxNum + m_DeviceDesc.tessControlShaderPerPatchOutputComponentMaxNum;

    m_DeviceDesc.tessEvaluationShaderInputComponentMaxNum = D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COUNT * D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENTS;
    m_DeviceDesc.tessEvaluationShaderOutputComponentMaxNum = D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COUNT * D3D12_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENTS;

    m_DeviceDesc.geometryShaderInvocationMaxNum = D3D12_GS_MAX_INSTANCE_COUNT;
    m_DeviceDesc.geometryShaderInputComponentMaxNum = D3D12_GS_INPUT_REGISTER_COUNT * D3D12_GS_INPUT_REGISTER_COMPONENTS;
    m_DeviceDesc.geometryShaderOutputComponentMaxNum = D3D12_GS_OUTPUT_REGISTER_COUNT * D3D12_GS_INPUT_REGISTER_COMPONENTS;
    m_DeviceDesc.geometryShaderOutputVertexMaxNum = D3D12_GS_MAX_OUTPUT_VERTEX_COUNT_ACROSS_INSTANCES;
    m_DeviceDesc.geometryShaderTotalOutputComponentMaxNum = D3D12_REQ_GS_INVOCATION_32BIT_OUTPUT_COMPONENT_LIMIT;

    m_DeviceDesc.fragmentShaderInputComponentMaxNum = D3D12_PS_INPUT_REGISTER_COUNT * D3D12_PS_INPUT_REGISTER_COMPONENTS;
    m_DeviceDesc.fragmentShaderOutputAttachmentMaxNum = D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT;
    m_DeviceDesc.fragmentShaderDualSourceAttachmentMaxNum = 1;
    m_DeviceDesc.fragmentShaderCombinedOutputResourceMaxNum = D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT + D3D12_PS_CS_UAV_REGISTER_COUNT;

    m_DeviceDesc.computeShaderSharedMemoryMaxSize = D3D12_CS_THREAD_LOCAL_TEMP_REGISTER_POOL;
    m_DeviceDesc.computeShaderWorkGroupMaxNum[0] = D3D12_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_DeviceDesc.computeShaderWorkGroupMaxNum[1] = D3D12_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_DeviceDesc.computeShaderWorkGroupMaxNum[2] = D3D12_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_DeviceDesc.computeShaderWorkGroupInvocationMaxNum = D3D12_CS_THREAD_GROUP_MAX_THREADS_PER_GROUP;
    m_DeviceDesc.computeShaderWorkGroupMaxDim[0] = D3D12_CS_THREAD_GROUP_MAX_X;
    m_DeviceDesc.computeShaderWorkGroupMaxDim[1] = D3D12_CS_THREAD_GROUP_MAX_Y;
    m_DeviceDesc.computeShaderWorkGroupMaxDim[2] = D3D12_CS_THREAD_GROUP_MAX_Z;

    if (m_IsRaytracingSupported)
    {
        m_DeviceDesc.rayTracingShaderGroupIdentifierSize = D3D12_RAYTRACING_SHADER_RECORD_BYTE_ALIGNMENT;
        m_DeviceDesc.rayTracingShaderTableAligment = D3D12_RAYTRACING_SHADER_TABLE_BYTE_ALIGNMENT;
        m_DeviceDesc.rayTracingShaderTableMaxStride = std::numeric_limits<uint64_t>::max();
        m_DeviceDesc.rayTracingShaderRecursionMaxDepth = D3D12_RAYTRACING_MAX_DECLARABLE_TRACE_RECURSION_DEPTH;
        m_DeviceDesc.rayTracingGeometryObjectMaxNum = (1 << 24) - 1;
    }

    m_DeviceDesc.timestampFrequencyHz = timestampFrequency;
    m_DeviceDesc.subPixelPrecisionBits = D3D12_SUBPIXEL_FRACTIONAL_BIT_COUNT;
    m_DeviceDesc.subTexelPrecisionBits = D3D12_SUBTEXEL_FRACTIONAL_BIT_COUNT;
    m_DeviceDesc.mipmapPrecisionBits = D3D12_MIP_LOD_FRACTIONAL_BIT_COUNT;
    m_DeviceDesc.drawIndexedIndex16ValueMax = D3D12_16BIT_INDEX_STRIP_CUT_VALUE;
    m_DeviceDesc.drawIndexedIndex32ValueMax = D3D12_32BIT_INDEX_STRIP_CUT_VALUE;
    m_DeviceDesc.drawIndirectMaxNum = (1ull << D3D12_REQ_DRAWINDEXED_INDEX_COUNT_2_TO_EXP) - 1;
    m_DeviceDesc.samplerLodBiasMin = D3D12_MIP_LOD_BIAS_MIN;
    m_DeviceDesc.samplerLodBiasMax = D3D12_MIP_LOD_BIAS_MAX;
    m_DeviceDesc.samplerAnisotropyMax = D3D12_DEFAULT_MAX_ANISOTROPY;
    m_DeviceDesc.texelOffsetMin = D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE;
    m_DeviceDesc.texelOffsetMax = D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE;
    m_DeviceDesc.texelGatherOffsetMin = D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE;
    m_DeviceDesc.texelGatherOffsetMax = D3D12_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE;
    m_DeviceDesc.clipDistanceMaxNum = D3D12_CLIP_OR_CULL_DISTANCE_COUNT;
    m_DeviceDesc.cullDistanceMaxNum = D3D12_CLIP_OR_CULL_DISTANCE_COUNT;
    m_DeviceDesc.combinedClipAndCullDistanceMaxNum = D3D12_CLIP_OR_CULL_DISTANCE_COUNT;
    m_DeviceDesc.conservativeRasterTier = (uint8_t)options.ConservativeRasterizationTier;
    m_DeviceDesc.physicalDeviceNum = (uint8_t)m_Device->GetNodeCount();

    m_DeviceDesc.isAPIValidationEnabled = enableValidation;
    m_DeviceDesc.isTextureFilterMinMaxSupported = levels.MaxSupportedFeatureLevel >= D3D_FEATURE_LEVEL_11_1 ? true : false;
    m_DeviceDesc.isLogicOpSupported = options.OutputMergerLogicOp != 0;
    m_DeviceDesc.isDepthBoundsTestSupported = options2.DepthBoundsTestSupported != 0;
    m_DeviceDesc.isProgrammableSampleLocationsSupported = options2.ProgrammableSamplePositionsTier != D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED;
    m_DeviceDesc.isComputeQueueSupported = true;
    m_DeviceDesc.isCopyQueueSupported = true;
    m_DeviceDesc.isCopyQueueTimestampSupported = options3.CopyQueueTimestampQueriesSupported != 0;
    m_DeviceDesc.isRegisterAliasingSupported = true;
    m_DeviceDesc.isSubsetAllocationSupported = true;
    m_DeviceDesc.isFloat16Supported = options4.Native16BitShaderOpsSupported;
}

//================================================================================================================
// DeviceBase
//================================================================================================================

void DeviceD3D12::Destroy()
{
    bool skipLiveObjectsReporting = m_SkipLiveObjectsReporting;
    Deallocate(GetStdAllocator(), this);

    if (!skipLiveObjectsReporting)
    {
        ComPtr<IDXGIDebug1> pDebug;
        HRESULT hr = DXGIGetDebugInterface1(0, IID_PPV_ARGS(&pDebug));
        if (SUCCEEDED(hr))
            pDebug->ReportLiveObjects(DXGI_DEBUG_ALL, (DXGI_DEBUG_RLO_FLAGS)((uint32_t)DXGI_DEBUG_RLO_DETAIL | (uint32_t)DXGI_DEBUG_RLO_IGNORE_INTERNAL));
    }
}

//================================================================================================================
// NRI
//================================================================================================================

inline Result DeviceD3D12::CreateSwapChain(const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return CreateImplementation<SwapChainD3D12>(swapChain, swapChainDesc);
}

inline void DeviceD3D12::DestroySwapChain(SwapChain& swapChain)
{
    Deallocate(GetStdAllocator(), (SwapChainD3D12*)&swapChain);
}

inline Result DeviceD3D12::GetDisplays(Display** displays, uint32_t& displayNum)
{
    HRESULT result = S_OK;

    if (displays == nullptr || displayNum == 0)
    {
        UINT i = 0;
        for(; result != DXGI_ERROR_NOT_FOUND; i++)
        {
            ComPtr<IDXGIOutput> output;
            result = m_Adapter->EnumOutputs(i, &output);
        }

        displayNum = i;
        return Result::SUCCESS;
    }

    UINT i = 0;
    for(; result != DXGI_ERROR_NOT_FOUND && i < displayNum; i++)
    {
        ComPtr<IDXGIOutput> output;
        result = m_Adapter->EnumOutputs(i, &output);
        if (result != DXGI_ERROR_NOT_FOUND)
            displays[i] = (Display*)(size_t)(i + 1);
    }

    for(; i < displayNum; i++)
        displays[i] = nullptr;

    return Result::SUCCESS;
}

inline Result DeviceD3D12::GetDisplaySize(Display& display, uint16_t& width, uint16_t& height)
{
    Display* address = &display;

    if (address == nullptr)
        return Result::UNSUPPORTED;

    const uint32_t index = (*(uint32_t*)&address) - 1;

    ComPtr<IDXGIOutput> output;
    HRESULT result = m_Adapter->EnumOutputs(index, &output);

    if (FAILED(result))
        return Result::UNSUPPORTED;

    DXGI_OUTPUT_DESC outputDesc = {};
    result = output->GetDesc(&outputDesc);

    if (FAILED(result))
        return Result::UNSUPPORTED;

    MONITORINFO monitorInfo = {};
    monitorInfo.cbSize = sizeof(monitorInfo);

    if (!GetMonitorInfoA(outputDesc.Monitor, &monitorInfo))
        return Result::UNSUPPORTED;

    const RECT rect = monitorInfo.rcMonitor;

    width = uint16_t(rect.right - rect.left);
    height = uint16_t(rect.bottom - rect.top);

    return Result::SUCCESS;
}

inline Result DeviceD3D12::GetCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    ExclusiveScope lock(m_QueueLock);

    uint32_t queueIndex = (uint32_t)commandQueueType;

    if (m_CommandQueues[queueIndex])
    {
        commandQueue = (CommandQueue*)m_CommandQueues[queueIndex];
        return Result::SUCCESS;
    }

    Result result = CreateCommandQueue(commandQueueType, commandQueue);
    if (result != Result::SUCCESS)
    {
        REPORT_ERROR(GetLog(), "Device::GetCommandQueue() failed.");
        return result;
    }

    m_CommandQueues[queueIndex] = (CommandQueueD3D12*)commandQueue;

    return Result::SUCCESS;
}

inline Result DeviceD3D12::CreateCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    return CreateImplementation<CommandQueueD3D12>(commandQueue, commandQueueType);
}

inline Result DeviceD3D12::CreateCommandQueue(void* d3d12commandQueue, CommandQueueD3D12*& commandQueue)
{
    return CreateImplementation<CommandQueueD3D12>(commandQueue, (ID3D12CommandQueue*)d3d12commandQueue);
}

inline Result DeviceD3D12::CreateCommandAllocator(const CommandQueue& commandQueue, CommandAllocator*& commandAllocator)
{
    return CreateImplementation<CommandAllocatorD3D12>(commandAllocator, commandQueue);
}

inline Result DeviceD3D12::CreateDescriptorPool(const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return CreateImplementation<DescriptorPoolD3D12>(descriptorPool, descriptorPoolDesc);
}

inline Result DeviceD3D12::CreateBuffer(const BufferDesc& bufferDesc, Buffer*& buffer)
{
    return CreateImplementation<BufferD3D12>(buffer, bufferDesc);
}

inline Result DeviceD3D12::CreateTexture(const TextureDesc& textureDesc, Texture*& texture)
{
    return CreateImplementation<TextureD3D12>(texture, textureDesc);
}

inline Result DeviceD3D12::CreateDescriptor(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    return CreateImplementation<DescriptorD3D12>(bufferView, bufferViewDesc);
}

inline Result DeviceD3D12::CreateDescriptor(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorD3D12>(textureView, textureViewDesc);
}

inline Result DeviceD3D12::CreateDescriptor(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorD3D12>(textureView, textureViewDesc);
}

inline Result DeviceD3D12::CreateDescriptor(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorD3D12>(textureView, textureViewDesc);
}

Result DeviceD3D12::CreateDescriptor(const AccelerationStructure& accelerationStructure, Descriptor*& accelerationStructureView) // TODO: not inline
{
    return CreateImplementation<DescriptorD3D12>(accelerationStructureView, accelerationStructure);
}

inline Result DeviceD3D12::CreateDescriptor(const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return CreateImplementation<DescriptorD3D12>(sampler, samplerDesc);
}

inline Result DeviceD3D12::CreatePipelineLayout(const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return CreateImplementation<PipelineLayoutD3D12>(pipelineLayout, pipelineLayoutDesc);
}

inline Result DeviceD3D12::CreatePipeline(const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineD3D12>(pipeline, graphicsPipelineDesc);
}

inline Result DeviceD3D12::CreatePipeline(const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineD3D12>(pipeline, computePipelineDesc);
}

inline Result DeviceD3D12::CreatePipeline(const RayTracingPipelineDesc& rayTracingPipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineD3D12>(pipeline, rayTracingPipelineDesc);
}

inline Result DeviceD3D12::CreateFence(uint64_t initialValue, Fence*& fence)
{
    return CreateImplementation<FenceD3D12>(fence, initialValue);
}

inline Result DeviceD3D12::CreateFrameBuffer(const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return CreateImplementation<FrameBufferD3D12>(frameBuffer, frameBufferDesc);
}

inline Result DeviceD3D12::CreateQueryPool(const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return CreateImplementation<QueryPoolD3D12>(queryPool, queryPoolDesc);
}

inline Result DeviceD3D12::CreateCommandBuffer(const CommandBufferD3D12Desc& commandBufferDesc, CommandBuffer*& commandBuffer)
{
    return CreateImplementation<CommandBufferD3D12>(commandBuffer, commandBufferDesc);
}

Result DeviceD3D12::CreateBuffer(const BufferD3D12Desc& bufferDesc, Buffer*& buffer) // TODO: not inline
{
    return CreateImplementation<BufferD3D12>(buffer, bufferDesc);
}

inline Result DeviceD3D12::CreateTexture(const TextureD3D12Desc& textureDesc, Texture*& texture)
{
    return CreateImplementation<TextureD3D12>(texture, textureDesc);
}

inline Result DeviceD3D12::CreateMemory(const MemoryD3D12Desc& memoryDesc, Memory*& memory)
{
    return CreateImplementation<MemoryD3D12>(memory, memoryDesc);
}

inline void DeviceD3D12::DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    Deallocate(GetStdAllocator(), (CommandAllocatorD3D12*)&commandAllocator);
}

inline void DeviceD3D12::DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    Deallocate(GetStdAllocator(), (DescriptorPoolD3D12*)&descriptorPool);
}

inline void DeviceD3D12::DestroyBuffer(Buffer& buffer)
{
    Deallocate(GetStdAllocator(), (BufferD3D12*)&buffer);
}

inline void DeviceD3D12::DestroyTexture(Texture& texture)
{
    Deallocate(GetStdAllocator(), (TextureD3D12*)&texture);
}

inline void DeviceD3D12::DestroyDescriptor(Descriptor& descriptor)
{
    Deallocate(GetStdAllocator(), (DescriptorD3D12*)&descriptor);
}

inline void DeviceD3D12::DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    Deallocate(GetStdAllocator(), (PipelineLayoutD3D12*)&pipelineLayout);
}

inline void DeviceD3D12::DestroyPipeline(Pipeline& pipeline)
{
    Deallocate(GetStdAllocator(), (PipelineD3D12*)&pipeline);
}

inline void DeviceD3D12::DestroyFence(Fence& fence)
{
    Deallocate(GetStdAllocator(), (FenceD3D12*)&fence);
}

inline void DeviceD3D12::DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    Deallocate(GetStdAllocator(), (FrameBufferD3D12*)&frameBuffer);
}

inline void DeviceD3D12::DestroyQueryPool(QueryPool& queryPool)
{
    Deallocate(GetStdAllocator(), (QueryPoolD3D12*)&queryPool);
}

inline Result DeviceD3D12::AllocateMemory(const MemoryType memoryType, uint64_t size, Memory*& memory)
{
    return CreateImplementation<MemoryD3D12>(memory, memoryType, size);
}

inline Result DeviceD3D12::BindBufferMemory(const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        Result result = ((BufferD3D12*)memoryBindingDescs[i].buffer)->BindMemory((MemoryD3D12*)memoryBindingDescs[i].memory, memoryBindingDescs[i].offset);
        if (result != Result::SUCCESS)
            return result;
    }

    return Result::SUCCESS;
}

inline Result DeviceD3D12::BindTextureMemory(const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        Result result = ((TextureD3D12*)memoryBindingDescs[i].texture)->BindMemory((MemoryD3D12*)memoryBindingDescs[i].memory, memoryBindingDescs[i].offset);
        if (result != Result::SUCCESS)
            return result;
    }

    return Result::SUCCESS;
}

inline Result DeviceD3D12::BindAccelerationStructureMemory(const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        Result result = ((AccelerationStructureD3D12*)memoryBindingDescs[i].accelerationStructure)->BindMemory(memoryBindingDescs[i].memory, memoryBindingDescs[i].offset);
        if (result != Result::SUCCESS)
            return result;
    }

    return Result::SUCCESS;
}

inline void DeviceD3D12::FreeMemory(Memory& memory)
{
    Deallocate(GetStdAllocator(), (MemoryD3D12*)&memory);
}

inline FormatSupportBits DeviceD3D12::GetFormatSupport(Format format) const
{
    const uint32_t offset = std::min((uint32_t)format, (uint32_t)GetCountOf(D3D_FORMAT_SUPPORT_TABLE) - 1);

    return D3D_FORMAT_SUPPORT_TABLE[offset];
}

inline uint32_t DeviceD3D12::CalculateAllocationNumber(const ResourceGroupDesc& resourceGroupDesc) const
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.CalculateAllocationNumber(resourceGroupDesc);
}

inline Result DeviceD3D12::AllocateAndBindMemory(const ResourceGroupDesc& resourceGroupDesc, nri::Memory** allocations)
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.AllocateAndBindMemory(resourceGroupDesc, allocations);
}

inline Result DeviceD3D12::CreateAccelerationStructure(const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return CreateImplementation<AccelerationStructureD3D12>(accelerationStructure, accelerationStructureDesc);
}

inline Result DeviceD3D12::CreateAccelerationStructure(const AccelerationStructureD3D12Desc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return CreateImplementation<AccelerationStructureD3D12>(accelerationStructure, accelerationStructureDesc);
}

inline void DeviceD3D12::DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
{
    Deallocate(GetStdAllocator(), (AccelerationStructureD3D12*)&accelerationStructure);
}

#include "DeviceD3D12.hpp"
