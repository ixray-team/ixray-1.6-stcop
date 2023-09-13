/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedD3D11.h"
#include "DeviceD3D11.h"
#include "BufferD3D11.h"
#include "CommandQueueD3D11.h"
#include "CommandAllocatorD3D11.h"
#include "DescriptorD3D11.h"
#include "DescriptorSetD3D11.h"
#include "DescriptorPoolD3D11.h"
#include "FenceD3D11.h"
#include "FrameBufferD3D11.h"
#include "MemoryD3D11.h"
#include "PipelineLayoutD3D11.h"
#include "PipelineD3D11.h"
#include "QueryPoolD3D11.h"
#include "SwapChainD3D11.h"
#include "TextureD3D11.h"

#include <dxgidebug.h>
#include <d3d12.h>

using namespace nri;

Result CreateDeviceD3D11(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device)
{
    Log log(GraphicsAPI::D3D11, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    ComPtr<IDXGIFactory4> factory;
    HRESULT hr = CreateDXGIFactory2(0, IID_PPV_ARGS(&factory));
    RETURN_ON_BAD_HRESULT(log, hr, "Can't create D3D11 device. CreateDXGIFactory2() failed. (result: %d)", (int32_t)hr);

    ComPtr<IDXGIAdapter> adapter;
    if (deviceCreationDesc.physicalDeviceGroup != nullptr)
    {
        LUID luid = *(LUID*)&deviceCreationDesc.physicalDeviceGroup->luid;
        hr = factory->EnumAdapterByLuid(luid, IID_PPV_ARGS(&adapter));
        RETURN_ON_BAD_HRESULT(log, hr, "Can't create D3D11 device. IDXGIFactory4::EnumAdapterByLuid() failed. (result: %d)", (int32_t)hr);
    }
    else
    {
        hr = factory->EnumAdapters(0, &adapter);
        RETURN_ON_BAD_HRESULT(log, hr, "Can't create D3D11 device. IDXGIFactory4::EnumAdapters() failed. (result: %d)", (int32_t)hr);
    }

    DeviceD3D11* implementation = Allocate<DeviceD3D11>(allocator, log, allocator);
    const nri::Result result = implementation->Create(deviceCreationDesc, adapter, nullptr, nullptr);

    if (result == nri::Result::SUCCESS)
    {
        device = (DeviceBase*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(allocator, implementation);

    return result;
}

DeviceD3D11::DeviceD3D11(const Log& log, StdAllocator<uint8_t>& stdAllocator) :
    DeviceBase(log, stdAllocator)
    , m_CommandQueues(GetStdAllocator())
{}

DeviceD3D11::~DeviceD3D11()
{
    DeleteCriticalSection(&m_CriticalSection);
}

bool DeviceD3D11::GetOutput(Display* display, ComPtr<IDXGIOutput>& output) const
{
    if (display == nullptr)
        return false;

    const uint32_t index = (*(uint32_t*)&display) - 1;
    const HRESULT result = m_Adapter->EnumOutputs(index, &output);

    return SUCCEEDED(result);
}

Result DeviceD3D11::Create(const DeviceCreationDesc& deviceCreationDesc, IDXGIAdapter* adapter, ID3D11Device* device, AGSContext* agsContext)
{
    m_SkipLiveObjectsReporting = deviceCreationDesc.skipLiveObjectsReporting;
    m_Adapter = adapter;

    DXGI_ADAPTER_DESC desc = {};
    adapter->GetDesc(&desc);

    const Vendor vendor = GetVendorFromID(desc.VendorId);

    m_Ext.Create(GetLog(), vendor, agsContext, device != nullptr);

    m_Device.ptr = (ID3D11Device5*)device;
    if (!device)
    {
        const UINT flags = deviceCreationDesc.enableAPIValidation ? D3D11_CREATE_DEVICE_DEBUG : 0;
        const std::array<D3D_FEATURE_LEVEL, 2> levels =
        {
            D3D_FEATURE_LEVEL_11_1,
            D3D_FEATURE_LEVEL_11_0
        };

        if (m_Ext.IsAGSAvailable())
        {
            device = m_Ext.CreateDeviceUsingAGS(adapter, levels.data(), levels.size(), flags);
            if (device == nullptr)
                return Result::FAILURE;
        }
        else
        {
            HRESULT hr = D3D11CreateDevice(adapter, D3D_DRIVER_TYPE_UNKNOWN, nullptr, flags, levels.data(), (uint32_t)levels.size(), D3D11_SDK_VERSION, (ID3D11Device**)&m_Device.ptr, nullptr, nullptr);

            if (flags && (uint32_t)hr == 0x887a002d)
                hr = D3D11CreateDevice(adapter, D3D_DRIVER_TYPE_UNKNOWN, nullptr, 0, &levels[0], (uint32_t)levels.size(), D3D11_SDK_VERSION, (ID3D11Device**)&m_Device.ptr, nullptr, nullptr);

            RETURN_ON_BAD_HRESULT(GetLog(), hr, "D3D11CreateDevice() - FAILED!");
        }
    }
    else
        device->AddRef();

    InitVersionedDevice(deviceCreationDesc.D3D11CommandBufferEmulation);
    InitVersionedContext();
    FillLimits(deviceCreationDesc.enableAPIValidation, vendor);

    for (uint32_t i = 0; i < COMMAND_QUEUE_TYPE_NUM; i++)
        m_CommandQueues.emplace_back(*this);

    if (FillFunctionTable(m_CoreInterface) != Result::SUCCESS)
        REPORT_ERROR(GetLog(), "Failed to get 'CoreInterface' interface in DeviceD3D11().");

    return Result::SUCCESS;
}

void DeviceD3D11::InitVersionedDevice(bool isDeferredContextsEmulationRequested)
{
    m_Device.ext = &m_Ext;

    ComPtr<ID3D11Device5> versionedDevice;
    HRESULT hr = m_Device->QueryInterface(__uuidof(ID3D11Device5), (void**)&versionedDevice);
    m_Device.version = 5;
    if (FAILED(hr))
    {
        REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Device5) - FAILED!");
        hr = m_Device->QueryInterface(__uuidof(ID3D11Device4), (void**)&versionedDevice);
        m_Device.version = 4;
        if (FAILED(hr))
        {
            REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Device4) - FAILED!");
            hr = m_Device->QueryInterface(__uuidof(ID3D11Device3), (void**)&versionedDevice);
            m_Device.version = 3;
            if (FAILED(hr))
            {
                REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Device3) - FAILED!");
                hr = m_Device->QueryInterface(__uuidof(ID3D11Device2), (void**)&versionedDevice);
                m_Device.version = 2;
                if (FAILED(hr))
                {
                    REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Device2) - FAILED!");
                    hr = m_Device->QueryInterface(__uuidof(ID3D11Device1), (void**)&versionedDevice);
                    m_Device.version = 1;
                    if (FAILED(hr))
                    {
                        REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Device1) - FAILED!");
                        m_Device.version = 0;
                        versionedDevice = m_Device.ptr;
                    }
                }
            }
        }
    }

    m_Device.ptr = versionedDevice;

    D3D11_FEATURE_DATA_THREADING threadingCaps = {};
    hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_THREADING, &threadingCaps, sizeof(threadingCaps));

    if (FAILED(hr) || !threadingCaps.DriverConcurrentCreates)
        REPORT_WARNING(GetLog(), "Concurrent resource creation is not supported by the driver!");

    m_Device.isDeferredContextEmulated = !m_Ext.IsNvAPIAvailable() || isDeferredContextsEmulationRequested;

    if (!threadingCaps.DriverCommandLists)
    {
        REPORT_WARNING(GetLog(), "Deferred Contexts are not supported by the driver and will be emulated!");
        m_Device.isDeferredContextEmulated = true;
    }
}

void DeviceD3D11::InitVersionedContext()
{
    m_ImmediateContext.ext = &m_Ext;

    m_Device.ptr->GetImmediateContext((ID3D11DeviceContext**)&m_ImmediateContext.ptr);

    ComPtr<ID3D11DeviceContext4> versionedImmediateContext;
    HRESULT hr = m_ImmediateContext->QueryInterface(__uuidof(ID3D11DeviceContext4), (void**)&versionedImmediateContext);
    m_ImmediateContext.version = 4;
    if (FAILED(hr))
    {
        REPORT_WARNING(GetLog(), "QueryInterface(ID3D11DeviceContext4) - FAILED!");
        hr = m_ImmediateContext->QueryInterface(__uuidof(ID3D11DeviceContext3), (void**)&versionedImmediateContext);
        m_ImmediateContext.version = 3;
        if (FAILED(hr))
        {
            REPORT_WARNING(GetLog(), "QueryInterface(ID3D11DeviceContext3) - FAILED!");
            hr = m_ImmediateContext->QueryInterface(__uuidof(ID3D11DeviceContext2), (void**)&versionedImmediateContext);
            m_ImmediateContext.version = 2;
            if (FAILED(hr))
            {
                REPORT_WARNING(GetLog(), "QueryInterface(ID3D11DeviceContext2) - FAILED!");
                hr = m_ImmediateContext->QueryInterface(__uuidof(ID3D11DeviceContext1), (void**)&versionedImmediateContext);
                m_ImmediateContext.version = 1;
                if (FAILED(hr))
                {
                    REPORT_WARNING(GetLog(), "QueryInterface(ID3D11DeviceContext1) - FAILED!");
                    m_ImmediateContext.version = 0;
                    versionedImmediateContext = m_ImmediateContext.ptr;
                }
            }
        }
    }

    m_ImmediateContext.ptr = versionedImmediateContext;

    InitializeCriticalSection(&m_CriticalSection);

    hr = m_ImmediateContext->QueryInterface(IID_PPV_ARGS(&m_ImmediateContext.multiThread));
    if (FAILED(hr))
    {
        REPORT_WARNING(GetLog(), "QueryInterface(ID3D11Multithread) - FAILED! Critical section will be used instead!");
        m_ImmediateContext.criticalSection = &m_CriticalSection;
    }
    else
        m_ImmediateContext.multiThread->SetMultithreadProtected(true);
}

void DeviceD3D11::FillLimits(bool isValidationEnabled, Vendor vendor)
{
    D3D11_FEATURE_DATA_D3D11_OPTIONS options = {};
    HRESULT hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_D3D11_OPTIONS, &options, sizeof(options));

    D3D11_FEATURE_DATA_D3D11_OPTIONS1 options1 = {};
    hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_D3D11_OPTIONS1, &options1, sizeof(options1));

    D3D11_FEATURE_DATA_D3D11_OPTIONS2 options2 = {};
    hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_D3D11_OPTIONS2, &options2, sizeof(options2));

    D3D11_FEATURE_DATA_D3D11_OPTIONS3 options3 = {};
    hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_D3D11_OPTIONS3, &options3, sizeof(options3));

    D3D11_FEATURE_DATA_D3D11_OPTIONS4 options4 = {};
    hr = m_Device->CheckFeatureSupport(D3D11_FEATURE_D3D11_OPTIONS4, &options4, sizeof(options4));

    uint64_t timestampFrequency = 0;
    {
        D3D11_QUERY_DESC queryDesc = {};
        queryDesc.Query = D3D11_QUERY_TIMESTAMP_DISJOINT;

        ComPtr<ID3D11Query> query;
        hr = m_Device->CreateQuery(&queryDesc, &query);
        if (SUCCEEDED(hr))
        {
            m_ImmediateContext->Begin(query);
            m_ImmediateContext->End(query);

            D3D11_QUERY_DATA_TIMESTAMP_DISJOINT data = {};
            while ( m_ImmediateContext->GetData(query, &data, sizeof(D3D11_QUERY_DATA_TIMESTAMP_DISJOINT), 0) == S_FALSE )
                ;

            timestampFrequency = data.Frequency;
        }
    }

    m_Desc.graphicsAPI = GraphicsAPI::D3D11;
    m_Desc.vendor = vendor;
    m_Desc.nriVersionMajor = NRI_VERSION_MAJOR;
    m_Desc.nriVersionMinor = NRI_VERSION_MINOR;

    m_Desc.viewportMaxNum = D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE;
    m_Desc.viewportSubPixelBits = D3D11_SUBPIXEL_FRACTIONAL_BIT_COUNT;
    m_Desc.viewportBoundsRange[0] = D3D11_VIEWPORT_BOUNDS_MIN;
    m_Desc.viewportBoundsRange[1] = D3D11_VIEWPORT_BOUNDS_MAX;

    m_Desc.frameBufferMaxDim = D3D11_REQ_RENDER_TO_BUFFER_WINDOW_WIDTH;
    m_Desc.frameBufferLayerMaxNum = D3D11_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION;
    m_Desc.framebufferColorAttachmentMaxNum = D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT;

    m_Desc.frameBufferColorSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.frameBufferDepthSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.frameBufferStencilSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.frameBufferNoAttachmentsSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.textureColorSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.textureIntegerSampleMaxNum = 1;
    m_Desc.textureDepthSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.textureStencilSampleMaxNum = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
    m_Desc.storageTextureSampleMaxNum = 1;

    m_Desc.texture1DMaxDim = D3D11_REQ_TEXTURE1D_U_DIMENSION;
    m_Desc.texture2DMaxDim = D3D11_REQ_TEXTURE2D_U_OR_V_DIMENSION;
    m_Desc.texture3DMaxDim = D3D11_REQ_TEXTURE3D_U_V_OR_W_DIMENSION;
    m_Desc.textureArrayMaxDim = D3D11_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION;
    m_Desc.texelBufferMaxDim = (1 << D3D11_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP) - 1;

    m_Desc.memoryAllocationMaxNum = 0xFFFFFFFF;
    m_Desc.samplerAllocationMaxNum = D3D11_REQ_SAMPLER_OBJECT_COUNT_PER_DEVICE;
    m_Desc.uploadBufferTextureRowAlignment = D3D12_TEXTURE_DATA_PITCH_ALIGNMENT;
    m_Desc.uploadBufferTextureSliceAlignment = D3D12_TEXTURE_DATA_PLACEMENT_ALIGNMENT;
    m_Desc.typedBufferOffsetAlignment = D3D11_RAW_UAV_SRV_BYTE_ALIGNMENT;
    m_Desc.constantBufferOffsetAlignment = D3D12_CONSTANT_BUFFER_DATA_PLACEMENT_ALIGNMENT;
    m_Desc.constantBufferMaxRange = D3D11_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT * 16;
    m_Desc.storageBufferOffsetAlignment = D3D11_RAW_UAV_SRV_BYTE_ALIGNMENT;
    m_Desc.storageBufferMaxRange = (1 << D3D11_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP) - 1;
    m_Desc.bufferTextureGranularity = 1;
    m_Desc.bufferMaxSize = D3D11_REQ_RESOURCE_SIZE_IN_MEGABYTES_EXPRESSION_C_TERM * 1024ull * 1024ull;
    m_Desc.pushConstantsMaxSize = D3D11_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT * 16;

    m_Desc.boundDescriptorSetMaxNum = D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;
    m_Desc.perStageDescriptorSamplerMaxNum = D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT;
    m_Desc.perStageDescriptorConstantBufferMaxNum = D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT;
    m_Desc.perStageDescriptorStorageBufferMaxNum = m_Device.version >= 1 ? D3D11_1_UAV_SLOT_COUNT : D3D11_PS_CS_UAV_REGISTER_COUNT;
    m_Desc.perStageDescriptorTextureMaxNum = D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;
    m_Desc.perStageDescriptorStorageTextureMaxNum = m_Device.version >= 1 ? D3D11_1_UAV_SLOT_COUNT : D3D11_PS_CS_UAV_REGISTER_COUNT;
    m_Desc.perStageResourceMaxNum = D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT;

    m_Desc.descriptorSetSamplerMaxNum = m_Desc.perStageDescriptorSamplerMaxNum;
    m_Desc.descriptorSetConstantBufferMaxNum = m_Desc.perStageDescriptorConstantBufferMaxNum;
    m_Desc.descriptorSetStorageBufferMaxNum = m_Desc.perStageDescriptorStorageBufferMaxNum;
    m_Desc.descriptorSetTextureMaxNum = m_Desc.perStageDescriptorTextureMaxNum;
    m_Desc.descriptorSetStorageTextureMaxNum = m_Desc.perStageDescriptorStorageTextureMaxNum;

    m_Desc.vertexShaderAttributeMaxNum = D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT;
    m_Desc.vertexShaderStreamMaxNum = D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT;
    m_Desc.vertexShaderOutputComponentMaxNum = D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT * 4;

    m_Desc.tessControlShaderGenerationMaxLevel = D3D11_HS_MAXTESSFACTOR_UPPER_BOUND;
    m_Desc.tessControlShaderPatchPointMaxNum = D3D11_IA_PATCH_MAX_CONTROL_POINT_COUNT;
    m_Desc.tessControlShaderPerVertexInputComponentMaxNum = D3D11_HS_CONTROL_POINT_PHASE_INPUT_REGISTER_COUNT * D3D11_HS_CONTROL_POINT_REGISTER_COMPONENTS;
    m_Desc.tessControlShaderPerVertexOutputComponentMaxNum = D3D11_HS_CONTROL_POINT_PHASE_OUTPUT_REGISTER_COUNT * D3D11_HS_CONTROL_POINT_REGISTER_COMPONENTS;
    m_Desc.tessControlShaderPerPatchOutputComponentMaxNum = D3D11_HS_OUTPUT_PATCH_CONSTANT_REGISTER_SCALAR_COMPONENTS;
    m_Desc.tessControlShaderTotalOutputComponentMaxNum = m_Desc.tessControlShaderPatchPointMaxNum * m_Desc.tessControlShaderPerVertexOutputComponentMaxNum + m_Desc.tessControlShaderPerPatchOutputComponentMaxNum;

    m_Desc.tessEvaluationShaderInputComponentMaxNum = D3D11_DS_INPUT_CONTROL_POINT_REGISTER_COUNT * D3D11_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENTS;
    m_Desc.tessEvaluationShaderOutputComponentMaxNum = D3D11_DS_INPUT_CONTROL_POINT_REGISTER_COUNT * D3D11_DS_INPUT_CONTROL_POINT_REGISTER_COMPONENTS;

    m_Desc.geometryShaderInvocationMaxNum = D3D11_GS_MAX_INSTANCE_COUNT;
    m_Desc.geometryShaderInputComponentMaxNum = D3D11_GS_INPUT_REGISTER_COUNT * D3D11_GS_INPUT_REGISTER_COMPONENTS;
    m_Desc.geometryShaderOutputComponentMaxNum = D3D11_GS_OUTPUT_REGISTER_COUNT * D3D11_GS_INPUT_REGISTER_COMPONENTS;
    m_Desc.geometryShaderOutputVertexMaxNum = D3D11_GS_MAX_OUTPUT_VERTEX_COUNT_ACROSS_INSTANCES;
    m_Desc.geometryShaderTotalOutputComponentMaxNum = D3D11_REQ_GS_INVOCATION_32BIT_OUTPUT_COMPONENT_LIMIT;

    m_Desc.fragmentShaderInputComponentMaxNum = D3D11_PS_INPUT_REGISTER_COUNT * D3D11_PS_INPUT_REGISTER_COMPONENTS;
    m_Desc.fragmentShaderOutputAttachmentMaxNum = D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT;
    m_Desc.fragmentShaderDualSourceAttachmentMaxNum = 1;
    m_Desc.fragmentShaderCombinedOutputResourceMaxNum = D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT + D3D11_PS_CS_UAV_REGISTER_COUNT;

    m_Desc.computeShaderSharedMemoryMaxSize = D3D11_CS_THREAD_LOCAL_TEMP_REGISTER_POOL;
    m_Desc.computeShaderWorkGroupMaxNum[0] = D3D11_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_Desc.computeShaderWorkGroupMaxNum[1] = D3D11_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_Desc.computeShaderWorkGroupMaxNum[2] = D3D11_CS_DISPATCH_MAX_THREAD_GROUPS_PER_DIMENSION;
    m_Desc.computeShaderWorkGroupInvocationMaxNum = D3D11_CS_THREAD_GROUP_MAX_THREADS_PER_GROUP;
    m_Desc.computeShaderWorkGroupMaxDim[0] = D3D11_CS_THREAD_GROUP_MAX_X;
    m_Desc.computeShaderWorkGroupMaxDim[1] = D3D11_CS_THREAD_GROUP_MAX_Y;
    m_Desc.computeShaderWorkGroupMaxDim[2] = D3D11_CS_THREAD_GROUP_MAX_Z;

    m_Desc.timestampFrequencyHz = timestampFrequency;
    m_Desc.subPixelPrecisionBits = D3D11_SUBPIXEL_FRACTIONAL_BIT_COUNT;
    m_Desc.subTexelPrecisionBits = D3D11_SUBTEXEL_FRACTIONAL_BIT_COUNT;
    m_Desc.mipmapPrecisionBits = D3D11_MIP_LOD_FRACTIONAL_BIT_COUNT;
    m_Desc.drawIndexedIndex16ValueMax = D3D11_16BIT_INDEX_STRIP_CUT_VALUE;
    m_Desc.drawIndexedIndex32ValueMax = D3D11_32BIT_INDEX_STRIP_CUT_VALUE;
    m_Desc.drawIndirectMaxNum = (1ull << D3D11_REQ_DRAWINDEXED_INDEX_COUNT_2_TO_EXP) - 1;
    m_Desc.samplerLodBiasMin = D3D11_MIP_LOD_BIAS_MIN;
    m_Desc.samplerLodBiasMax = D3D11_MIP_LOD_BIAS_MAX;
    m_Desc.samplerAnisotropyMax = D3D11_DEFAULT_MAX_ANISOTROPY;
    m_Desc.texelOffsetMin = D3D11_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE;
    m_Desc.texelOffsetMax = D3D11_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE;
    m_Desc.texelGatherOffsetMin = D3D11_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE;
    m_Desc.texelGatherOffsetMax = D3D11_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE;
    m_Desc.clipDistanceMaxNum = D3D11_CLIP_OR_CULL_DISTANCE_COUNT;
    m_Desc.cullDistanceMaxNum = D3D11_CLIP_OR_CULL_DISTANCE_COUNT;
    m_Desc.combinedClipAndCullDistanceMaxNum = D3D11_CLIP_OR_CULL_DISTANCE_COUNT;
    m_Desc.rayTracingShaderGroupIdentifierSize = 0;
    m_Desc.rayTracingShaderTableAligment = 0;
    m_Desc.rayTracingShaderTableMaxStride = 0;
    m_Desc.rayTracingShaderRecursionMaxDepth = 0;
    m_Desc.rayTracingGeometryObjectMaxNum = 0;
    m_Desc.conservativeRasterTier = (uint8_t)options2.ConservativeRasterizationTier;
    m_Desc.physicalDeviceNum = 1; // TODO: is there a way to query it in D3D11?

    m_Desc.isAPIValidationEnabled = isValidationEnabled;
    m_Desc.isTextureFilterMinMaxSupported = options1.MinMaxFiltering != 0;
    m_Desc.isLogicOpSupported = options.OutputMergerLogicOp != 0;
    m_Desc.isDepthBoundsTestSupported = vendor == Vendor::NVIDIA || vendor == Vendor::AMD;
    m_Desc.isProgrammableSampleLocationsSupported = vendor == Vendor::NVIDIA;
    m_Desc.isComputeQueueSupported = false;
    m_Desc.isCopyQueueSupported = false;
    m_Desc.isCopyQueueTimestampSupported = false;
    m_Desc.isRegisterAliasingSupported = false;
    m_Desc.isSubsetAllocationSupported = true;
}

template<typename Implementation, typename Interface, typename ConstructorArg, typename ... Args>
nri::Result DeviceD3D11::CreateImplementationWithNonEmptyConstructor(Interface*& entity, ConstructorArg&& constructorArg, const Args&... args)
{
    Implementation* implementation = Allocate<Implementation>(GetStdAllocator(), constructorArg);
    const nri::Result res = implementation->Create(args...);

    if (res == nri::Result::SUCCESS)
    {
        entity = (Interface*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return res;
}

//================================================================================================================
// DeviceBase
//================================================================================================================

void DeviceD3D11::Destroy()
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

inline Result DeviceD3D11::CreateSwapChain(const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return CreateImplementationWithNonEmptyConstructor<SwapChainD3D11>(swapChain, *this, swapChainDesc);
}

inline void DeviceD3D11::DestroySwapChain(SwapChain& swapChain)
{
    Deallocate(GetStdAllocator(), (SwapChainD3D11*)&swapChain);
}

inline Result DeviceD3D11::GetDisplays(Display** displays, uint32_t& displayNum)
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

inline Result DeviceD3D11::GetDisplaySize(Display& display, uint16_t& width, uint16_t& height)
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

inline Result DeviceD3D11::GetCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    commandQueue = (CommandQueue*)&m_CommandQueues[(uint32_t)commandQueueType];

    return Result::SUCCESS;
}

inline Result DeviceD3D11::CreateCommandAllocator(const CommandQueue& commandQueue, CommandAllocator*& commandAllocator)
{
    MaybeUnused(commandQueue);

    commandAllocator = (CommandAllocator*)Allocate<CommandAllocatorD3D11>(GetStdAllocator(), *this);

    return Result::SUCCESS;
}

inline Result DeviceD3D11::CreateDescriptorPool(const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorPoolD3D11>(descriptorPool, *this, descriptorPoolDesc);
}

inline Result DeviceD3D11::CreateBuffer(const BufferDesc& bufferDesc, Buffer*& buffer)
{
    buffer = (Buffer*)Allocate<BufferD3D11>(GetStdAllocator(), *this, bufferDesc);

    return Result::SUCCESS;
}

inline Result DeviceD3D11::CreateTexture(const TextureDesc& textureDesc, Texture*& texture)
{
    texture = (Texture*)Allocate<TextureD3D11>(GetStdAllocator(), *this, textureDesc);

    return Result::SUCCESS;
}

inline Result DeviceD3D11::CreateDescriptor(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorD3D11>(bufferView, *this, bufferViewDesc);
}

inline Result DeviceD3D11::CreateDescriptor(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorD3D11>(textureView, *this, textureViewDesc);
}

inline Result DeviceD3D11::CreateDescriptor(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorD3D11>(textureView, *this, textureViewDesc);
}

inline Result DeviceD3D11::CreateDescriptor(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorD3D11>(textureView, *this, textureViewDesc);
}

inline Result DeviceD3D11::CreateDescriptor(const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return CreateImplementationWithNonEmptyConstructor<DescriptorD3D11>(sampler, *this, samplerDesc);
}

inline Result DeviceD3D11::CreatePipelineLayout(const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    PipelineLayoutD3D11* implementation = Allocate<PipelineLayoutD3D11>(GetStdAllocator(), *this);
    const nri::Result res = implementation->Create(pipelineLayoutDesc);

    if (res == nri::Result::SUCCESS)
    {
        pipelineLayout = (PipelineLayout*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return res;
}

inline Result DeviceD3D11::CreatePipeline(const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    PipelineD3D11* implementation = Allocate<PipelineD3D11>(GetStdAllocator(), *this);
    const nri::Result res = implementation->Create(graphicsPipelineDesc);

    if (res == nri::Result::SUCCESS)
    {
        pipeline = (Pipeline*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return res;
}

inline Result DeviceD3D11::CreatePipeline(const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    PipelineD3D11* implementation = Allocate<PipelineD3D11>(GetStdAllocator(), *this);
    const nri::Result res = implementation->Create(computePipelineDesc);

    if (res == nri::Result::SUCCESS)
    {
        pipeline = (Pipeline*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return res;
}

inline Result DeviceD3D11::CreateFrameBuffer(const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    frameBuffer = (FrameBuffer*)Allocate<FrameBufferD3D11>(GetStdAllocator(), *this, frameBufferDesc);

    return Result::SUCCESS;
}

inline Result DeviceD3D11::CreateQueryPool(const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return CreateImplementationWithNonEmptyConstructor<QueryPoolD3D11>(queryPool, *this, queryPoolDesc);
}

inline Result DeviceD3D11::CreateFence(uint64_t initialValue, Fence*& fence)
{
    FenceD3D11* implementation = Allocate<FenceD3D11>(GetStdAllocator(), *this);
    const nri::Result res = implementation->Create(initialValue);

    if (res == nri::Result::SUCCESS)
    {
        fence = (Fence*)implementation;
        return nri::Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);

    return res;
}

inline void DeviceD3D11::DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    Deallocate(GetStdAllocator(), (CommandAllocatorD3D11*)&commandAllocator);
}

inline void DeviceD3D11::DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    Deallocate(GetStdAllocator(), (DescriptorPoolD3D11*)&descriptorPool);
}

inline void DeviceD3D11::DestroyBuffer(Buffer& buffer)
{
    Deallocate(GetStdAllocator(), (BufferD3D11*)&buffer);
}

inline void DeviceD3D11::DestroyTexture(Texture& texture)
{
    Deallocate(GetStdAllocator(), (TextureD3D11*)&texture);
}

inline void DeviceD3D11::DestroyDescriptor(Descriptor& descriptor)
{
    Deallocate(GetStdAllocator(), (DescriptorD3D11*)&descriptor);
}

inline void DeviceD3D11::DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    Deallocate(GetStdAllocator(), (PipelineLayoutD3D11*)&pipelineLayout);
}

inline void DeviceD3D11::DestroyPipeline(Pipeline& pipeline)
{
    Deallocate(GetStdAllocator(), (PipelineD3D11*)&pipeline);
}

inline void DeviceD3D11::DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    Deallocate(GetStdAllocator(), (FrameBufferD3D11*)&frameBuffer);
}

inline void DeviceD3D11::DestroyQueryPool(QueryPool& queryPool)
{
    Deallocate(GetStdAllocator(), (QueryPoolD3D11*)&queryPool);
}

inline void DeviceD3D11::DestroyFence(Fence& fence)
{
    Deallocate(GetStdAllocator(), (FenceD3D11*)&fence);
}

inline Result DeviceD3D11::AllocateMemory(MemoryType memoryType, uint64_t size, Memory*& memory)
{
    MaybeUnused(size);

    memory = (Memory*)Allocate<MemoryD3D11>(GetStdAllocator(), *this, memoryType);

    return Result::SUCCESS;
}

inline Result DeviceD3D11::BindBufferMemory(const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        const BufferMemoryBindingDesc& desc = memoryBindingDescs[i];
        Result res = ((BufferD3D11*)desc.buffer)->Create(*(MemoryD3D11*)desc.memory);
        if (res != Result::SUCCESS)
            return res;
    }

    return Result::SUCCESS;
}

inline Result DeviceD3D11::BindTextureMemory(const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        const TextureMemoryBindingDesc& desc = memoryBindingDescs[i];
        Result res = ((TextureD3D11*)desc.texture)->Create((MemoryD3D11*)desc.memory);
        if (res != Result::SUCCESS)
            return res;
    }

    return Result::SUCCESS;
}

inline void DeviceD3D11::FreeMemory(Memory& memory)
{
    Deallocate(GetStdAllocator(), (MemoryD3D11*)&memory);
}

inline FormatSupportBits DeviceD3D11::GetFormatSupport(Format format) const
{
    const uint32_t offset = std::min((uint32_t)format, (uint32_t)GetCountOf(D3D_FORMAT_SUPPORT_TABLE) - 1);

    return D3D_FORMAT_SUPPORT_TABLE[offset];
}

inline uint32_t DeviceD3D11::CalculateAllocationNumber(const ResourceGroupDesc& resourceGroupDesc) const
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.CalculateAllocationNumber(resourceGroupDesc);
}

inline Result DeviceD3D11::AllocateAndBindMemory(const ResourceGroupDesc& resourceGroupDesc, nri::Memory** allocations)
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.AllocateAndBindMemory(resourceGroupDesc, allocations);
}

#include "DeviceD3D11.hpp"
