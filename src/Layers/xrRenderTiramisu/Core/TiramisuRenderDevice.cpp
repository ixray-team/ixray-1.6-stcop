#include "TiramisuRenderDevice.h"
#include "Extensions/NRIDeviceCreation.h"
#include "Extensions/NRIStreamer.h"
#include "TiramisuRenderAdapterSelection.h"
#include "../../../xrCore/RenderDebugPolicy.h"
#include "../../../xrCore/RenderDocIntegration.h"

#include <vector>

TiramisuRenderDevice GRenderDevice;

TiramisuRenderDevice::TiramisuRenderDevice() : CoreInterface({}), SwapChainInterface({}), HelperInterface({})
{
}

TiramisuRenderDevice::~TiramisuRenderDevice()
= default;

void TiramisuRenderDevice::Initialize()
{
    CheckIsGameThread();

    nri::GraphicsAPI RequestedApi = nri::GraphicsAPI::VK;
    if (Core.Params && strstr(Core.Params, "-dx12"))
        RequestedApi = nri::GraphicsAPI::D3D12;

    Initialize(RequestedApi, {});
}

void TiramisuRenderDevice::Initialize(
    const nri::GraphicsAPI InGraphicsApi,
    const nri::CallbackInterface& CallbackInterface)
{
    CheckIsGameThread();
    R_ASSERT2(!IsInitialized(), "Tiramisu NRI device is already initialized");
    GraphicsApi = InGraphicsApi;

    nri::AdapterDesc CurrentAdapterDescription = {};
    R_ASSERT2(FindBestAdapterDescription(GraphicsApi, CurrentAdapterDescription),
        "Tiramisu could not find a compatible graphics adapter");
    Msg("* Tiramisu adapter: %s (%llu MB dedicated)",
        CurrentAdapterDescription.name,
        static_cast<unsigned long long>(
            CurrentAdapterDescription.videoMemorySize / (1024ull * 1024ull)));
    // Settings
    constexpr bool D3D11_ENABLE_COMMAND_BUFFER_EMULATION = false;
    constexpr bool D3D12_DISABLE_ENHANCED_BARRIERS = false;
    // Device
    nri::DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.graphicsAPI = GraphicsApi;
    deviceCreationDesc.callbackInterface = CallbackInterface;
	const FRenderDebugPolicy DebugPolicy = ResolveRenderDebugPolicy(
		Core.Params ? Core.Params : "", xrRenderDoc::IsLoaded());
	deviceCreationDesc.enableGraphicsAPIValidation =
		DebugPolicy.GraphicsApiValidation;
	deviceCreationDesc.enableNRIValidation = DebugPolicy.NriValidation;
	if (DebugPolicy.ValidationSuppressedByRenderDoc)
	{
		Msg("* Tiramisu: RenderDoc is active; conflicting graphics API and "
			"NRI validation layers are disabled. Shader debug info from "
			"-rdbg remains enabled.");
	}
	else if (DebugPolicy.RenderDocActive &&
		DebugPolicy.ForceRenderDocValidation)
	{
		Msg("! Tiramisu: -renderdoc-validation forces graphics API and NRI "
			"validation together with RenderDoc; this mode may be unstable.");
	}
    deviceCreationDesc.enableD3D11CommandBufferEmulation = D3D11_ENABLE_COMMAND_BUFFER_EMULATION;
    deviceCreationDesc.disableD3D12EnhancedBarriers = D3D12_DISABLE_ENHANCED_BARRIERS;
    deviceCreationDesc.vkBindingOffsets = VK_BINDING_OFFSETS;
    deviceCreationDesc.adapterDesc = &CurrentAdapterDescription;
    deviceCreationDesc.allocationCallbacks = AllocationCallbacks;
    NRI_CHECK(nri::nriCreateDevice(deviceCreationDesc, Device));
	
    // CoreInterface
    NRI_CHECK(nri::nriGetInterface(*Device, NRI_INTERFACE(nri::CoreInterface), &CoreInterface));
    NRI_CHECK(nri::nriGetInterface(*Device, NRI_INTERFACE(nri::SwapChainInterface), &SwapChainInterface));
    NRI_CHECK(nri::nriGetInterface(*Device, NRI_INTERFACE(nri::HelperInterface), &HelperInterface));
	// ExtensionInterface
	NRI_CHECK(nri::nriGetInterface(*Device, NRI_INTERFACE(nri::ImguiInterface), &ImGuiInterface));
    // Command queue
    NRI_CHECK(CoreInterface.GetQueue(*Device, nri::QueueType::GRAPHICS, 0, GraphicsQueue));
	if (CurrentAdapterDescription.queueNum[static_cast<u32>(
	        nri::QueueType::COMPUTE)] != 0)
	{
		NRI_CHECK(CoreInterface.GetQueue(*Device, nri::QueueType::COMPUTE, 0,
			ComputeQueue));
	}
	if (CurrentAdapterDescription.queueNum[static_cast<u32>(
	        nri::QueueType::COPY)] != 0)
	{
		NRI_CHECK(CoreInterface.GetQueue(*Device, nri::QueueType::COPY, 0,
			CopyQueue));
	}
	Msg("* Tiramisu queues: graphics=yes, async-compute=%s, copy=%s",
		ComputeQueue ? "yes" : "fallback-to-graphics",
		CopyQueue ? "yes" : "fallback-to-graphics");
	
	NRI_CHECK(nri::nriGetInterface(*Device, NRI_INTERFACE(nri::StreamerInterface), &StreamerInterface));

	// Create streamer
	nri::StreamerDesc StreamerDescription = {};
	StreamerDescription.dynamicBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDescription.dynamicBufferDesc = { 0, 0, nri::BufferUsageBits::VERTEX_BUFFER | nri::BufferUsageBits::INDEX_BUFFER };
	StreamerDescription.constantBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDescription.queuedFrameNum = 3;
	NRI_CHECK(StreamerInterface.CreateStreamer(*Device, StreamerDescription, Streamer));

	DeviceDescription = CoreInterface.GetDeviceDesc(*Device);
	
}

void TiramisuRenderDevice::Destroy()
{
    CheckIsGameThread();
    VERIFY(!IsRenderThreadRunning());
    if (!IsInitialized())
        return;

    if (Streamer)
        StreamerInterface.DestroyStreamer(Streamer);

    Streamer = nullptr;
    GraphicsQueue = nullptr;
    ComputeQueue = nullptr;
    CopyQueue = nullptr;
    CoreInterface = {};
    SwapChainInterface = {};
    HelperInterface = {};
    ImGuiInterface = {};
    StreamerInterface = {};
    DeviceDescription = {};
    nri::nriDestroyDevice(Device);
    Device = nullptr;
}
bool TiramisuRenderDevice::FindBestAdapterDescription(const nri::GraphicsAPI GraphicsApi,
    nri::AdapterDesc& Result)
{
	u32 AdapterDescriptionCount = 0;
	if (nri::nriEnumerateAdapters(nullptr, AdapterDescriptionCount) != nri::Result::SUCCESS ||
        AdapterDescriptionCount == 0)
    {
        return false;
    }

    xr_vector<nri::AdapterDesc> AdapterDescriptions(AdapterDescriptionCount);
    if (nri::nriEnumerateAdapters(AdapterDescriptions.data(), AdapterDescriptionCount) !=
        nri::Result::SUCCESS)
    {
        return false;
    }
    AdapterDescriptions.resize(AdapterDescriptionCount);

    const ETiramisuGraphicsApi RequiredApi = GraphicsApi == nri::GraphicsAPI::D3D12
        ? ETiramisuGraphicsApi::D3D12
        : ETiramisuGraphicsApi::Vulkan;
    xr_vector<FTiramisuAdapterCandidate> Candidates;
    Candidates.reserve(AdapterDescriptions.size());
    for (const nri::AdapterDesc& Adapter : AdapterDescriptions)
    {
        FTiramisuAdapterCandidate Candidate;
        if ((static_cast<u8>(Adapter.supportedGraphicsAPIs) &
                static_cast<u8>(nri::GraphicsAPI::VK)) != 0)
        {
            Candidate.SupportedApis |= static_cast<u8>(
                ETiramisuGraphicsApi::Vulkan);
        }
        if ((static_cast<u8>(Adapter.supportedGraphicsAPIs) &
                static_cast<u8>(nri::GraphicsAPI::D3D12)) != 0)
        {
            Candidate.SupportedApis |= static_cast<u8>(
                ETiramisuGraphicsApi::D3D12);
        }

        switch (Adapter.architecture)
        {
        case nri::Architecture::DISCRETE:
            Candidate.Kind = ETiramisuAdapterKind::Discrete;
            break;
        case nri::Architecture::INTEGRATED:
            Candidate.Kind = ETiramisuAdapterKind::Integrated;
            break;
        case nri::Architecture::VIRTUAL:
            Candidate.Kind = ETiramisuAdapterKind::Virtual;
            break;
        case nri::Architecture::SOFTWARE:
            Candidate.Kind = ETiramisuAdapterKind::Software;
            break;
        default:
            Candidate.Kind = ETiramisuAdapterKind::Unknown;
            break;
        }
        Candidate.GraphicsQueueCount =
            Adapter.queueNum[static_cast<u32>(nri::QueueType::GRAPHICS)];
        Candidate.ComputeQueueCount =
            Adapter.queueNum[static_cast<u32>(nri::QueueType::COMPUTE)];
        Candidate.CopyQueueCount =
            Adapter.queueNum[static_cast<u32>(nri::QueueType::COPY)];
        Candidate.DedicatedVideoMemory = Adapter.videoMemorySize;
        Candidate.SharedSystemMemory = Adapter.sharedSystemMemorySize;
        Candidates.push_back(Candidate);
    }

    const xr_optional<size_t> Best =
        SelectBestTiramisuAdapter(Candidates, RequiredApi);
    if (!Best)
        return false;

    Result = AdapterDescriptions[*Best];
    return true;
}
