#include "TRenderDevice.h"
#include "Extensions/NRIDeviceCreation.h"
#include "Extensions/NRIStreamer.h"

TRenderDevice GRenderDevice;

TRenderDevice::TRenderDevice() : CoreInterface({}), SwapChainInterface({}), HelperInterface({})
{
}

TRenderDevice::~TRenderDevice()
= default;

void TRenderDevice::Initialize()
{
	GraphicsApi = nri::GraphicsAPI::VK;
	if (strstr(Core.Params,"-dx12"))
	{
		GraphicsApi = nri::GraphicsAPI::D3D12;
	}
	
	nri::AdapterDesc CurrentAdapterDescription = GetBestAdapterDescription();
    // Settings
    constexpr bool D3D11_ENABLE_COMMAND_BUFFER_EMULATION = false;
    constexpr bool D3D12_DISABLE_ENHANCED_BARRIERS = false;
    // Device
    nri::DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.graphicsAPI = GraphicsApi;
	if (strstr(Core.Params,"-d3ddebug") || 
		strstr(Core.Params,"-vkdebug") ||
		strstr(Core.Params,"-rdebug")||
		strstr(Core.Params,"-rdbg"))
	{
		deviceCreationDesc.enableGraphicsAPIValidation = true;
		deviceCreationDesc.enableNRIValidation = true;
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

void TRenderDevice::Destroy()
{
	
	StreamerInterface.DestroyStreamer(Streamer);
	GraphicsQueue = nullptr;
	CoreInterface = {};
	SwapChainInterface = {};
	HelperInterface = {};
	StreamerInterface = {};
	nri::nriDestroyDevice(Device);
}

nri::AdapterDesc TRenderDevice::GetBestAdapterDescription()
{
	nri::AdapterDesc AdapterDescription[2] = {};
	uint32_t AdapterDescriptionsNum = 2;
	NRI_CHECK(nri::nriEnumerateAdapters(AdapterDescription, AdapterDescriptionsNum));
	return AdapterDescription[0];
}
