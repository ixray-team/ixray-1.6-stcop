#include "XRayRenderDevice.h"
#include "Extensions/NRIDeviceCreation.h"
XRayRenderDevice GRenderDevice;

XRayRenderDevice::XRayRenderDevice() : CoreInterface({}), SwapChainInterface({}), HelperInterface({})
{
}

XRayRenderDevice::~XRayRenderDevice()
= default;

void XRayRenderDevice::Initialize()
{
	GraphicsApi = nri::GraphicsAPI::D3D12;
	nri::AdapterDesc CurrentAdapterDescription = GetBestAdapterDescription();
    // Settings
    constexpr bool D3D11_ENABLE_COMMAND_BUFFER_EMULATION = false;
    constexpr bool D3D12_DISABLE_ENHANCED_BARRIERS = false;
    // Device
    nri::DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.graphicsAPI = GraphicsApi;
    deviceCreationDesc.enableGraphicsAPIValidation = true;
    deviceCreationDesc.enableNRIValidation = true;
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
    // Command queue
    NRI_CHECK(CoreInterface.GetQueue(*Device, nri::QueueType::GRAPHICS, 0, GraphicsQueue));
	
	DeviceDescription = CoreInterface.GetDeviceDesc(*Device);
	
}

void XRayRenderDevice::Destroy()
{
	GraphicsQueue = nullptr;
	CoreInterface = {};
	SwapChainInterface = {};
	HelperInterface = {};
	nri::nriDestroyDevice(Device);
}

nri::AdapterDesc XRayRenderDevice::GetBestAdapterDescription()
{
	nri::AdapterDesc AdapterDescription[2] = {};
	uint32_t AdapterDescriptionsNum = 2;
	NRI_CHECK(nri::nriEnumerateAdapters(AdapterDescription, AdapterDescriptionsNum));
	return AdapterDescription[0];
}
