#pragma once
#include "Extensions/NRIDeviceCreation.h"

class XRayRenderDevice
{
public:
            								XRayRenderDevice    		();
            								~XRayRenderDevice   		();
    void    								Initialize          		();
    void    								Destroy             		();
    
    nri::Device*							Device = nullptr;
    nri::Queue*								GraphicsQueue = nullptr;
	
    nri::CoreInterface						CoreInterface;
    nri::SwapChainInterface 				SwapChainInterface;
    nri::HelperInterface					HelperInterface;
	static constexpr  nri::VKBindingOffsets	VK_BINDING_OFFSETS = {0, 128, 32, 64}; // see CMake
	nri::GraphicsAPI						GraphicsApi = nri::GraphicsAPI::VK;
	nri::DeviceDesc							DeviceDescription = {};
private:
	nri::AdapterDesc						GetBestAdapterDescription	();
	nri::AllocationCallbacks				AllocationCallbacks = {};
};

extern XRayRenderDevice GRenderDevice;