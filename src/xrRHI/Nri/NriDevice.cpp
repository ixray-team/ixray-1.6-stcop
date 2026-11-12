#include "../RHI.h"
#include "NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIDeviceCreation.h>

static nri::CoreInterface NriCore;

static void NRI_CALL NriMessage(nri::Message Type, const char*, u32, const char* Text, void*)
{
	static const char Prefix[] = { '*', '~', '!' };
	Msg("%c NRI: %s", Prefix[u32(Type)], Text);

	if (Type == nri::Message::ERROR)
	{
		xrLogger::FlushLog();
	}
}

NriDevice::NriDevice(ERHI_API_LAYER Api)
{
	nri::DeviceCreationDesc Desc = {};
	Desc.graphicsAPI = Api == ERHI_API_LAYER::D3D12 ? nri::GraphicsAPI::D3D12 : nri::GraphicsAPI::D3D11;
	Desc.callbackInterface.MessageCallback = NriMessage;
	Desc.enableGraphicsAPIValidation = Core.ParamsData.test(ECoreParams::dxdebug);
	Desc.enableNRIValidation = Desc.enableGraphicsAPIValidation;
	Desc.deviceLostInfoLevel = Desc.enableGraphicsAPIValidation ? nri::DeviceLostInfoLevel::VERBOSE : nri::DeviceLostInfoLevel::NONE;
	// The borrowing D3D12 backend still issues legacy barriers on this device
	Desc.disableD3D12EnhancedBarriers = true;

	nri::Result Result = nri::nriCreateDevice(Desc, Device);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "Unable to create a graphics device. Please update the video driver.");
	Result = nri::nriGetInterface(*Device, NRI_INTERFACE(nri::CoreInterface), &NriCore);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The NRI core interface is unavailable.");
	Result = NriCore.GetQueue(*Device, nri::QueueType::GRAPHICS, 0, Queue);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The graphics device has no graphics queue.");

	Msg("* NRI %u adapter: %s", u32(NRI_VERSION), NriCore.GetDeviceDesc(*Device).adapterDesc.name);
}

NriDevice::~NriDevice()
{
	nri::nriDestroyDevice(Device);
}

void* NriDevice::NativeDevice() const
{
	return NriCore.GetDeviceNativeObject(Device);
}

void* NriDevice::NativeQueue() const
{
	return NriCore.GetQueueNativeObject(Queue);
}
