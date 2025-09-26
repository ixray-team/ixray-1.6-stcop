#include "RHI.h"

#include "D3D11/Device.h"
#include "D3D9/Device.h"

RHI_API u32 psCurrentVidMode[2] = { 1024,768 };
RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Ivector2 HalfTarget = { 0, 0 };
RHI_API void* g_pAnnotation = nullptr;
RHI_API CRHI* GRHI = nullptr;

CRHI::~CRHI()
{
	xr_delete(DevicePtr);
}

IRHIDevice* CRHI::CreateDevice(ERHI_API_LAYER NewAPILevel)
{
	switch (NewAPILevel)
	{
		case ERHI_API_LAYER::D3D9:  DevicePtr = new InternalDevice9;  break;
		case ERHI_API_LAYER::D3D11: DevicePtr = new InternalDevice11; break;
	}

	APILevel = NewAPILevel;

	return DevicePtr;
}

void CRHI::ResizeBuffers(u32 Width, u32 Height)
{
	DevicePtr->ResizeBuffers(Width, Height);
}

void* CRHI::GetContext()
{
	if (APILevel == ERHI_API_LAYER::NOT_CREATED)
	{
		return nullptr;
	}
	else if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWRenderContext;
	}

	VERIFY(!"Unsupported");
	return nullptr;
}

void* CRHI::GetSwapchain()
{
	if (APILevel == ERHI_API_LAYER::NOT_CREATED)
	{
		return nullptr;
	}
	else  if (APILevel == ERHI_API_LAYER::D3D11)
	{
		return ((InternalDevice11*)DevicePtr)->HWSwapchain;
	}

	VERIFY(!"Unsupported");
	return nullptr;
}

void CRHI::ClearTarget(void* Target, ERTColor Transparent)
{
	DevicePtr->ClearTarget(Target, Transparent);
}