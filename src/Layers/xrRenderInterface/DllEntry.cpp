#include "stdafx.h"
#include "linker.h"
#include "DeviceRHI.h"

#include "Dx11/Dx11Device.h"

extern CRenderRHI_DX11 g_RenderRHI_DX11Implementation;
extern "C"
{

RHI_API IRender_RHI* GetRenderRHIAPI(ERHI_API API)
{
	switch (API)
	{
	case ERHI_API::DX11:
		return (IRender_RHI*)&g_RenderRHI_DX11Implementation;
	}

	FATAL("Unknowed APILevel!");
	return nullptr;
}

}

#ifdef IXR_WINDOWS
BOOL APIENTRY DllMain(HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		// To ForserX: !!! g_CreateRHIFunc should be in xrAbstractions
		// = GetRenderRHIAPI;
		break;
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	return TRUE;
}

#else
#	error Not supported yet
#endif