#include "stdafx.h"

#ifdef IXR_WINDOWS
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD ul_reason_for_call, LPVOID lpvReserved)
{
	switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		{
			g_RenderRHI = new CRender_RHI;
		} break;

		case DLL_PROCESS_DETACH:
		{
			xr_delete(g_RenderRHI);
		} break;
	}
	return TRUE;
}
#else
#	error Not supported yet
#endif