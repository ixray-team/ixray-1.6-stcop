#include "stdafx.h"
#include "dxRenderFactory.h"
#include "dxUIRender.h"
#include "dxDebugRender.h"

static void
call_shit()
{
	::Render = &RImplementation;
	::RenderFactory = &RenderFactoryImpl;
	::DU = &DUImpl;
	UIRender = &UIRenderImpl;

#ifdef DEBUG_DRAW
	DRender = &DebugRenderImpl;
#endif // DEBUG

	xrRender_initconsole();
}

#ifdef IXR_WINDOWS
BOOL APIENTRY DllMain(HANDLE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH: call_shit(); break;
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	return true;
}
#else

__attribute__((constructor)) 
static void on_library_load(void)
{
	call_shit();
}
#endif