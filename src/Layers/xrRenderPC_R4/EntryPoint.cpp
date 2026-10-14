#include "stdafx.h"
#include "dxRenderFactory.h"
#include "dxUIRender.h"
#include "dxDebugRender.h"
#include "../../xrEngine/XR_IOConsole.h"

static void RenderEntryLibraryPoint()
{
	::Render = &RImplementation;
	::RenderFactory = &RenderFactoryImpl;
	::DU = &DUImpl;
	UIRender = &UIRenderImpl;

#ifdef DEBUG_DRAW
	DRender = &DebugRenderImpl;
#endif // DEBUG

	// The editors link this library directly, so it is loaded before the console exists;
	// they do not use the renderer console commands.
	if (Console != nullptr)
	{
		xrRender_initconsole();
	}
}

#ifdef IXR_WINDOWS
BOOL APIENTRY DllMain(HANDLE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH: RenderEntryLibraryPoint(); break;
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
	RenderEntryLibraryPoint();
}
#endif