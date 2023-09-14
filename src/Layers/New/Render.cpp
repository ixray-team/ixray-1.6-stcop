#include "pch.h"

void InitializeRenderConsole();

BOOL APIENTRY DllMain(HANDLE hModule,
	DWORD  ul_reason_for_call,
	LPVOID lpReserved
)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		::Render = &RenderEngineInterface;
		::RenderFactory = &RenderFactoryInterface;
		//::DU = &DUImpl;
		UIRender = &UIRenderInterface;

#ifdef DEBUG
		//DRender = &DebugRenderImpl;
#endif	//	DEBUG

		InitializeRenderConsole();
		break;
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	return TRUE;
}