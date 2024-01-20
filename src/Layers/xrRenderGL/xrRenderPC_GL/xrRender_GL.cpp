// xrRender_GL.cpp : Defines the entry point for the DLL application.
//
#include "stdafx.h"
#include "xrRenderOpenGL/dxRenderFactory.h"
#include "xrRenderOpenGL/dxUIRender.h"
#include "xrRenderOpenGL/dxDebugRender.h"

BOOL APIENTRY DllMain(HANDLE hModule,
	DWORD  ul_reason_for_call,
	LPVOID lpReserved
)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		xrAPI.Render = &RImplementation;
		xrAPI.RenderFactory = &RenderFactoryImpl;
		xrAPI.DU = &DUImpl;
		xrAPI.UIRender = &UIRenderImpl;
		xrAPI.DRender = &DebugRenderImpl;
		xrRender_initconsole();
		break;
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	return TRUE;
}