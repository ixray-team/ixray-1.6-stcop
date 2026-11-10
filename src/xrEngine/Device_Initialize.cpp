#include "stdafx.h"

void CRenderDevice::Initialize			()
{
	Log("Initializing Engine...");
	TimerGlobal.Start			();
	TimerMM.Start				();

    // Save window properties

	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Width, &Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &PosX, &PosY);
}

