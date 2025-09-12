#include "stdafx.h"
#include "device.h"
#include "Stats.h"
#include "GameFont.h"
#include "XR_IOConsole.h"
#include "FPSCounter.h"
#include "../xrCore/appinfo.h"

ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter = nullptr;

using xr_clock = std::chrono::high_resolution_clock;

enum DebugTextColor : u64
{
	DTC_FPS_INFO = 0xFFFF8080,
};

XRay::Hardware::FPSCounter::FPSCounter()
{
	pCGameFont = g_FontManager->CloneFont("ui_font_console");
}

void XRay::Hardware::FPSCounter::OnRender()
{
	auto GetMonitorHZLambda = []() -> u32
	{
		int DisplayID = SDL_GetDisplayForWindow(g_AppInfo.Window);
		if (DisplayID < 0)
		{
			return 0u;
		}

		const SDL_DisplayMode* DisplayModePtr = SDL_GetDesktopDisplayMode(DisplayID);
		if (DisplayModePtr == nullptr)
		{
			return 0u;
		}

		return static_cast<u32>(DisplayModePtr->refresh_rate);
	};

	pCGameFont->SetHeight(0.013f);

	static auto lastFrameTime = xr_clock::now();
	auto currentTime = xr_clock::now();
	currentTime = xr_clock::now();
	std::chrono::duration<double> frameDuration = currentTime - lastFrameTime;
	lastFrameTime = currentTime;

	float fps = 0.f;
	static float prevFps = 0.f;

	if ((Device.dwFrame % GetMonitorHZLambda()) == 0)
		fps = 1.f / (float)frameDuration.count();

	fps = (fps == 0.0f) ? prevFps : fps;

	pCGameFont->SetAligment(CGameFont::alLeft);
	pCGameFont->SetColor(DebugTextColor::DTC_FPS_INFO);
	pCGameFont->Out(psCurrentVidMode[0] - pCGameFont->GetHeight() * 4.5f, 35, "FPS: %i", static_cast<int>(fps));
	pCGameFont->OnRender();

	prevFps = fps;
}