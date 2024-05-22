#include "stdafx.h"
#include "device.h"
#include "Stats.h"
#include "GameFont.h"
#include "XR_IOConsole.h"
#include "FPSCounter.h"
#include "GameFont.h"
#include "../xrCore/git_version.h"

FPS::FPSCounter* pFPSCounter = nullptr;

using xr_clock = std::chrono::high_resolution_clock;

enum DebugTextColor : u64
{
    DTC_GAME_VERSION = 0xFFF5F5DC,
    DTC_FPS_INFO = 0xFFFF8080,
};

FPS::FPSCounter::FPSCounter()
{
    pCGameFont = g_FontManager->GetFont("ui_font_console");
}

void FPS::FPSCounter::ShowEngineVersion(bool IsMainMenu)
{
    auto l_MonitorHZ = []() -> u32
    {
        u32 refreshRate = 0;

        DEVMODE devMode = {};
        devMode.dmSize = sizeof(devMode);

        if (EnumDisplaySettings(nullptr, ENUM_CURRENT_SETTINGS, &devMode) != 0) {
            return refreshRate = devMode.dmDisplayFrequency;
        }

        return 0u;
    };

    if (IsMainMenu && !Console->bVisible)
    {
        pCGameFont->SetAligment(CGameFont::alLeft);
        pCGameFont->SetHeight(0.022f);
        pCGameFont->SetColor(DebugTextColor::DTC_GAME_VERSION);

        pCGameFont->Out(psCurrentVidMode[0] - 475, (psCurrentVidMode[1] - pCGameFont->GetHeight()) - 10, "%s Branch[" _BRANCH "] Hash[" _HASH "]", ActualVersion.c_str());

        pCGameFont->OnRender();
    }
    else if (!IsMainMenu)
    {
        pCGameFont->SetHeight(0.013f);

        static auto lastFrameTime = xr_clock::now();
        auto currentTime = xr_clock::now();
        currentTime = xr_clock::now();
        std::chrono::duration<double> frameDuration = currentTime - lastFrameTime;
        lastFrameTime = currentTime;

        float fps = 0.f;
        static float prevFps = 0.f;

        if ((Device.dwFrame % l_MonitorHZ()) == 0)
            fps = 1.f / (float)frameDuration.count();

        fps = (fps == 0.0f) ? prevFps : fps;

        //clamp(fps, 0.f, static_cast<float>(l_MonitorHZ()));

        pCGameFont->SetColor(DebugTextColor::DTC_FPS_INFO);
        pCGameFont->Out(psCurrentVidMode[0] - pCGameFont->GetHeight() * 5, 35, "FPS: %i", static_cast<int>(fps));
        pCGameFont->OnRender();

        prevFps = fps;
    }
}