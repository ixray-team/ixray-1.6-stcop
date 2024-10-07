#include "stdafx.h"

#include "../Layers/xrRenderInterface/DeviceRHI.h"

#include "device.h"
#include "Stats.h"
#include "GameFont.h"
#include "XR_IOConsole.h"
#include "FPSCounter.h"
#include "GameFont.h"

FPS::FPSCounter* pFPSCounter = nullptr;

using xr_clock = std::chrono::high_resolution_clock;

enum DebugTextColor : u64
{
    DTC_FPS_INFO = 0xFFFF8080,
};

FPS::FPSCounter::FPSCounter()
{
    pCGameFont = g_FontManager->CloneFont("ui_font_console");
}

void FPS::FPSCounter::OnRender() {
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
    pCGameFont->SetAligment(CGameFont::alLeft);
    pCGameFont->SetColor(DebugTextColor::DTC_FPS_INFO);
    pCGameFont->Out(psCurrentVidMode[0] - pCGameFont->GetHeight() * 4.5f, 35, "FPS: %i", static_cast<int>(fps));
    pCGameFont->OnRender();

    prevFps = fps;
}