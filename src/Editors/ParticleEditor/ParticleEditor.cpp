// ParticleEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"
#include "../xrEProps/UIFileLoad.h"

CUFileOpen* FileOpen = nullptr;

void BeginRender()
{
#define D3DCOLOR_RGBA(r,g,b,a) D3DCOLOR_ARGB(a,r,g,b)
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    RDevice->SetRenderState(D3DRS_ZENABLE, FALSE);
    RDevice->SetRenderState(D3DRS_ALPHABLENDENABLE, FALSE);
    RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, FALSE);
    D3DCOLOR clear_col_dx = D3DCOLOR_RGBA((int)(clear_color.x * clear_color.w * 255.0f), (int)(clear_color.y * clear_color.w * 255.0f), (int)(clear_color.z * clear_color.w * 255.0f), (int)(clear_color.w * 255.0f));
    RDevice->Clear(0, nullptr, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, clear_col_dx, 1.0f, 0);

    RDevice->BeginScene();
}

void EndRender()
{
    RDevice->EndScene();
    RDevice->Present(nullptr, nullptr, nullptr, nullptr);
}

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow)
{
    if (!IsDebuggerPresent()) Debug._initialize(false);
    const char* FSName = "fs.ltx";

    CoInitialize(nullptr);

    Core._initialize("Patricle", ELogCallback, 1, FSName);

    psDeviceFlags.set(rsFullscreen, false);

    Tools = xr_new<CParticleTool>();
    PTools = (CParticleTool*)Tools;
    UI = xr_new<CParticleMain>();
    UI->RegisterCommands();
    
    FileOpen = new CUFileOpen;

    UIMainForm* MainForm = xr_new< UIMainForm>();
    ::MainForm = MainForm;
    UI->Push(MainForm, false);
    UI->Push(FileOpen, false);

    //MainForm->Frame();
    while (true)
    {
        SDL_Event Event;
        while (SDL_PollEvent(&Event))
        {
            switch (Event.type)
            {
                case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
                    return 0;

                case SDL_EVENT_WINDOW_RESIZED:
                   if (UI && REDevice)
                   {
                       UI->Resize(Event.window.data1, Event.window.data2, true);
                       EPrefs->Save();
                   }
                    break;
                case SDL_EVENT_WINDOW_SHOWN:
                case SDL_EVENT_WINDOW_MOUSE_ENTER:
                    Device.b_is_Active = true;
                    //if (UI) UI->OnAppActivate();

                    break;
                case SDL_EVENT_WINDOW_HIDDEN:
                case SDL_EVENT_WINDOW_MOUSE_LEAVE:
                    Device.b_is_Active = false;
                    //if (UI)UI->OnAppDeactivate();
                    break;
            }

            if (!UI->ProcessEvent(&Event))
                break;
        }
        MainForm->Frame();
    }

    xr_delete(MainForm);
    Core._destroy();
    return 0;
}
