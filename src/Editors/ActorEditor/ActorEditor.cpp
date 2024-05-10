// ActorEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"

#include "../xrEProps/UIFileLoad.h"
CUFileOpen* FileOpen = nullptr;

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow)
{
    if (!IsDebuggerPresent())
        Debug._initialize(false);

    const char* FSName = "fs.ltx";
    Core._initialize("Actor", ELogCallback, 1, FSName);

    Tools = xr_new<CActorTools>();
    ATools = (CActorTools*)Tools;
    UI = xr_new<CActorMain>();
    UI->RegisterCommands();

    UIMainForm* MainForm = xr_new<UIMainForm>();
    ::MainForm = MainForm;

    FileOpen = new CUFileOpen;
    UI->Push(MainForm, false);
    UI->Push(FileOpen, false);

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