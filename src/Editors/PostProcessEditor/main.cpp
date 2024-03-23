#include "../EditorRender/stdafx.h"
#include <imgui.h>

int main()
{
    Core._initialize("PPE", 0, TRUE, "fsgame.ltx");

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();

    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMEPAD);
    Uint32 window_flags =  SDL_WINDOW_RESIZABLE;
    g_AppInfo.Window = SDL_CreateWindow("Dear ImGui SDL3+OpenGL3 example", 1280, 720, window_flags);
    CEditorDevice* MyDevice = new CEditorDevice;
    DevicePtr = MyDevice;

    MyDevice->InitRenderDevice(APILevel::DX9);
    MyDevice->Reset(1280, 720);

    while (true)
    {
        // Poll and handle events (inputs, window resize, etc.)
        // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
        // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
        // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
        SDL_Event Event;
        while (SDL_PollEvent(&Event))
        {
            MyDevice->ProcessEvent(Event);
        }

        // Start the Dear ImGui frame
        MyDevice->BeginRender();
        ImGui::NewFrame();

        bool show_demo_window = true;
        ImGui::ShowDemoWindow(&show_demo_window);


        // Rendering
        ImGui::Render();
        ImGui::EndFrame();

        MyDevice->Clear();

        MyDevice->EndRender();
    }
	return 1;
}