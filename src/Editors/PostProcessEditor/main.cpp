#include "stdafx.h"
#include "UIEditorWindow.h"

void InitStyle()
{
	ImGuiStyle& Style = ImGui::GetStyle();

	ImVec4* colors = Style.Colors;
	colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	colors[ImGuiCol_TextDisabled] = ImVec4(0.26f, 0.26f, 0.26f, 1.00f);
	colors[ImGuiCol_WindowBg] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PopupBg] = ImVec4(0.08f, 0.08f, 0.08f, 1.00f);
	colors[ImGuiCol_Border] = ImVec4(0.16f, 0.16f, 0.18f, 0.50f);
	colors[ImGuiCol_BorderShadow] = ImVec4(0.05f, 0.05f, 0.05f, 0.00f);
	colors[ImGuiCol_FrameBg] = ImVec4(0.13f, 0.13f, 0.13f, 0.54f);
	colors[ImGuiCol_FrameBgHovered] = ImVec4(0.27f, 0.27f, 0.27f, 0.54f);
	colors[ImGuiCol_FrameBgActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_TitleBg] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgActive] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_MenuBarBg] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_ScrollbarBg] = ImVec4(0.02f, 0.02f, 0.02f, 0.53f);
	colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.31f, 0.31f, 0.31f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.41f, 0.41f, 0.41f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_CheckMark] = ImVec4(0.83f, 0.83f, 0.83f, 0.54f);
	colors[ImGuiCol_SliderGrab] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_SliderGrabActive] = ImVec4(0.95f, 0.95f, 0.95f, 1.00f);
	colors[ImGuiCol_Button] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_ButtonHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_ButtonActive] = ImVec4(0.75f, 0.75f, 0.75f, 0.54f);
	colors[ImGuiCol_Header] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_HeaderHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_HeaderActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_Separator] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_SeparatorHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_SeparatorActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_ResizeGrip] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_ResizeGripHovered] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_ResizeGripActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_Tab] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabHovered] = ImVec4(0.30f, 0.30f, 0.30f, 1.00f);
	colors[ImGuiCol_TabActive] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_TabUnfocused] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabUnfocusedActive] = ImVec4(0.42f, 0.42f, 0.42f, 0.54f);
	colors[ImGuiCol_DockingPreview] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DockingEmptyBg] = ImVec4(0.05f, 0.05f, 0.05f, 1.00f);
	colors[ImGuiCol_PlotLines] = ImVec4(0.61f, 0.61f, 0.61f, 1.00f);
	colors[ImGuiCol_PlotLinesHovered] = ImVec4(1.00f, 0.43f, 0.35f, 1.00f);
	colors[ImGuiCol_PlotHistogram] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.60f, 0.00f, 1.00f);
	colors[ImGuiCol_TableHeaderBg] = ImVec4(0.19f, 0.19f, 0.20f, 1.00f);
	colors[ImGuiCol_TableBorderStrong] = ImVec4(0.31f, 0.31f, 0.35f, 1.00f);
	colors[ImGuiCol_TableBorderLight] = ImVec4(0.23f, 0.23f, 0.25f, 1.00f);
	colors[ImGuiCol_TableRowBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_TableRowBgAlt] = ImVec4(1.00f, 1.00f, 1.00f, 0.06f);
	colors[ImGuiCol_TextSelectedBg] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DragDropTarget] = ImVec4(1.00f, 1.00f, 0.00f, 0.90f);
	colors[ImGuiCol_NavHighlight] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
	colors[ImGuiCol_NavWindowingHighlight] = ImVec4(1.00f, 1.00f, 1.00f, 0.70f);
	colors[ImGuiCol_NavWindowingDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.20f);
	colors[ImGuiCol_ModalWindowDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.35f);
}
int main()
{
    Core._initialize("PPE", 0, TRUE, "fs.ltx");

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

    // Setup Dear ImGui style
   // ImGui::StyleColorsDark();
	InitStyle();

    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMEPAD);
    Uint32 window_flags =  SDL_WINDOW_RESIZABLE;
    g_AppInfo.Window = SDL_CreateWindow("IX-Ray PostProcess Editor", 820, 620, window_flags);
    DevicePtr = new CRenderDevice;

	DevicePtr->InitRenderDevice(APILevel::DX9);
	DevicePtr->TargetWidth = 820;
	DevicePtr->TargetHeight = 620;
	DevicePtr->Reset();

    // Main editor object 
    CMainPPE MyEditorObject;

    bool CloseCheck = false;
    while (!CloseCheck)
    {
        // Poll and handle events (inputs, window resize, etc.)
        // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
        // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
        // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
        SDL_Event Event;
        while (SDL_PollEvent(&Event))
        {
			DevicePtr->ProcessEvent(Event);

            if (Event.window.windowID != SDL_GetWindowID(g_AppInfo.Window))
                continue;

            if (Event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED || Event.type == SDL_EVENT_WINDOW_DESTROYED || Event.type == SDL_EVENT_QUIT)
            {
                CloseCheck = true;
                break;
            }
            else if (Event.type == SDL_EVENT_WINDOW_RESIZED)
            {
				DevicePtr->TargetWidth = Event.window.data1;
				DevicePtr->TargetHeight = Event.window.data2;
				DevicePtr->Reset();
            }
        }

        // Start the Dear ImGui frame
		DevicePtr->BeginRender();
        ImGui::NewFrame();

        MyEditorObject.DrawUI();


        // Rendering
        ImGui::Render();
        ImGui::EndFrame();

		DevicePtr->Clear();

		DevicePtr->EndRender();
    }

	DevicePtr->DestroyRenderDevice();
    xr_delete(DevicePtr);

	return 1;
}