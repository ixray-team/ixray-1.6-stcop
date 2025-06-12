#include "api.h"
#include <imgui.h>
#include <mysql/jdbc.h>

#include "../xrForms/imgui_impl_sdl3.h"
#include "../xrForms/imgui_impl_sdlrenderer3.h"

#include "../../Editors/xrEUI/spectrum.h"
#include "ImGuiSpinner.h"

enum UIState
{
	LoginForm = 0,
	Connecting,
	ConFail,
	WorkForm
};


SLoginInfo GLoginInfo;
UIState GUIState;
void RenderLoginUI(ImGuiViewport* viewport,bool connecting = false)
{	
		if (connecting)
		{
			float r = 32;
			ImGui::SetCursorPos({ (viewport->Size.x - r*2) / 2, (viewport->Size.y - r*2) / 2});
			const ImU32 col = ImGui::GetColorU32(ImGuiCol_TextDisabled);
			ImGui::Spinner("##spinner", r, 6, col);
			//ImGui::LoadingIndicatorCircle("##spin", r, { 150,150,150,255 }, { 50,50,50,255 }, 12, 2.f);
		}

		// Центрируем по вертикали (располагаем примерно по центру экрана)
		float center_y = viewport->Size.y * 0.3f;
		ImGui::SetCursorPosY(center_y);

		// Центрируем каждый элемент по горизонтали
		const float ItemWidth = 200.0f; // Ширина элементов ввода
		float WndWidth = viewport->Size.x;

		auto x = ImGui::GetContentRegionAvail().x;
		auto cur_x = ImGui::GetCursorPosX();

		ImGui::BeginDisabled(connecting);

		ImGui::SetCursorPosX((x - ItemWidth) / 2 + cur_x);
		if (ImGui::BeginChild("##loginForm", { ItemWidth,-1 }, 0, 0))
		{
			 x = ImGui::GetContentRegionAvail().x;
			 cur_x = ImGui::GetCursorPosX();
			
#define CenterText(text) ImGui::SetCursorPosX( (x-ImGui::CalcTextSize(text).x)/2 +cur_x );\
								ImGui::Text(text);

			// Login
			ImGui::PushItemWidth(-1);
			CenterText("Login");
			ImGui::InputText("##login", GLoginInfo.Login, 32);

			// Server
			CenterText("Host");
			ImGui::InputText("##server", GLoginInfo.Host, 128);

			// Password
			CenterText("Password");
			ImGui::InputText("##pass", GLoginInfo.Pass, 32, ImGuiInputTextFlags_Password);

			ImGui::Dummy({ 0,5 });
			ImGui::Separator();
			ImGui::Dummy({ 0,5 });

			if (ImGui::Button("Login", { -1,35 }))
			{
				GUIState = UIState::Connecting;

				std::thread([&]() {
					try {
						sql::Driver* driver = get_driver_instance();

						std::unique_ptr<sql::Connection> conn(driver->connect(
							GLoginInfo.Host,
							GLoginInfo.Login,
							GLoginInfo.Pass
						));

						// Проверка состояния подключения
						GUIState = (conn && conn->isValid() ? UIState::WorkForm : UIState::ConFail);

					}
					catch (const sql::SQLException& e) {
						GUIState = UIState::ConFail;
					}

					}).detach();
			}

			ImGui::PopItemWidth();

			ImGui::EndChild();
		}
		ImGui::EndDisabled();
}

void RenderErrorPage(ImGuiViewport* viewport)
{
	ImGui::OpenPopup("Connection Error");
	if (ImGui::BeginPopupModal("Connection Error", NULL, ImGuiWindowFlags_AlwaysAutoResize)) {
		ImGui::TextColored(ImVec4(1, 0.3f, 0.3f, 1), "Failed to connect to server!");

		ImGui::Spacing();
		ImGui::Separator();
		ImGui::Spacing();

		ImGuiStyle& style = ImGui::GetStyle();
		float button_width = 120.0f;
		float avail = ImGui::GetContentRegionAvail().x;
		ImGui::SetCursorPosX((avail - button_width) * 0.5f);

		if (ImGui::Button("OK", ImVec2(button_width, 0))) {
			GUIState = UIState::LoginForm();
		}
		ImGui::EndPopup();
	}
}

void RenderWorkForm(ImGuiViewport* viewport)
{
	//
	static bool syncQuest;
	static bool syncItems;
	//

	// Верхняя часть — чекбоксы и кнопка
	ImGui::BeginChild("Top", ImVec2(0, 250), true);
	ImGui::Checkbox("Sync Quest", &syncQuest);
	ImGui::Checkbox("Sync Items", &syncItems);

	
	ImGui::EndChild();
	// Кнопка справа
	ImGui::SetCursorPosX(viewport->Size.x - 90);
	if (ImGui::Button("Sync", ImVec2(70, 30))) {
		//some chto-to
	}
	// Нижняя часть — лог
	ImGui::Separator();
	ImGui::Text("Лог:");
	ImGui::Separator();
	ImGui::BeginChild("Log", ImVec2(0, 0), true);
	for (int i = 0; i < 25; ++i) {
		ImGui::TextUnformatted("palceholder log line");
	}

	ImGui::EndChild();
}

void UIHub()
{
	// Получаем основной viewport
	ImGuiViewport* viewport = ImGui::GetMainViewport();

	// Устанавливаем позицию и размер окна равными viewport'у
	ImGui::SetNextWindowPos(viewport->Pos);
	ImGui::SetNextWindowSize(viewport->Size);

	// Убираем декорации окна (заголовок, рамку и т.д.)
	ImGuiWindowFlags window_flags =
		ImGuiWindowFlags_NoTitleBar |
		ImGuiWindowFlags_NoCollapse |
		ImGuiWindowFlags_NoResize |
		ImGuiWindowFlags_NoMove |
		ImGuiWindowFlags_NoBringToFrontOnFocus |
		ImGuiWindowFlags_NoNavFocus;

	if (ImGui::Begin("##MainWnd", nullptr, window_flags))
	{
		switch (GUIState)
		{
		case UIState::LoginForm:
			RenderLoginUI(viewport);
			break;
		case UIState::Connecting:
			RenderLoginUI(viewport,true);
			break;
		case UIState::ConFail:
			RenderErrorPage(viewport);
			RenderLoginUI(viewport);
			break;
		case UIState::WorkForm:
			RenderWorkForm(viewport);
			//ImGui::Text("OK");
			break;
		default:
			break;
		}
		ImGui::End();
	}
}

void SDL_Application()
{
	if (!SDL_Init(SDL_INIT_EVENTS) != 0)
	{
		printf("Error: SDL_Init(): %s\n", SDL_GetError());
		return;
	}

	GUIState = UIState::LoginForm;

	SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Level Builder", 1000, 560, window_flags);
	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL);

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);

	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO(); (void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

	// Setup Dear ImGui style

	ImVec4* colors = ImGui::GetStyle().Colors;

	colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_BorderShadow] = ImVec4(0.00f, 0.00f, 0.00f, 0.24f);
	colors[ImGuiCol_FrameBgHovered] = ImVec4(0.19f, 0.19f, 0.19f, 0.54f);
	colors[ImGuiCol_FrameBgActive] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_ScrollbarBg] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.34f, 0.34f, 0.34f, 0.54f);
	colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.40f, 0.40f, 0.40f, 0.54f);
	colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_SliderGrab] = ImVec4(0.34f, 0.34f, 0.34f, 0.54f);
	colors[ImGuiCol_SliderGrabActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_HeaderActive] = ImVec4(0.20f, 0.22f, 0.23f, 0.33f);
	colors[ImGuiCol_Separator] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_SeparatorHovered] = ImVec4(0.44f, 0.44f, 0.44f, 0.29f);
	colors[ImGuiCol_SeparatorActive] = ImVec4(0.40f, 0.44f, 0.47f, 1.00f);
	colors[ImGuiCol_ResizeGrip] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_ResizeGripHovered] = ImVec4(0.44f, 0.44f, 0.44f, 0.29f);
	colors[ImGuiCol_ResizeGripActive] = ImVec4(0.40f, 0.44f, 0.47f, 1.00f);
	colors[ImGuiCol_Tab] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_DockingPreview] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_DockingEmptyBg] = ImVec4(0.30f, 0.30f, 0.30f, 1.00f);
	colors[ImGuiCol_DragDropTarget] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_NavHighlight] = ImVec4(0.00f, 0.00f, 0.30f, 1.00f);
	colors[ImGuiCol_NavWindowingHighlight] = ImVec4(0.00f, 0.00f, 0.30f, 0.70f);
	colors[ImGuiCol_NavWindowingDimBg] = ImVec4(0.00f, 0.00f, 0.30f, 0.20f);
	colors[ImGuiCol_ModalWindowDimBg] = ImVec4(0.00f, 0.00f, 0.30f, 0.35f);

	ImGuiStyle& style = ImGui::GetStyle();
	style.WindowPadding = ImVec2(8.00f, 8.00f);
	style.FramePadding = ImVec2(5.00f, 2.00f);
	style.CellPadding = ImVec2(6.00f, 6.00f);
	style.ItemSpacing = ImVec2(6.00f, 6.00f);
	style.ItemInnerSpacing = ImVec2(6.00f, 6.00f);
	style.TouchExtraPadding = ImVec2(0.00f, 0.00f);
	style.IndentSpacing = 25;
	style.ScrollbarSize = 15;
	style.GrabMinSize = 10;
	style.WindowBorderSize = 1;
	style.ChildBorderSize = 1;
	style.PopupBorderSize = 1;
	style.FrameBorderSize = 1;
	style.TabBorderSize = 1;
	style.WindowRounding = 7;
	style.ChildRounding = 4;
	style.FrameRounding = 3;
	style.PopupRounding = 4;
	style.ScrollbarRounding = 9;
	style.GrabRounding = 3;
	style.LogSliderDeadzone = 4;
	style.TabRounding = 4;

	colors[ImGuiCol_WindowBg] = ImVec4(0.10f, 0.10f, 0.10f, 1.00f);
	colors[ImGuiCol_MenuBarBg] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	colors[ImGuiCol_TextDisabled] = ImVec4(0.50f, 0.50f, 0.50f, 1.00f);
	colors[ImGuiCol_TextSelectedBg] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_TableHeaderBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TableBorderStrong] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TableBorderLight] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_TableRowBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_TableRowBgAlt] = ImVec4(1.00f, 1.00f, 1.00f, 0.06f);
	colors[ImGuiCol_FrameBg] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_CheckMark] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_Border] = ImVec4(0.19f, 0.19f, 0.19f, 0.29f);
	colors[ImGuiCol_TitleBg] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_TabHovered] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_TabActive] = ImVec4(0.20f, 0.20f, 0.20f, 0.36f);
	colors[ImGuiCol_TabUnfocused] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TabUnfocusedActive] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_TitleBgActive] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_Button] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_ButtonHovered] = ImVec4(0.19f, 0.19f, 0.19f, 0.54f);
	colors[ImGuiCol_ButtonActive] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_Header] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_HeaderHovered] = ImVec4(0.00f, 0.00f, 0.00f, 0.36f);
	colors[ImGuiCol_PopupBg] = ImVec4(0.19f, 0.19f, 0.19f, 0.92f);
	colors[ImGuiCol_PlotLines] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotLinesHovered] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogram] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);

	//setup cool font!
	ImGui::GetIO().Fonts->Clear();
	ImGui::Spectrum::LoadFont(18.f);

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
	ImFont* defaultFont = io.Fonts->AddFontDefault();

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	bool done = false;
	while (!done)
	{
		// Poll and handle events (inputs, window resize, etc.)
		// You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
		// - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
		// - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
		// Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			ImGui_ImplSDL3_ProcessEvent(&event);
			if (event.type == SDL_EVENT_QUIT)
				done = true;
			if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED && event.window.windowID == SDL_GetWindowID(g_AppInfo.Window))
				done = true;
		}

		// Start the Dear ImGui frame
		ImGui_ImplSDLRenderer3_NewFrame();
		ImGui_ImplSDL3_NewFrame();
		ImGui::NewFrame();

		{
			UIHub();
		}

		// Rendering
		ImGui::Render();

		SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
		SDL_RenderClear(renderer);
		ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());
		SDL_RenderPresent(renderer);
	}

	// Cleanup
	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(g_AppInfo.Window);
	SDL_Quit();

}

int main()
{
	Debug._initialize(false);
	Core._initialize("IXRay", nullptr, TRUE, "fsgame.ltx");

	SDL_Application();

	return 0;
}
