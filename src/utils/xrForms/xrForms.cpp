#include "../../xrCore/xrCore.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <luabind/luabind.hpp>
#include <imgui.h>

#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "CompilerIcons.h"

static LPVOID __cdecl luabind_allocator(
	luabind::memory_allocation_function_parameter const,
	void const* const pointer,
	size_t const size
)
{
	if (!size)
	{
		LPVOID	non_const_pointer = const_cast<LPVOID>(pointer);
		xr_free(non_const_pointer);
		return	(0);
	}

	if (!pointer)
	{
		return	(Memory.mem_alloc(size));
	}

	LPVOID non_const_pointer = const_cast<LPVOID>(pointer);
	return (Memory.mem_realloc(non_const_pointer, size));
}

void setup_luabind_allocator()
{
	luabind::allocator = &luabind_allocator;
	luabind::allocator_parameter = 0;
}

#pragma warning(disable:4995)
#include <timeapi.h>
#include <commctrl.h>
#pragma warning(default:4995)

extern HWND logWindow;

void StartupAI();
void StartupLC();
void StartupDO();

void InitialFactory();
void DestroyFactory();

void Help(const char* h_str) {
	MessageBoxA(0, h_str, "Command line options", MB_OK | MB_ICONINFORMATION);
}

CompilersMode gCompilerMode;

extern bool ShowMainUI;
void Startup(LPSTR lpCmdLine) 
{
	GetIterationData().push_back({ "xrLC" });
	GetIterationData().push_back({ "xrAI" });
	GetIterationData().push_back({ "xrDO" });

	u32 dwStartupTime = timeGetTime();

	SetActiveIteration(&(GetIterationData()[0]));
	u32 dwTimeLC = 0;
	if (gCompilerMode.LC) {
		GetActiveIteration()->status = InProgress;
		dwTimeLC = timeGetTime();
		Phase("xrLC Startup");
		StartupLC();

		dwTimeLC = (timeGetTime() - dwTimeLC) / 1000;

		GetActiveIteration()->status = Complited;
		GetActiveIteration()->elapsed_time = dwTimeLC;
	}
	else
	{
		GetActiveIteration()->status = Skip;
	}

	SetActiveIteration(&(GetIterationData()[1]));
	u32 dwTimeAI = 0;
	if (gCompilerMode.AI)
	{
		GetActiveIteration()->status = InProgress;

		dwTimeAI = timeGetTime();
		Phase("xrAI Startup");

		setup_luabind_allocator();
		InitialFactory();
		StartupAI();
		DestroyFactory();
		dwTimeAI = (timeGetTime() - dwTimeAI) / 1000;

		GetActiveIteration()->status = Complited;
		GetActiveIteration()->elapsed_time = dwTimeLC;
	}
	else
	{
		GetActiveIteration()->status = Skip;
	}

	SetActiveIteration(&(GetIterationData()[2]));
	u32 dwTimeDO = 0;
	if (gCompilerMode.DO) {
		GetActiveIteration()->status = InProgress;
		dwTimeDO = timeGetTime();
		Phase("xrDO Startup");
		StartupDO();
		dwTimeDO = (timeGetTime() - dwTimeDO) / 1000;

		GetActiveIteration()->status = Complited;
		GetActiveIteration()->elapsed_time = dwTimeLC;
	}
	else
	{
		GetActiveIteration()->status = Skip;
	}

	// Show statistic
	string256 stats;
	extern xr_string make_time(u32 sec);
	u32 dwEndTime = timeGetTime();

	xr_sprintf(
		stats, 
		"Time elapsed: %s \r\n xrLC: %s\r\n xrAI: %s\r\n xrDO: %s", 
		make_time((dwEndTime - dwStartupTime) / 1000).c_str(), 
		make_time(dwTimeLC).c_str(),
		make_time(dwTimeAI).c_str(), 
		make_time(dwTimeDO).c_str()
	);

	if (!gCompilerMode.Silent)
	{
		MessageBoxA(logWindow, stats, "Congratulation!", MB_OK | MB_ICONINFORMATION);
	}

	extern volatile BOOL bClose;

	// Close log
	bClose = TRUE;
	xrLogger::FlushLog();

	ShowMainUI = true;
	Sleep(200);
}

void SDL_Application()
{
	if (SDL_Init(SDL_INIT_TIMER) != 0)
	{
		printf("Error: SDL_Init(): %s\n", SDL_GetError());
		return;
	}

	// Enable native IME.
	SDL_SetHint(SDL_HINT_IME_SHOW_UI, "1");

	SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
	g_AppInfo.Window = SDL_CreateWindow("IXR Level Builder", 1000, 560, window_flags);
	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);

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

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault(); 

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	gCompilerMode.CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault());

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
			RenderMainUI();
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

void StartCompile()
{
	// Give a LOG-thread a chance to startup
	InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);

	/*while (!logWindow)
		Sleep(100);*/
	
	
}

int APIENTRY WinMain (
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR     lpCmdLine,
	int       nCmdShow
) {
	// Initialize debugging
	Debug._initialize(false);
	Core._initialize("IX-Ray Compilers");

#if true
	InitializeUIData();
	SDL_Application();
#else
	// Read modes
	bool SupportAll = strstr(lpCmdLine, "-all");
	gCompilerMode.AI = SupportAll || strstr(lpCmdLine, "-ai");
	gCompilerMode.LC = SupportAll || strstr(lpCmdLine, "-lc");
	gCompilerMode.DO = SupportAll || strstr(lpCmdLine, "-do");

	gCompilerMode.Silent = strstr(lpCmdLine, "-silent");

	// Give a LOG-thread a chance to startup
	InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);

	while (!logWindow)
		Sleep(100);

	Startup(lpCmdLine);
#endif

	return 0;
}
