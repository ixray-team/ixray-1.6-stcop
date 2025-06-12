#include "../../xrCore/xrCore.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <luabind/luabind.hpp>
#include <imgui.h>

#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

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

void StartupAI(LPSTR lpCmdLine);
void StartupLC(LPSTR lpCmdLine);
void StartupDO(LPSTR lpCmdLine);

void InitialFactory();
void DestroyFactory();

void Help(const char* h_str) {
	MessageBoxA(0, h_str, "Command line options", MB_OK | MB_ICONINFORMATION);
}

CompilersMode gCompilerMode;

void Startup(LPSTR lpCmdLine) 
{
	u32 dwStartupTime = timeGetTime();

	u32 dwTimeLC = 0;
	if (gCompilerMode.LC) {
		dwTimeLC = timeGetTime();
		Phase("xrLC Startup");
		StartupLC(lpCmdLine);

		dwTimeLC = (timeGetTime() - dwTimeLC) / 1000;
	}

	u32 dwTimeAI = 0;
	if (gCompilerMode.AI)
	{
		dwTimeAI = timeGetTime();
		Phase("xrAI Startup");

		setup_luabind_allocator();
		InitialFactory();
		StartupAI(lpCmdLine);
		DestroyFactory();
		dwTimeAI = (timeGetTime() - dwTimeAI) / 1000;
	}

	u32 dwTimeDO = 0;
	if (gCompilerMode.DO) {
		dwTimeDO = timeGetTime();
		Phase("xrDO Startup");
		StartupDO(lpCmdLine);
		dwTimeDO = (timeGetTime() - dwTimeDO) / 1000;
	}

	// Show statistic
	string256 stats;
	extern std::string make_time(u32 sec);
	u32 dwEndTime = timeGetTime();

	xr_sprintf(
		stats, 
		"Time elapsed: %s \r\n xrLC: %s\r\n xrAI: %s\r\n xrDO: %s", 
		make_time((dwEndTime - dwStartupTime) / 1000).c_str(), 
		make_time(dwTimeLC).c_str(),
		make_time(dwTimeAI).c_str(), 
		make_time(dwTimeDO).c_str()
	);

	if (!gCompilerMode.Silent) {
		MessageBoxA(logWindow, stats, "Congratulation!", MB_OK | MB_ICONINFORMATION);
	}

	extern volatile BOOL bClose;

	// Close log
	bClose = TRUE;
	xrLogger::FlushLog();
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
	g_AppInfo.Window = SDL_CreateWindow("IXR Level Builder", 430, 560, window_flags);
	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);


	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO(); (void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

	// Setup Dear ImGui style
	ImGui::StyleColorsDark();
	//ImGui::StyleColorsLight();

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
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

int APIENTRY WinMain (
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR     lpCmdLine,
	int       nCmdShow
) {
	// Initialize debugging
	Debug._initialize(false);
	Core._initialize("IX-Ray Compilers");

#if 0
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
