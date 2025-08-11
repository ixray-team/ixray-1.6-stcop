#include "../../xrCore/xrCore.h"
#include "../../xrCore/FormatParsers/json/JsonSerialize.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <luabind/luabind.hpp>
#include <imgui.h>

#include "../../editors/xrEUI/imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "CompilerIcons.h"

static LPVOID __cdecl luabind_allocator(luabind::memory_allocation_function_parameter const, void const* const pointer, size_t const size)
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

void StartupAI();
void StartupLC();
void StartupDO();

void InitialFactory();
void DestroyFactory();

void Help(const char* h_str)
{
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
	if (gCompilerMode.LC)
	{
		GetActiveIteration()->status = InProgress;
		dwTimeLC = timeGetTime();
		Phase("xrLC Startup");
		StartupLC();

		dwTimeLC = (timeGetTime() - dwTimeLC) / 1000;

		GetActiveIteration()->status = Complete;
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

		GetActiveIteration()->status = Complete;
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

		GetActiveIteration()->status = Complete;
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
		MessageBoxA(nullptr, stats, "Congratulations!", MB_OK | MB_ICONINFORMATION);
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
	if (!SDL_Init(SDL_INIT_EVENTS) != 0)
	{
		printf("Error: SDL_Init(): %s\n", SDL_GetError());
		return;
	}

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
	XRay::ImGui::MakeEditorTheme();

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault(); 

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	gCompilerMode.CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault());
	gCompilerMode.ThreadsPerWork = CPU::ID.n_threads - 1;

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
}

int APIENTRY WinMain 
(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR     lpCmdLine,
	int       nCmdShow
) 
{
	// Initialize debugging
	Debug._initialize(false);
	Core._initialize("IX-Ray Compilers");

	CJsonSerializer Serializer("xrlevelbuilder.json");
	Serializer.Read("ai", gCompilerMode.AI);
	Serializer.Read("lc", gCompilerMode.LC);
	Serializer.Read("do", gCompilerMode.DO);
	Serializer.Read("Silent", gCompilerMode.Silent);
	Serializer.Read("Embree", gCompilerMode.Embree);
	Serializer.Read("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer.Read("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer.Read("ClearTemp", gCompilerMode.ClearTemp);
	Serializer.Read("SkipTHM", gCompilerMode.SkipTHM);
	Serializer.Read("LC_BackingDisabled", gCompilerMode.LC_BackingDisabled);
	Serializer.Read("LC_SaveOFG", gCompilerMode.LC_SaveOFG);
	Serializer.Read("LC_GI", gCompilerMode.LC_GI);
	Serializer.Read("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer.Read("LC_NoSMG", gCompilerMode.LC_NoSMG);
	Serializer.Read("LC_Noise", gCompilerMode.LC_Noise);
	Serializer.Read("LC_Tess", gCompilerMode.LC_Tess);
	Serializer.Read("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer.Read("LC_tex_rgba", gCompilerMode.LC_tex_rgba);
	Serializer.Read("LC_NoSubdivide", gCompilerMode.LC_NoSubdivide);
	Serializer.Read("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer.Read("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer.Read("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
	Serializer.Read("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer.Read("LC_JSample", gCompilerMode.LC_JSample);
	Serializer.Read("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer.Read("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer.Read("WeldDistance", gCompilerMode.WeldDistance);
	Serializer.Read("DO_NoSun", gCompilerMode.DO_NoSun);
	Serializer.Read("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer.Read("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer.Read("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer.Read("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer.Read("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer.Read("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer.Read("AI_Draft", gCompilerMode.AI_Draft);
	Serializer.Read("AI_Verify", gCompilerMode.AI_Verify);
	Serializer.Read("AI_Verbose", gCompilerMode.AI_Verbose);
	
	InitializeUIData();
	SDL_Application();

	Serializer.Write("ai", gCompilerMode.AI);
	Serializer.Write("lc", gCompilerMode.LC);
	Serializer.Write("do", gCompilerMode.DO);
	Serializer.Write("Silent", gCompilerMode.Silent);
	Serializer.Write("Embree", gCompilerMode.Embree);
	Serializer.Write("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer.Write("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer.Write("ClearTemp", gCompilerMode.ClearTemp);
	Serializer.Write("SkipTHM", gCompilerMode.SkipTHM);
	Serializer.Write("LC_BackingDisabled", gCompilerMode.LC_BackingDisabled);
	Serializer.Write("LC_SaveOFG", gCompilerMode.LC_SaveOFG);
	Serializer.Write("LC_GI", gCompilerMode.LC_GI);
	Serializer.Write("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer.Write("LC_NoSMG", gCompilerMode.LC_NoSMG);
	Serializer.Write("LC_Noise", gCompilerMode.LC_Noise);
	Serializer.Write("LC_Tess", gCompilerMode.LC_Tess);
	Serializer.Write("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer.Write("LC_tex_rgba", gCompilerMode.LC_tex_rgba);
	Serializer.Write("LC_NoSubdivide", gCompilerMode.LC_NoSubdivide);
	Serializer.Write("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer.Write("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer.Write("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
	Serializer.Write("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer.Write("LC_JSample", gCompilerMode.LC_JSample);
	Serializer.Write("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer.Write("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer.Write("WeldDistance", gCompilerMode.WeldDistance);
	Serializer.Write("DO_NoSun", gCompilerMode.DO_NoSun);
	Serializer.Write("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer.Write("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer.Write("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer.Write("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer.Write("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer.Write("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer.Write("AI_Draft", gCompilerMode.AI_Draft);
	Serializer.Write("AI_Verify", gCompilerMode.AI_Verify);
	Serializer.Write("AI_Verbose", gCompilerMode.AI_Verbose);

	return 0;
}
