#include "xrCore.h"
#include "FormatParsers/json/JsonSerialize.h"
#include "../xrForms/cl_log.h"
#include "ConverterUI.h"

#include <luabind/luabind.hpp>
#include <imgui.h>
#include <timeapi.h>

#include "../../Editors/xrEUI/imgui_impl_sdl3.h"
#include "../xrForms/imgui_impl_sdlrenderer3.h"

#include "CompilerIcons.h"

extern int item_current_cform;
extern int item_current_geom;

void StartupConv();

void Help(const char* h_str)
{
	MessageBoxA(0, h_str, "Command line options", MB_OK | MB_ICONINFORMATION);
}

CJsonSerializer* Serializer = nullptr;

extern bool ShowMainUI;
void Startup(LPSTR lpCmdLine) 
{
 	xrLogger::EnableFastDebugLog();

    CFormConverter::SaveCompilerCfg();

	GetIterationData().push_back({ "xrLC" });
	GetIterationData().push_back({ "xrAI" });
	GetIterationData().push_back({ "xrDO" });

	u32 dwStartupTime = timeGetTime();

	SetActiveIteration(&(GetIterationData()[0]));
	u32 dwTimeConv = 0;
	{
 		GetActiveIteration()->status = InProgress;
 		dwTimeConv = timeGetTime();
 		Phase("CForm convertion Startup");
 		StartupConv();

 		dwTimeConv = (timeGetTime() - dwTimeConv) / 1000;

 		GetActiveIteration()->status = Complete;
 		GetActiveIteration()->elapsed_time = dwTimeConv;
	}

	// Show statistic
	string256 stats;
	extern xr_string make_time(u32 sec);
	u32 dwEndTime = timeGetTime();

	xr_sprintf(
		stats, 
		"Time elapsed: %s \r\n CForm convert: %s", 
		make_time((dwEndTime - dwStartupTime) / 1000).c_str(), 
		make_time(dwTimeConv).c_str()
	);

	/*if (!gCompilerMode.Silent)
	{
		MessageBoxA(nullptr, stats, "Congratulations!", MB_OK | MB_ICONINFORMATION);
	}*/

	extern volatile bool bClose;

	// Close log
	bClose = true;
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
	g_AppInfo.Window = SDL_CreateWindow("IX-Ray CForm converter", 1000, 560, window_flags);
	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL);

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);

	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO(); (void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls

	// Setup Dear ImGui style
	XRay::ImGui::MakeEditorTheme();

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault(); 

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	CFormConverter::GetConverterSettings().CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault());
	CFormConverter::GetConverterSettings().ThreadsPerWork = CPU::ID().n_threads - 1;

	bool done = false;

	// se7kills (4000 FPS) !!!
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
			CFormConverter::RenderMainUI();
		}

		// Rendering
		ImGui::Render();

		SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
		SDL_RenderClear(renderer);
		ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());
		SDL_RenderPresent(renderer);

		// se7kills (fix big GPU Usage)
		Sleep(41);
	}

	// Cleanup
	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(g_AppInfo.Window);
	SDL_Quit();

}

void CFormConverter::StartCompile()
{
	// Give a LOG-thread a chance to startup
	//InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);
}

void CFormConverter::SaveCompilerCfg()
{
	Serializer->Write("item_current_cform", item_current_cform);
	Serializer->Write("item_current_geom", item_current_geom);
	Serializer->Write("LC_CformType", CFormConverter::GetConverterSettings().LC_CformType);
	Serializer->Write("LC_GeomType", CFormConverter::GetConverterSettings().LC_GeomType);
	Serializer->Write("LC_CFormChunkSize", CFormConverter::GetConverterSettings().LC_CFormChunkSize);
	Serializer->Write("LC_GeomChunkSize", CFormConverter::GetConverterSettings().LC_GeomChunkSize);
	Serializer->Save();
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

	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(lpCmdLine, fsgame_ltx_name))
	{
		int sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(lpCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}
	Core._initialize("IX-Ray Compilers", nullptr, true, fsgame[0] ? fsgame : nullptr);

	Serializer = new CJsonSerializer("xrcformconverter.json");
	Serializer->Read("item_current_cform", item_current_cform);
	Serializer->Read("item_current_geom", item_current_geom);
	Serializer->Read("LC_CformType", CFormConverter::GetConverterSettings().LC_CformType);
	Serializer->Read("LC_GeomType", CFormConverter::GetConverterSettings().LC_GeomType);
	Serializer->Read("LC_CFormChunkSize", CFormConverter::GetConverterSettings().LC_CFormChunkSize);
	Serializer->Read("LC_GeomChunkSize", CFormConverter::GetConverterSettings().LC_GeomChunkSize);

	CFormConverter::InitializeUIData();
	SDL_Application();

	CFormConverter::SaveCompilerCfg();

	xr_delete(Serializer);

	return 0;
}
