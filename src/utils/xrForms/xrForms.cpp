#include "../../xrCore/xrCore.h"
#include "../../xrCore/FormatParsers/json/JsonSerialize.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <imgui.h>
#include <timeapi.h>

#include "../../Editors/xrEUI/imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "CompilerIcons.h"


extern int item_current_lightmap;
extern int item_current_cform;
extern int item_current_geom;
extern int item_current_jitter;
extern int item_current_jitter_mu;
extern int current_format;

void StartupAI();
void StartupLC();
void StartupDO();

CompilersMode gCompilerMode;
CJsonSerializer* Serializer = nullptr;

extern bool ShowMainUI;
void Startup(LPSTR lpCmdLine) 
{
 	xrLogger::EnableFastDebugLog();

	SaveCompilerCfg();

	GetIterationData().push_back({ "xrLC" });
	GetIterationData().push_back({ "xrAI" });
	GetIterationData().push_back({ "xrDO" });
	 
	auto InitilizeIteration = [](LCBuildingType Type, bool active, LPCSTR phase)
	{
		SetActiveIteration(&(GetIterationData()[(int)Type]));
		gCompilerMode.builder_type = Type;
		if (active)
		{
			GetActiveIteration()->status = InProgress;
			u32 dwTime = timeGetTime();
			Phase(phase);
			
			if (Type == LCBuildingType::eLC)
				StartupLC();
			else if (Type == LCBuildingType::eDO)
				StartupDO();
			else if (Type == LCBuildingType::eAI)
  				StartupAI();
 			
			dwTime = (timeGetTime() - dwTime) / 1000;

			GetActiveIteration()->status = Complete;
			GetActiveIteration()->elapsed_time = dwTime;
		}
		else
 			GetActiveIteration()->status = Skip;

		PhaseEnd();
 	};

	InitilizeIteration(LCBuildingType::eLC, gCompilerMode.LC, "xrLC Startup");
	InitilizeIteration(LCBuildingType::eAI, gCompilerMode.AI, "xrAI Startup");
	InitilizeIteration(LCBuildingType::eDO, gCompilerMode.DO, "xrDO Startup");

	// Show statistic
	extern xr_string make_time(u32 sec);
	for (auto& I : GetIterationData())
	{
		// Много лога вырубил !
		// for (auto& PH : I.phases)
		// 	clMsg("* %40s  : Time elapsed %s", PH.PhaseName.c_str(), make_time(PH.elapsed_time));

		clMsg("* Compiler (%s) : Time elapsed: %s ", I.iterationName.c_str(), make_time(I.elapsed_time));
	} 

	// Close log
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

	// Setup Dear ImGui style
	XRay::ImGui::MakeRedTheme();

	// Setup Platform/Renderer backends
	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault(); 

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	gCompilerMode.CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault());
	gCompilerMode.ThreadsPerWork = CPU::ID().n_threads - 1;

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
			RenderMainUI();
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

void StartCompile()
{
	// Give a LOG-thread a chance to startup
	//InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);
}

void SaveCompilerCfg()
{
	Serializer->Write("ai", gCompilerMode.AI);
	Serializer->Write("lc", gCompilerMode.LC);
	Serializer->Write("do", gCompilerMode.DO);
	Serializer->Write("Silent", gCompilerMode.Silent);
	Serializer->Write("Embree", gCompilerMode.Embree);
	Serializer->Write("CUDA", gCompilerMode.CUDA);
	Serializer->Write("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer->Write("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer->Write("ClearTemp", gCompilerMode.ClearTemp);
	Serializer->Write("SkipTHM", gCompilerMode.SkipTHM);
	Serializer->Write("LC_BackingDisabled", gCompilerMode.LC_BackingDisabled);
	Serializer->Write("LC_SaveOFG", gCompilerMode.LC_SaveOFG);
	Serializer->Write("LC_GI", gCompilerMode.LC_GI);
	
	Serializer->Write("LC_SkipStaticMap", gCompilerMode.LC_SkipStaticMap);
	Serializer->Write("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer->Write("LC_NoSMG", gCompilerMode.LC_NoSMG);
 	Serializer->Write("LC_Tess", gCompilerMode.LC_Tess);
	Serializer->Write("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer->Write("LC_tex_format", current_format);
 	Serializer->Write("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer->Write("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer->Write("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
 	Serializer->Write("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer->Write("LC_JSample", gCompilerMode.LC_JSample);
	Serializer->Write("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer->Write("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer->Write("WeldDistance", gCompilerMode.WeldDistance);
	Serializer->Write("DO_NoSun", gCompilerMode.DO_NoSun);
	Serializer->Write("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer->Write("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer->Write("AI_FreeMPBuild", gCompilerMode.AI_FreeMPBuild);
	Serializer->Write("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer->Write("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer->Write("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer->Write("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer->Write("AI_Draft", gCompilerMode.AI_Draft);
	Serializer->Write("AI_Verify", gCompilerMode.AI_Verify);
	Serializer->Write("AI_Verbose", gCompilerMode.AI_Verbose);

	Serializer->Write("item_current_selected", item_current_lightmap);
	Serializer->Write("item_current_cform", item_current_cform);
	Serializer->Write("item_current_geom", item_current_geom);
	Serializer->Write("item_current_jitter", item_current_jitter);
	Serializer->Write("item_current_jitter_mu", item_current_jitter_mu);
	Serializer->Write("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Write("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Write("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Write("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Write("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	// new Geometry Optimization off
	Serializer->Write("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Write("LC_Skip_Striptify",   gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Write("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

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

	Serializer = new CJsonSerializer("xrlevelbuilder.json");
	Serializer->Read("ai", gCompilerMode.AI);
	Serializer->Read("lc", gCompilerMode.LC);
	Serializer->Read("do", gCompilerMode.DO);
	Serializer->Read("Silent", gCompilerMode.Silent);
	Serializer->Read("Embree", gCompilerMode.Embree);
	Serializer->Read("CUDA", gCompilerMode.CUDA);
	Serializer->Read("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer->Read("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer->Read("ClearTemp", gCompilerMode.ClearTemp);
	Serializer->Read("SkipTHM", gCompilerMode.SkipTHM);
	Serializer->Read("LC_BackingDisabled", gCompilerMode.LC_BackingDisabled);
	Serializer->Read("LC_SaveOFG", gCompilerMode.LC_SaveOFG);
	Serializer->Read("LC_GI", gCompilerMode.LC_GI);

	Serializer->Read("LC_SkipStaticMap", gCompilerMode.LC_SkipStaticMap);
	Serializer->Read("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer->Read("LC_NoSMG", gCompilerMode.LC_NoSMG);
 	Serializer->Read("LC_Tess", gCompilerMode.LC_Tess);
	Serializer->Read("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer->Read("LC_tex_format", current_format);
 	Serializer->Read("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer->Read("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer->Read("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
	Serializer->Read("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer->Read("LC_JSample", gCompilerMode.LC_JSample);
	Serializer->Read("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer->Read("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer->Read("WeldDistance", gCompilerMode.WeldDistance);
	Serializer->Read("DO_NoSun", gCompilerMode.DO_NoSun);
	Serializer->Read("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer->Read("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer->Read("AI_FreeMPBuild", gCompilerMode.AI_FreeMPBuild);
	Serializer->Read("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer->Read("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer->Read("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer->Read("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer->Read("AI_Draft", gCompilerMode.AI_Draft);
	Serializer->Read("AI_Verify", gCompilerMode.AI_Verify);
	Serializer->Read("AI_Verbose", gCompilerMode.AI_Verbose);
	Serializer->Read("item_current_selected", item_current_lightmap);
	Serializer->Read("item_current_cform", item_current_cform);
	Serializer->Read("item_current_geom", item_current_geom);
	Serializer->Read("item_current_jitter", item_current_jitter);
	Serializer->Read("item_current_jitter_mu", item_current_jitter_mu);
 	Serializer->Read("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Read("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Read("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Read("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Read("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	// Geometry
 	Serializer->Read("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Read("LC_Skip_Striptify", gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Read("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

	gCompilerMode.LmapsFormat = (LCLightmapFormat) current_format;

	InitializeUIData();
	SDL_Application();

	SaveCompilerCfg();

	xr_delete(Serializer);

	return 0;
}
