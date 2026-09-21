#include "../../xrCore/xrCore.h"
#include "../../xrCore/FormatParsers/json/JsonSerialize.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <imgui.h>
#include <timeapi.h>

#include "../../Editors/xrEUI/imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "CompilerIcons.h"

// >>> UX-PROGRESS
#include "TaskbarProgress.h"
#include "ToastNotify.h"
// <<< UX-PROGRESS


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


// >>> UX-PROGRESS
static xr_string GetBuildingLevelName()
{
	xr_string result;

	for (const auto& FILE : gCompilerMode.Files)
	{
		if (FILE.Select)
		{
			if (!result.empty())
			{
				result += ", ";
			}

			result += FILE.Name.c_str();
		}
	}

	if (result.empty())
	{
		result = "<unknown>";
	}

	return result;
}

// =========================================================================
// ������� ���� �������� � �������-������ � ������ �����.
// =========================================================================
static TaskbarOverlay GetOverlayForIteration(LCBuildingType type)
{
	switch (type)
	{
		case LCBuildingType::eLC:
			return TaskbarOverlay::XrLC;
		case LCBuildingType::eAI:
			return TaskbarOverlay::XrAI;
		case LCBuildingType::eDO:
			return TaskbarOverlay::XrDO;
		default:
			return TaskbarOverlay::None;
	}
}

// ������: ������� wide-string �� ���� (LPCSTR).
static std::wstring WidenPhase(LPCSTR phase)
{
	if (!phase)
	{
		return std::wstring();
	}

	int len = (int)xr_strlen(phase);
	return std::wstring(phase, phase + len);
}
// <<< UX-PROGRESS


void Startup(LPSTR lpCmdLine)
{
	xrLogger::EnableFastDebugLog();

	SaveCompilerCfg();

	GetIterationData().push_back({"xrLC"});
	GetIterationData().push_back({"xrAI"});
	GetIterationData().push_back({"xrDO"});

	// >>> UX-PROGRESS
	const xr_string levelName = GetBuildingLevelName();
	const std::wstring wLevel(levelName.begin(), levelName.end());

	clMsg("=== Startup: initializing ToastNotify and TaskbarProgress ===");
	clMsg("* Startup: building level = '%s'", levelName.c_str());

	CToastNotify::Instance().Initialize();

	// Tooltip ��� ������� � ������ ����� (�� ������ ������ ������).
	{
		std::wstring tooltip = L"IX-Ray Level Builder - building: ";
		tooltip += wLevel;
		CTaskbarProgress::Instance().SetTooltip(tooltip);
	}

	// ��������� ���� � ������ ������.
	CToastNotify::Instance().ShowInfo(
		L"IX-Ray Level Builder",
		std::wstring(L"Compilation started: ") + wLevel
	);
	// <<< UX-PROGRESS

	// >>> UX-PROGRESS
	int totalActive = 0;
	if (gCompilerMode.LC)
	{
		++totalActive;
	}
	if (gCompilerMode.AI)
	{
		++totalActive;
	}
	if (gCompilerMode.DO)
	{
		++totalActive;
	}
	int completed = 0;
	clMsg("* Startup: totalActive = %d", totalActive);
	// <<< UX-PROGRESS

	auto InitilizeIteration = [&](LCBuildingType Type, bool active, LPCSTR phase)
	{
		SetActiveIteration(&(GetIterationData()[(int)Type]));
		gCompilerMode.builder_type = Type;

		if (active)
		{
			clMsg("* Startup: iteration %d (%s) - starting", (int)Type, phase);

			// >>> UX-PROGRESS: indeterminate + ����������� ������ ������
			CTaskbarProgress::Instance().SetState(TBPF_INDETERMINATE);

			{
				const TaskbarOverlay overlay = GetOverlayForIteration(Type);
				const std::wstring wPhase = WidenPhase(phase);

				std::wstring desc = L"IX-Ray Level Builder - ";
				desc += wPhase;
				CTaskbarProgress::Instance().SetOverlayIcon(overlay, desc);

				std::wstring tip = L"IX-Ray Level Builder - ";
				tip += wLevel;
				tip += L" (";
				tip += wPhase;
				tip += L")";
				CTaskbarProgress::Instance().SetTooltip(tip);
			}
			// <<< UX-PROGRESS

			GetActiveIteration()->status = InProgress;
			u32 dwTime = timeGetTime();
			Phase(phase);

			if (Type == LCBuildingType::eLC)
			{
				StartupLC();
			}
			else if (Type == LCBuildingType::eDO)
			{
				StartupDO();
			}
			else if (Type == LCBuildingType::eAI)
			{
				StartupAI();
			}

			dwTime = (timeGetTime() - dwTime) / 1000;

			GetActiveIteration()->status = Complete;
			GetActiveIteration()->elapsed_time = dwTime;

			clMsg("* Startup: iteration %d (%s) - complete, elapsed = %u sec", (int)Type, phase, dwTime);
		}
		else
		{
			GetActiveIteration()->status = Skip;
			clMsg("* Startup: iteration %d (%s) - skipped", (int)Type, phase);
		}

		PhaseEnd();

		// >>> UX-PROGRESS: �������� �� ������ ����� �� �������
		++completed;
		if (totalActive > 0)
		{
			CTaskbarProgress::Instance().SetState(TBPF_NORMAL);
			CTaskbarProgress::Instance().SetProgress((ULONGLONG)completed, (ULONGLONG)totalActive);
		}
		// <<< UX-PROGRESS
	};

	InitilizeIteration(LCBuildingType::eLC, gCompilerMode.LC, "xrLC Startup");
	InitilizeIteration(LCBuildingType::eAI, gCompilerMode.AI, "xrAI Startup");
	InitilizeIteration(LCBuildingType::eDO, gCompilerMode.DO, "xrDO Startup");

	// Show statistic
	extern xr_string make_time(u32 sec);
	for (auto& I : GetIterationData())
	{
		clMsg("* Compiler (%s) : Time elapsed: %s ", I.iterationName.c_str(), make_time(I.elapsed_time));
	}

	// Close log
	xrLogger::FlushLog();

	// >>> UX-PROGRESS: ��������� ������� + ����� ���������
	CTaskbarProgress::Instance().Reset();
	CTaskbarProgress::Instance().SetTooltip(L"IX-Ray Level Builder");

	{
		xr_string summary;
		for (auto& I : GetIterationData())
		{
			if (I.status == Complete)
			{
				summary += I.iterationName.c_str();
				summary += ": ";
				summary += make_time(I.elapsed_time).c_str();
				summary += "  ";
			}
		}

		// ���������, ���� �� ������� (�� Complete � �� Skip).
		bool anyFailed = false;
		for (auto& I : GetIterationData())
		{
			if (I.status != Complete && I.status != Skip)
			{
				anyFailed = true;
				break;
			}
		}

		const TaskbarOverlay finalOverlay = anyFailed
												? TaskbarOverlay::Error
												: TaskbarOverlay::Success;

		const wchar_t* finalDesc = anyFailed
									   ? L"Compilation failed"
									   : L"Compilation complete";

		CTaskbarProgress::Instance().SetOverlayIcon(finalOverlay, finalDesc);

		// ��������� ����� � ������ ������.
		std::wstring title = anyFailed
								 ? L"Compilation failed: "
								 : L"Compilation complete: ";
		title += wLevel;

		std::wstring body(summary.begin(), summary.end());
		clMsg("* Startup: showing completion toast, level = '%s'", levelName.c_str());

		if (anyFailed)
		{
			CToastNotify::Instance().ShowError(title, body);
		}
		else
		{
			CToastNotify::Instance().ShowSuccess(title, body);
		}
	}
	// <<< UX-PROGRESS

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

	if (!g_AppInfo.Window)
	{
		clMsg("! SDL_Application: SDL_CreateWindow failed: %s", SDL_GetError());
		return;
	}

	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL);

	if (!renderer)
	{
		clMsg("! SDL_Application: SDL_CreateRenderer failed: %s", SDL_GetError());
		return;
	}

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);

	// >>> UX-PROGRESS: �������� HWND �� SDL3 � ���������������� taskbar/toast
	{
		HWND hwnd = nullptr;
		SDL_PropertiesID props = SDL_GetWindowProperties(g_AppInfo.Window);
		clMsg("* SDL_Application: SDL_GetWindowProperties returned props = %u", (unsigned)props);

		if (props != 0)
		{
			hwnd = static_cast<HWND>(
				SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr)
			);
		}

		clMsg("* SDL_Application: extracted HWND = 0x%p", hwnd);

		if (hwnd)
		{
			if (!CTaskbarProgress::Instance().Initialize(hwnd))
			{
				clMsg("! SDL_Application: TaskbarProgress initialization FAILED");
			}
			else
			{
				clMsg("* SDL_Application: TaskbarProgress initialization OK");
			}
		}
		else
		{
			clMsg("! SDL_Application: HWND not found - taskbar progress unavailable");
		}

		CToastNotify::Instance().Initialize();
	}
	// <<< UX-PROGRESS

	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	(void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

	XRay::ImGui::MakeEditorTheme();

	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault();

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	gCompilerMode.CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(
		IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault()
	);

	gCompilerMode.ThreadsPerWork = CPU::ID().n_threads - 1;

	bool done = false;

	while (!done)
	{
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			ImGui_ImplSDL3_ProcessEvent(&event);
			if (event.type == SDL_EVENT_QUIT)
			{
				done = true;
			}
			if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
				event.window.windowID == SDL_GetWindowID(g_AppInfo.Window))
			{
				done = true;
			}
		}

		ImGui_ImplSDLRenderer3_NewFrame();
		ImGui_ImplSDL3_NewFrame();
		ImGui::NewFrame();

		{
			RenderMainUI();
		}

		ImGui::Render();

		SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
		SDL_RenderClear(renderer);
		ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());
		SDL_RenderPresent(renderer);

		Sleep(41);
	}

	// >>> UX-PROGRESS: ���������� ����������
	clMsg("=== SDL_Application: shutting down UX-PROGRESS ===");
	CToastNotify::Instance().Shutdown();
	CTaskbarProgress::Instance().Release();
	// <<< UX-PROGRESS

	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(g_AppInfo.Window);
	SDL_Quit();
}


void StartCompile()
{
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

	Serializer->Write("LC_fast_way", gCompilerMode.LC_fast_way);
	Serializer->Write("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Write("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Write("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Write("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Write("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	Serializer->Write("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Write("LC_Skip_Striptify", gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Write("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

	Serializer->Save();
}


int APIENTRY WinMain(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR lpCmdLine,
	int nCmdShow
)
{
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

	Serializer->Read("LC_fast_way", gCompilerMode.LC_fast_way);
	Serializer->Read("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Read("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Read("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Read("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Read("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	Serializer->Read("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Read("LC_Skip_Striptify", gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Read("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

	gCompilerMode.LmapsFormat = (LCLightmapFormat)current_format;

	InitializeUIData();
	SDL_Application();

	SaveCompilerCfg();

	xr_delete(Serializer);

	return 0;
}