#include "../../xrCore/stdafx.h"
#include "imgui.h"
#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "NodeEditor.h"

struct FAppInfo
{
	SDL_Window* Window = nullptr;
	SDL_Renderer* Renderer = nullptr;
	int Width = 1280;
	int Height = 720;
};

FAppInfo GAppInfo;

bool InitializeImGui()
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
	io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;

	ImGui::StyleColorsDark();

	if (!ImGui_ImplSDL3_InitForSDLRenderer(GAppInfo.Window, GAppInfo.Renderer))
	{
		Msg("! ImGui_ImplSDL3_InitForSDLRenderer Error");
		return false;
	}

	if (!ImGui_ImplSDLRenderer3_Init(GAppInfo.Renderer))
	{
		Msg("! ImGui_ImplSDLRenderer3_Init Error");
		return false;
	}

	return true;
}

void ShutdownImGui()
{
	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();
}

void RenderImGui()
{
	ImGui_ImplSDLRenderer3_NewFrame();
	ImGui_ImplSDL3_NewFrame();
	ImGui::NewFrame();

	ImGuiViewport* viewport = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(viewport->Pos);
	ImGui::SetNextWindowSize(viewport->Size);
	ImGuiWindowFlags windowFlags = ImGuiWindowFlags_NoTitleBar |
		ImGuiWindowFlags_NoCollapse |
		ImGuiWindowFlags_NoResize |
		ImGuiWindowFlags_NoMove |
		ImGuiWindowFlags_NoBringToFrontOnFocus |
		ImGuiWindowFlags_NoNavFocus |
		ImGuiWindowFlags_MenuBar; 

	ImGui::Begin("Behavior Logic Editor", nullptr, windowFlags);
	GNodeEditor->Render();
	ImGui::End();

	// Рендерим ImGui
	ImGui::Render();
	SDL_RenderClear(GAppInfo.Renderer);
	ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());

	ImGuiIO& io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		SDL_Window* backup_current_window = SDL_GetKeyboardFocus();
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}

	SDL_RenderPresent(GAppInfo.Renderer);
}

// Основная функция WinMain
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, char* pCmdLine, int nCmdShow)
{
	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS | SDL_INIT_VIDEO))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}

	// Создание окна
	GAppInfo.Window = SDL_CreateWindow(
		"Logic Editor - S.T.A.L.K.E.R. Call of Pripyat",
		GAppInfo.Width,
		GAppInfo.Height,
		SDL_WINDOW_RESIZABLE
	);

	if (!GAppInfo.Window)
	{
		Msg("! SDL_CreateWindow Error: %s", SDL_GetError());
		SDL_Quit();
		return 0;
	}

	// Создание рендерера
	GAppInfo.Renderer = SDL_CreateRenderer(GAppInfo.Window, nullptr);

	if (!GAppInfo.Renderer)
	{
		Msg("! SDL_CreateRenderer Error: %s", SDL_GetError());
		SDL_DestroyWindow(GAppInfo.Window);
		SDL_Quit();
		return 0;
	}

	Debug._initialize(false);

	const char* FSName = "fsgame.ltx";
	LPCSTR fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(pCmdLine, fsgame_ltx_name))
	{
		int sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(pCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}

	CFilewatcher::instance().SetFilewatcherActive(true);
	Core._initialize("LogicEditor", nullptr, 1, fsgame[0] ? fsgame : FSName);

	// Инициализация ImGui
	if (!InitializeImGui())
	{
		Msg("! Failed to initialize ImGui");
		Core._destroy();
		SDL_DestroyRenderer(GAppInfo.Renderer);
		SDL_DestroyWindow(GAppInfo.Window);
		SDL_Quit();
		return 0;
	}

	GNodeEditor = new FNodeEditor;

	bool NeedExit = false;
	while (!NeedExit)
	{
		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			ImGui_ImplSDL3_ProcessEvent(&Event);

			switch (Event.type)
			{
			case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
			{
				SDL_WindowID MainWndID = SDL_GetWindowID(GAppInfo.Window);
				if (Event.window.windowID == MainWndID)
				{
					NeedExit = true;
				}
				break;
			}
			case SDL_EVENT_QUIT:
			{
				NeedExit = true;
				break;
			}
			case SDL_EVENT_KEY_DOWN:
			{
				if (Event.key.scancode == SDL_SCANCODE_ESCAPE)
				{
					NeedExit = true;
				}
				break;
			}
			case SDL_EVENT_WINDOW_RESIZED:
			{
				SDL_GetWindowSize(GAppInfo.Window, &GAppInfo.Width, &GAppInfo.Height);
				break;
			}
			}
		}

		RenderImGui();
	}

	ShutdownImGui();
	Core._destroy();
	SDL_DestroyRenderer(GAppInfo.Renderer);
	SDL_DestroyWindow(GAppInfo.Window);
	SDL_Quit();

	return 0;
}