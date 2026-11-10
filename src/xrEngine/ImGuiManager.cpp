#include "stdafx.h"
#include "ImGuiManager.h"
#include "IGame_Persistent.h"
#include "XR_IOConsole.h"
#include "imgui_impl_sdl3.h"

#include <imgui.h>
#include <imgui_internal.h>
#include <imgui_user.h>

constexpr u32 MainViewportSlot = 0;

namespace ImGui
{
	ImFont* LightFont = nullptr;
	ImFont* RegularFont = nullptr;
	ImFont* MediumFont = nullptr;
	ImFont* BoldFont = nullptr;
}

IImGuiManager* GImGUIManager = &CImGuiManager::Engine();

//////////////////////////////////////////////////////////////////////////
// CImGuiWindowRegistry

void CImGuiWindowRegistry::SetMainViewport(DrawFunction&& Function)
{
	Commands[MainViewportSlot] = { "MainViewPort", std::move(Function) };
}

void CImGuiWindowRegistry::Subscribe(shared_str Name, u32 RenderPriority, DrawFunction&& Function)
{
	if (RenderPriority == MainViewportSlot)
	{
		RenderPriority++;
	}

	while (Commands.contains(RenderPriority))
	{
		RenderPriority++;
	}

	Commands[RenderPriority] = { Name, std::move(Function) };
}

void CImGuiWindowRegistry::Unsubscribe(shared_str Name)
{
	for (auto It = Commands.begin(); It != Commands.end(); ++It)
	{
		if (It->second.Name == Name)
		{
			Commands.erase(It);
			return;
		}
	}
}

void CImGuiWindowRegistry::UnsubscribeAll()
{
	Commands.clear();
}

void CImGuiWindowRegistry::DrawMainViewport() const
{
	auto It = Commands.find(MainViewportSlot);
	if (It != Commands.end() && It->second.Function)
	{
		It->second.Function();
	}
}

void CImGuiWindowRegistry::DrawAll() const
{
	for (const auto& [Id, Command] : Commands)
	{
		if (Command.Function)
		{
			Command.Function();
		}
	}
}

//////////////////////////////////////////////////////////////////////////
// CImGuiManager

CImGuiManager& CImGuiManager::Engine()
{
	static CImGuiManager Object;
	return Object;
}

void CImGuiManager::LoadFont(ImFont*& FontHandle, const char* Font, float Scale)
{
	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, Font);
	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	if (FS.exist(FullPath))
	{
		IReader* FontReader = FS.r_open(FullPath);

		FontHandle = ImGui::GetIO().Fonts->AddFontFromMemoryTTF(FontReader->pointer(), FontReader->length(), Scale * 16.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		R_ASSERT(FontHandle);

		FontReaders.push_back(FontReader);
	}
}

static float GetWindowScale(SDL_Window* window)
{
	int w_px, h_px;
	SDL_GetWindowSizeInPixels(window, &w_px, &h_px);

	int w_log, h_log;
	SDL_GetWindowSize(window, &w_log, &h_log);

	if (w_log == 0)
	{
		return 1.0f;
	}

	return (float)w_px / (float)w_log;
}

void CImGuiManager::InitPlatform()
{
#ifdef IXR_WINDOWS
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();

#ifdef DEBUG_DRAW
	Console->RegisterImGuiConsoleSettingsHandler();
#endif

	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	//io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;       // Enable Multi-Viewport / Platform Windows

	ImGuiStyle& Style = ImGui::GetStyle();
	XRay::ImGui::MakeEditorTheme();

	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		Style.WindowRounding = 0.0f;
		Style.Colors[ImGuiCol_WindowBg].w = 1.0f;
	}

	float scale = GetWindowScale(g_AppInfo.Window);

	LoadFont(ImGui::RegularFont, "rus\\RobotoMono.ttf", scale);
	LoadFont(ImGui::LightFont, "rus\\RobotoMono-Light.ttf", scale);
	LoadFont(ImGui::MediumFont, "rus\\RobotoMono-Medium.ttf", scale);
	LoadFont(ImGui::BoldFont, "rus\\RobotoMono-Bold.ttf", scale);

	//io.Fonts->Build();

#ifdef DEBUG_DRAW
	if (Core.ParamsData.test(ECoreParams::no_debug_panel))
		DrawUIRender = false;
#endif

	ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window);
#endif
}

void CImGuiManager::DestroyPlatform()
{
#ifdef IXR_WINDOWS
	ImGui_ImplSDL3_Shutdown();
#endif

	for (IReader* FontReader : FontReaders)
	{
		FS.r_close(FontReader);
	}
	FontReaders.clear();

	Windows().UnsubscribeAll();
}

void CImGuiManager::NewPlatformFrame()
{
#ifdef IXR_WINDOWS
	ImGui_ImplSDL3_NewFrame();
	UpdateCapture();
#endif
}

void CImGuiManager::InitDevice()
{
	RHIUtils::ImGui::Init();
}

void CImGuiManager::ResetDevice()
{
	RHIUtils::ImGui::Reset();
}

void CImGuiManager::DestroyDevice()
{
	RHIUtils::ImGui::Destroy();
}

void CImGuiManager::RenderFrame()
{
	RHIUtils::ImGui::NewFrame();

#ifdef IXR_WINDOWS
	ImGui::NewFrame();

#ifdef DEBUG_DRAW
	if (DrawUIRender && !psDeviceFlags.test(rsClearBB))
	{
		Windows().DrawAll();

		if (!CaptureInputs)
		{
			for (auto& Window : ImGui::GetCurrentContext()->Windows) 
			{
				Window->Flags |= ImGuiWindowFlags_NoInputs;
			}
		}
	}
	else 
	{
#endif
		Windows().DrawMainViewport();
#ifdef DEBUG_DRAW
	}
#endif

	ImGui::Render();
#endif

	RHIUtils::ImGui::DrawData();

#ifdef IXR_WINDOWS
	// Update and Render additional Platform Windows
	if (ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
#endif
}

void CImGuiManager::UpdateCapture()
{
	if (ImGui::IsKeyDown(ImGuiMod_Alt))
	{
		if (!CaptureKeyProcessed)
		{
			if (ImGui::IsKeyDown(ImGuiMod_Ctrl))
			{
				DrawUIRender = !DrawUIRender;

				if (!DrawUIRender)
					CaptureInputs = false;
			}
			else if (DrawUIRender)
			{
				CaptureInputs = !CaptureInputs;
			}

			CaptureKeyProcessed = true;
		}
	}
	else
	{
		CaptureKeyProcessed = false;
	}

	ImGuiIO& IO = ImGui::GetIO();
	if (CaptureInputs || g_dedicated_server)
	{
		IO.ConfigFlags &= ~ImGuiConfigFlags_NoMouseCursorChange;
		SDL_ShowCursor();
	}
	else if (!g_dedicated_server)
	{
		IO.ConfigFlags |= ImGuiConfigFlags_NoMouseCursorChange;
		SDL_HideCursor();
	}
}

bool CImGuiManager::IsCapturingInputs() const
{
	return CaptureInputs;
}

bool CImGuiManager::IsGUIRendering() const
{
	return DrawUIRender;
}
