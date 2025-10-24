#include "stdafx.h"
#include "ImGuiManager.h"
#include "IGame_Persistent.h"

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

void CImGuiManager::LoadImGuiFont(ImFont*& FontHandle, const char* Font, const float scale)
{
	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, Font);
	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	if (FS.exist(FullPath))
	{
		IReader* FontReader = FS.r_open(FullPath);

		FontHandle = ImGui::GetIO().Fonts->AddFontFromMemoryTTF(FontReader->pointer(), FontReader->length(), scale * 16.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		R_ASSERT(FontHandle);

		ImGuiFontsPtr.push_back(FontReader);
	}
}

float GetWindowScale(SDL_Window* window)
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
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	//io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;       // Enable Multi-Viewport / Platform Windows

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();

	ImGuiStyle& Style = ImGui::GetStyle();
	XRay::ImGui::MakeEditorTheme();

	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		Style.WindowRounding = 0.0f;
		Style.Colors[ImGuiCol_WindowBg].w = 1.0f;
	}

	float ddpi, hdpi, vdpi;
	float scale = GetWindowScale(g_AppInfo.Window);

	LoadImGuiFont(ImGui::RegularFont, "rus\\RobotoMono.ttf", scale);
	LoadImGuiFont(ImGui::LightFont, "rus\\RobotoMono-Light.ttf", scale);
	LoadImGuiFont(ImGui::MediumFont, "rus\\RobotoMono-Medium.ttf", scale);
	LoadImGuiFont(ImGui::BoldFont, "rus\\RobotoMono-Bold.ttf", scale);

	//io.Fonts->Build();

#ifdef DEBUG_DRAW
	if (Core.ParamsData.test(ECoreParams::no_debug_panel))
		DrawUIRender = false;
#endif

	PlatformInitCallback();
}

void CImGuiManager::InitHardware()
{
	RHIUtils::ImGui::Init();
}

void CImGuiManager::Reset()
{
	RHIUtils::ImGui::Reset();
}

void CImGuiManager::Destroy(bool HardwareOnly)
{
	if (HardwareOnly)
	{
		RHIUtils::ImGui::Destroy();
		return;
	}

	PlatformDestroyCallback();

	for (IReader* FontPtr : ImGuiFontsPtr)
	{
		FS.r_close(FontPtr);
	}

	RenderFrameData.clear();

	PlatformInitCallback = nullptr;
	PlatformDestroyCallback = nullptr;
	PlatformNewFrameCallback = nullptr;
}

void CImGuiManager::ApplyMainViewport(ImGuiCallback RenderFunction)
{
	RenderFrameData[MainViewportSlot] = { "MainViewPort", RenderFunction };
}

void CImGuiManager::Subscribe(shared_str Name, u32 RenderPriority, ImGuiCallback&& RenderFunction)
{
	if (RenderPriority == MainViewportSlot)
	{
		RenderPriority++;
	}

	do
	{
		if (!RenderFrameData.contains(RenderPriority))
		{
			RenderFrameData[RenderPriority] = { Name, RenderFunction };
			break;
		}
		RenderPriority++;
	} while (true);
}

void CImGuiManager::Unsubscribe(shared_str Name)
{
	u32 EraseID = u32(-1);
	for (auto& [ID, Data] : RenderFrameData)
	{
		if (Data.Name == Name)
		{
			EraseID = ID;
			break;
		}
	}

	if (EraseID != u32(-1))
	{
		RenderFrameData.erase(EraseID);
	}
}

void CImGuiManager::BeginRender()
{
	RHIUtils::ImGui::NewFrame();
}

void CImGuiManager::AfterRender()
{
	ImGuiIO& io = ImGui::GetIO();

	// Update and Render additional Platform Windows
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
}

void CImGuiManager::Render()
{
	ImGui::NewFrame();

#ifdef DEBUG_DRAW
	if (DrawUIRender)
	{
		for (const auto& [Id, CommandData] : RenderFrameData)
		{
			CommandData.Function();
		}

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
		RenderFrameData[MainViewportSlot].Function();
#ifdef DEBUG_DRAW
	}
#endif

	ImGui::Render();

	RHIUtils::ImGui::DrawData();
}

void CImGuiManager::UpdateCapture()
{
    static bool keyProcessed = false;

    if (ImGui::IsKeyDown(ImGuiMod_Alt))
    {
        if (!keyProcessed)
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

            keyProcessed = true;
        }
    }
    else
    {
        keyProcessed = false;
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

void CImGuiManager::NewPlatformFrame() const
{
	PlatformNewFrameCallback();
}

void CImGuiManager::NewHardwareFrame() const
{
	RHIUtils::ImGui::NewFrame();
}

CImGuiManager& CImGuiManager::Instance()
{
	static CImGuiManager Object;
	return Object;
}
