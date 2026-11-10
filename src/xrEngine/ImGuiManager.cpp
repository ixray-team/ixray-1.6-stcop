#include "stdafx.h"
#include "ImGuiManager.h"
#include "IGame_Persistent.h"
#include "XR_IOConsole.h"

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

		FontHandle = ImGui::GetIO().Fonts->AddFontFromMemoryTTF(FontReader->pointer(), FontReader->length(), scale * 20.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
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
#ifdef IXR_WINDOWS
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();

#ifdef DEBUG_DRAW
	Console->RegisterImGuiConsoleSettingsHandler();
#endif

	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;

	ImGuiStyle& Style = ImGui::GetStyle();
	XRay::ImGui::MakeEditorTheme();

	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		Style.WindowRounding = 0.0f;
		Style.Colors[ImGuiCol_WindowBg].w = 1.0f;
	}

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
#endif
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

#ifdef IXR_WINDOWS
	PlatformDestroyCallback();
#endif

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

void CImGuiManager::UnsubscribeAll()
{
	RenderFrameData.clear();
}

void CImGuiManager::BeginRender()
{
	RHIUtils::ImGui::NewFrame();
}

void CImGuiManager::AfterRender()
{
#ifdef IXR_WINDOWS
	ImGuiIO& io = ImGui::GetIO();

	// Update and Render additional Platform Windows
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
#endif
}

void CImGuiManager::Render()
{
#ifdef IXR_WINDOWS
	if (AppliedScale != psImGuiScale)
	{
		BaseStyle = AppliedScale > 0.0f ? BaseStyle : ImGui::GetStyle();
		ImGuiStyle Scaled = BaseStyle;
		Scaled.ScaleAllSizes(psImGuiScale);
		Scaled.FontScaleMain = psImGuiScale;
		ImGui::GetStyle() = Scaled;
		AppliedScale = psImGuiScale;
	}
	ImGui::NewFrame();

#ifdef DEBUG_DRAW
	if (DrawUIRender && !psDeviceFlags.test(rsClearBB))
	{
		for (const auto& [Id, CommandData] : RenderFrameData)
		{
			CommandData.Function();
		}

		if (!IsCapturingInputs())
		{
			for (auto& Window : ImGui::GetCurrentContext()->Windows) 
			{
				if (Window->Viewport == ImGui::GetMainViewport())
				{
					Window->Flags |= ImGuiWindowFlags_NoInputs;
				}
			}
		}
	}
	else 
	{
#endif
		RenderFrameData[MainViewportSlot].Function();
		if (HiddenCallback)
		{
			HiddenCallback();
		}
#ifdef DEBUG_DRAW
	}
#endif

	ImGui::Render();
#endif

	RHIUtils::ImGui::DrawData();
}

void CImGuiManager::UpdateCapture()
{
	HasPlatformWindowFocus = SDL_GetKeyboardFocus() != g_AppInfo.Window && IsFocused();

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
	if (IsCapturingInputs() || g_dedicated_server)
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
	return CaptureInputs || (DrawUIRender && HasPlatformWindowFocus);
}

bool CImGuiManager::IsFocused() const
{
	SDL_Window* FocusedWindow = SDL_GetKeyboardFocus();
	if (!FocusedWindow)
	{
		return false;
	}

	if (FocusedWindow == g_AppInfo.Window)
	{
		return true;
	}

#ifdef IXR_WINDOWS
	if (ImGui::GetCurrentContext())
	{
		return ImGui::FindViewportByPlatformHandle((void*)(size_t)SDL_GetWindowID(FocusedWindow)) != nullptr;
	}
#endif

	return false;
}

void CImGuiManager::SetMouseLook(bool IsEnabled)
{
	MouseLook = IsEnabled && CaptureInputs;
	MouseLookDelta = ImVec2(0.0f, 0.0f);
	MouseLookSkip = MouseLook ? 2 : 0;
}

float psImGuiScale = 1.0f;

bool CImGuiManager::IsButtonHeld(bool IsMiddle) const
{
	return (SDL_GetMouseState(nullptr, nullptr) & (IsMiddle ? SDL_BUTTON_MMASK : SDL_BUTTON_RMASK)) != 0;
}

bool CImGuiManager::IsMouseLook() const
{
	return MouseLook && CaptureInputs;
}

void CImGuiManager::AddMouseLook(float X, float Y)
{
	MouseLookDelta.x += IsMouseLook() ? X : 0.0f;
	MouseLookDelta.y += IsMouseLook() ? Y : 0.0f;
}

ImVec2 CImGuiManager::TakeMouseLook()
{
	const ImVec2 Delta = MouseLookSkip ? ImVec2(0.0f, 0.0f) : MouseLookDelta;
	MouseLookSkip -= MouseLookSkip ? 1 : 0;
	MouseLookDelta = ImVec2(0.0f, 0.0f);
	return Delta;
}

void CImGuiManager::NewPlatformFrame() const
{
#ifdef IXR_WINDOWS
	PlatformNewFrameCallback();
#endif
}

void CImGuiManager::NewHardwareFrame() const
{
	RHIUtils::ImGui::NewFrame();
}

bool CImGuiManager::IsGUIRendering() const
{
	return DrawUIRender;
}

CImGuiManager& CImGuiManager::Instance()
{
	static CImGuiManager Object;
	return Object;
}
