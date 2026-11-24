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
	RHIUtils::ImGui::Init(true);
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
	UpdatePlatform();
	RenderPlatform();
}

void CImGuiManager::UpdatePlatform()
{
#ifdef IXR_WINDOWS
	if (ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
	}
#endif
}

void CImGuiManager::RenderPlatform()
{
#ifdef IXR_WINDOWS
	ImGuiIO& io = ImGui::GetIO();

	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		// The tools' windows are drawn at most 60 times a second, the one with the game's picture every frame
		const bool IsDue = PlatformTimer.GetElapsed_mcs() >= 1000000 / 60;
		if (IsDue)
		{
			PlatformTimer.Start();
		}

		const ImGuiViewport* Game = GetGameViewport();
		ImGuiPlatformIO& PlatformIO = ImGui::GetPlatformIO();
		const auto IsDrawn = [IsDue, Game](const ImGuiViewport* Viewport)
		{
			return !(Viewport->Flags & ImGuiViewportFlags_IsMinimized) && (IsDue || Viewport == Game);
		};

		for (int i = 1; i < PlatformIO.Viewports.Size; i++)
		{
			ImGuiViewport* Viewport = PlatformIO.Viewports[i];
			if (IsDrawn(Viewport))
			{
				if (PlatformIO.Platform_RenderWindow)
				{
					PlatformIO.Platform_RenderWindow(Viewport, nullptr);
				}

				if (PlatformIO.Renderer_RenderWindow)
				{
					PlatformIO.Renderer_RenderWindow(Viewport, nullptr);
				}
			}
		}

		for (int i = 1; i < PlatformIO.Viewports.Size; i++)
		{
			ImGuiViewport* Viewport = PlatformIO.Viewports[i];
			if (IsDrawn(Viewport))
			{
				if (PlatformIO.Platform_SwapBuffers)
				{
					PlatformIO.Platform_SwapBuffers(Viewport, nullptr);
				}

				if (PlatformIO.Renderer_SwapBuffers)
				{
					PlatformIO.Renderer_SwapBuffers(Viewport, nullptr);
				}
			}
		}
	}
#endif
}

void CImGuiManager::Render()
{
	Build();
	Draw();
}

void CImGuiManager::Build()
{
#ifdef IXR_WINDOWS
	// Before the frame, not when the display's scale changes: the font atlas may be in use then
	const float Scale = psImGuiScale * Device.Window.DisplayScale;
	if (AppliedScale != Scale)
	{
		BaseStyle = AppliedScale > 0.0f ? BaseStyle : ImGui::GetStyle();
		ImGuiStyle Scaled = BaseStyle;
		Scaled.ScaleAllSizes(Scale);
		Scaled.FontScaleMain = Scale;
		ImGui::GetStyle() = Scaled;
		AppliedScale = Scale;
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
			const ImGuiViewport* Game = GetGameViewport();
			for (auto& Window : ImGui::GetCurrentContext()->Windows)
			{
				if (Window->Viewport == Game)
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
}

void CImGuiManager::Draw()
{
#ifdef IXR_WINDOWS
	Device.m_pRender->DrawImGui(ImGui::GetDrawData());
#endif
}

void CImGuiManager::UpdateCapture()
{
	HasPlatformWindowFocus = SDL_GetKeyboardFocus() != GetGameWindow() && IsFocused();

	static bool keyProcessed = false;

    if (ImGui::IsKeyDown(ImGuiMod_Alt))
    {
        if (!keyProcessed)
        {
            if (ImGui::IsKeyDown(ImGuiMod_Ctrl))
            {
                DrawUIRender = !DrawUIRender;

                // The picture goes back to the main window, whichever window had the focus
                if (!DrawUIRender)
                {
                    CaptureInputs = false;
                    SDL_RaiseWindow(g_AppInfo.Window);
                }
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
int psImGuiDockWindow = 0;
int psImGuiGameWindow = 0;
int psGameViewMode = 0;
float psGameViewAspect = 16.0f / 9.0f;
int psGameViewWidth = 1280;
int psGameViewHeight = 720;

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

bool CImGuiManager::IsGameViewWindow() const
{
#ifdef DEBUG_DRAW
	return DrawUIRender && psImGuiGameWindow != 0 && !psDeviceFlags.test(rsClearBB);
#else
	return false;
#endif
}

ImGuiViewport* CImGuiManager::GetGameViewport() const
{
	ImGuiViewport* Viewport = ImGui::FindViewportByID(GameViewportId);
	return Viewport ? Viewport : ImGui::GetMainViewport();
}

SDL_Window* CImGuiManager::GetGameWindow() const
{
	SDL_Window* Window = GameWindowId ? SDL_GetWindowFromID(GameWindowId) : nullptr;
	return Window ? Window : g_AppInfo.Window;
}

bool CImGuiManager::FitGameView(u32 MaxWidth, u32 MaxHeight, u32& Width, u32& Height) const
{
	if (!IsGameViewWindow() || GameViewRoom.x < 1.0f || GameViewRoom.y < 1.0f)
	{
		return false;
	}

	float Wide = GameViewRoom.x;
	float High = GameViewRoom.y;

	if (psGameViewMode == 1)
	{
		const float Aspect = std::max(psGameViewAspect, 0.1f);
		Wide = std::min(Wide, High * Aspect);
		High = Wide / Aspect;
	}
	else if (psGameViewMode == 2)
	{
		Wide = float(psGameViewWidth);
		High = float(psGameViewHeight);
	}

	// Even sizes: the half-size targets of the frame take whole pixels of them
	Width = std::min(MaxWidth, std::max(64u, u32(Wide) & ~1u));
	Height = std::min(MaxHeight, std::max(64u, u32(High) & ~1u));
	return true;
}

ImVec2 CImGuiManager::GetGameViewMouse() const
{
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	return ImVec2((Mouse.x - GameViewPos.x) / std::max(GameViewSize.x, 1.0f), (Mouse.y - GameViewPos.y) / std::max(GameViewSize.y, 1.0f));
}

bool CImGuiManager::IsGameViewHovered() const
{
	const ImGuiContext* Context = ImGui::GetCurrentContext();
	const ImVec2 At = Context ? GetGameViewMouse() : ImVec2(-1.0f, -1.0f);

	if (At.x < 0.0f || At.x >= 1.0f || At.y < 0.0f || At.y >= 1.0f)
	{
		return false;
	}

	return !Context->HoveredWindow || (IsGameViewWindow() && Context->HoveredWindow == ImGui::FindWindowByName("Game"));
}

bool CImGuiManager::ReadGameViewColor(float (&Color)[3]) const
{
#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
	// The gamma pass leaves the frame in the output target's corner, the size of the view
	IRHIDevice* Rhi = GRHI ? GRHI->DevicePtr : nullptr;
	const ImVec2 At = GetGameViewMouse();
	const u64 Capacity = (u64(Device.OutputWidth) + 64) * (u64(Device.OutputHeight) + 64) * 4;

	if (!Rhi || !Rhi->RenderRTV || Capacity > u32(-1) || At.x < 0.0f || At.x >= 1.0f || At.y < 0.0f || At.y >= 1.0f)
	{
		return false;
	}

	xr_vector<u8> Pixels(static_cast<size_t>(Capacity));
	u32 Width = 0, Height = 0, Pitch = 0;

	if (!Rhi->ReadRenderTargetPixels(Rhi->RenderRTV, Pixels.data(), u32(Pixels.size()), Width, Height, Pitch) || !Width || !Height)
	{
		return false;
	}

	const u32 X = std::min(u32(At.x * float(Device.TargetWidth)), std::min(Device.TargetWidth, Width) - 1);
	const u32 Y = std::min(u32(At.y * float(Device.TargetHeight)), std::min(Device.TargetHeight, Height) - 1);
	const u8* Pixel = Pixels.data() + size_t(Y) * Pitch + size_t(X) * 4;
	Color[0] = Pixel[2] / 255.0f;
	Color[1] = Pixel[1] / 255.0f;
	Color[2] = Pixel[0] / 255.0f;
	return true;
#else
	Color[0] = Color[1] = Color[2] = 0.0f;
	return false;
#endif
}

CImGuiManager& CImGuiManager::Instance()
{
	static CImGuiManager Object;
	return Object;
}
