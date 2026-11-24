#include "stdafx.h"

#include "imgui_impl_sdl3.h"
#include "imgui_internal.h"
#include "IGame_Persistent.h"
#include "Autotest.h"

static ERHI_API_LAYER CurrentAPILevel = ERHI_API_LAYER::D3D11;

extern ENGINE_API bool g_appLoaded;
void DrawProfilerOverlay();

// The frame the gamma pass leaves for the tools, in the output target's corner
static void* GamePicture()
{
	IRHIRenderTargetView* Picture = GRHI->DevicePtr->RenderRTV;
	return RHIUtils::ImGui::Image(Picture ? Picture->GetSurface() : nullptr);
}

// The game's picture as a window among the tools: moved, sized and docked as any of them, the picture fitted
// into it with its own proportions. Ctrl+Alt hides the tools and this window with them; the picture then fills
// the main window, as it does without imgui_game_window
static void DrawGameWindow()
{
	CImGuiManager& Manager = CImGuiManager::Instance();
	const ImGuiViewport* Viewport = ImGui::GetMainViewport();

	// Nothing else covers the back buffer now
	ImGui::GetBackgroundDrawList(ImGui::GetMainViewport())->AddRectFilled(Viewport->Pos, ImVec2(Viewport->Pos.x + Viewport->Size.x, Viewport->Pos.y + Viewport->Size.y), IM_COL32(24, 24, 24, 255));

	ImGui::SetNextWindowPos(Viewport->WorkPos, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSize(Viewport->WorkSize, ImGuiCond_FirstUseEver);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));

	// Moved, docked and taken out into an OS window of its own by its title bar or tab, as the tools are. The
	// hidden autotest keeps it in the main window, which its window shots take
	if (Autotest::Active())
	{
		ImGui::SetNextWindowViewport(Viewport->ID);
	}

	const bool IsShown = ImGui::Begin("Game", nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoFocusOnAppearing);
	const ImGuiViewport* Host = ImGui::GetWindowViewport();
	Manager.GameViewportId = Host->ID;
	Manager.GameWindowId = Host != Viewport ? u32(size_t(Host->PlatformHandle)) : 0;

	if (IsShown)
	{
		const ImVec2 Room = ImGui::GetContentRegionAvail();
		Manager.GameViewRoom = Room;

		// A set size is shown scaled into the window; the other modes draw the frame at the window's own size,
		// pixel for pixel, and only a window that has just shrunk scales it down for a frame
		const float Fitted = std::min(Room.x / (float)Device.TargetWidth, Room.y / (float)Device.TargetHeight);
		const float Scale = psGameViewMode == 2 ? Fitted : std::min(Fitted, 1.0f);
		const ImVec2 Size((float)Device.TargetWidth * Scale, (float)Device.TargetHeight * Scale);
		const ImVec2 Corner = ImGui::GetCursorScreenPos();
		// Whole pixels: the linear filter of the image then returns the frame's own pixels
		const ImVec2 At(std::floor(Corner.x + (Room.x - Size.x) * 0.5f), std::floor(Corner.y + (Room.y - Size.y) * 0.5f));

		if (Size.x >= 1.0f && Size.y >= 1.0f)
		{
			ImGui::SetCursorScreenPos(At);
			ImGui::Image(GamePicture(), Size, ImVec2(0.0f, 0.0f), ImVec2((float)Device.TargetWidth / (float)Device.OutputWidth, (float)Device.TargetHeight / (float)Device.OutputHeight));
			// Hovered as an item, never active: a drag over the picture is the game's or a tool's, not a move of the window
			ImGui::ItemHoverable(ImRect(At.x, At.y, At.x + Size.x, At.y + Size.y), ImGui::GetID("Picture"), ImGuiItemFlags_None);
			Manager.GameViewPos = At;
			Manager.GameViewSize = Size;
		}

		if (ImGui::BeginPopupContextWindow("GameView"))
		{
			static const struct
			{
				const char* Name;
				float Aspect;
			} Shapes[] = { { "16:9", 16.0f / 9.0f }, { "16:10", 16.0f / 10.0f }, { "4:3", 4.0f / 3.0f }, { "21:9", 21.0f / 9.0f }, { "1:1", 1.0f } };

			static const int Sizes[][2] = { { 1280, 720 }, { 1600, 900 }, { 1920, 1080 }, { 2560, 1440 }, { 3840, 2160 } };

			if (ImGui::MenuItem("Free", nullptr, psGameViewMode == 0))
			{
				psGameViewMode = 0;
			}

			ImGui::Separator();

			for (const auto& Shape : Shapes)
			{
				if (ImGui::MenuItem(Shape.Name, nullptr, psGameViewMode == 1 && std::abs(psGameViewAspect - Shape.Aspect) < 0.01f))
				{
					psGameViewMode = 1;
					psGameViewAspect = Shape.Aspect;
				}
			}

			ImGui::Separator();

			for (const auto& Item : Sizes)
			{
				string64 Name;
				xr_sprintf(Name, "%d x %d", Item[0], Item[1]);

				// A size larger than the targets has nothing to draw into
				if (ImGui::MenuItem(Name, nullptr, psGameViewMode == 2 && psGameViewWidth == Item[0] && psGameViewHeight == Item[1], u32(Item[0]) <= Device.OutputWidth && u32(Item[1]) <= Device.OutputHeight))
				{
					psGameViewMode = 2;
					psGameViewWidth = Item[0];
					psGameViewHeight = Item[1];
				}
			}

			ImGui::EndPopup();
		}
	}
	else
	{
		Manager.GameViewRoom = ImVec2(0.0f, 0.0f);
	}

	ImGui::End();
	ImGui::PopStyleVar();
}

void DrawMainViewport()
{
	CImGuiManager& Manager = CImGuiManager::Instance();

	if (Manager.IsGameViewWindow() && g_appLoaded)
	{
		DrawGameWindow();
		return;
	}

	// The view where the gamma pass of the builds without the tools puts it: fitted into the window, the rest black
	ImGuiViewport* Viewport = ImGui::GetMainViewport();
	const Irect& Shown = Device.PresentRect;
	const ImVec2 At(Viewport->Pos.x + (float)Shown.x1, Viewport->Pos.y + (float)Shown.y1);
	const ImVec2 Size((float)Shown.width(), (float)Shown.height());

	Manager.GameViewRoom = ImVec2(0.0f, 0.0f);
	Manager.GameViewPos = At;
	Manager.GameViewSize = Size;
	Manager.GameViewportId = Viewport->ID;
	Manager.GameWindowId = 0;

	if (Shown.x1 > 0 || Shown.y1 > 0 || Size.x < Viewport->Size.x || Size.y < Viewport->Size.y)
	{
		ImGui::GetBackgroundDrawList(Viewport)->AddRectFilled(Viewport->Pos, ImVec2(Viewport->Pos.x + Viewport->Size.x, Viewport->Pos.y + Viewport->Size.y), IM_COL32(0, 0, 0, 255));
	}

	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));
	ImGui::SetNextWindowViewport(Viewport->ID);

	if (g_appLoaded)
	{
		ImGui::SetNextWindowBgAlpha(0.f);
	}

	ImGui::SetNextWindowPos(At);
	ImGui::SetNextWindowSize(Size);
	if (ImGui::Begin("Main", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoFocusOnAppearing))
	{
		ImGui::BringWindowToDisplayBack(ImGui::GetCurrentWindow());
		ImGui::SetCursorPos(ImVec2(0, 0));
		ImGui::GetWindowDrawList()->AddRect(At, ImVec2(At.x + Size.x, At.y + Size.y), 0xFFFFFFFF);
		ImGui::SetCursorPos(ImVec2(0, 0));
		ImGui::Image(GamePicture(), Size, ImVec2(0.0f, 0.0f), ImVec2((float)Device.TargetWidth / (float)Device.OutputWidth, (float)Device.TargetHeight / (float)Device.OutputHeight));
	}
	ImGui::End();

	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
}

namespace
{
	xrCriticalSection VideoModeListLock;
	xr_token EmptyVideoModeList[] = { { nullptr, -1 } };
}

void free_vid_mode_list()
{
	xrCriticalSectionGuard Guard(VideoModeListLock);
	if (vid_mode_token == nullptr)
	{
		return;
	}

	for (int i = 0; vid_mode_token[i].name; i++)
	{
		xr_free(vid_mode_token[i].name);
	}

	xr_free(vid_mode_token);
	vid_mode_token = nullptr;
}

xr_token* GetVideoModeList()
{
	xrCriticalSectionGuard Guard(VideoModeListLock);
	if (vid_mode_token != nullptr)
	{
		return vid_mode_token;
	}

	// Display enumeration is restricted to SDL's main thread. A later request can retry.
	if (GRHI == nullptr || !SDL_IsMainThread())
	{
		return EmptyVideoModeList;
	}

	xr_vector<shared_str> _tmp = GRHI->DisplaySizeArray();
	u32 _cnt = (u32)_tmp.size() + 1;

	vid_mode_token = xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt - 1].id = -1;
	vid_mode_token[_cnt - 1].name = nullptr;

	for (u32 i = 0; i < _tmp.size(); ++i)
	{
		vid_mode_token[i].id = i;
		vid_mode_token[i].name = xr_strdup(*_tmp[i]);
	}

	return vid_mode_token;
}

bool CRenderDevice::InitRenderDeviceEditor()
{
	GRHI = new CRHI;
	GetVideoModeList();
	GRHI->SwapChain = { psCurrentVidMode[0], psCurrentVidMode[1], u32(psFrameQueue), psDeviceFlags.test(rsVSync) };

	if (!GRHI->CreateDevice((ERHI_API_LAYER)g_graphicsAPI))
	{
		return false;
	}

	Device.TargetWidth = OutputWidth = psCurrentVidMode[0];
	Device.TargetHeight = OutputHeight = psCurrentVidMode[1];
	PresentRect.set(0, 0, int(OutputWidth), int(OutputHeight));
	g_UiView.set(int(psCurrentVidMode[0]), int(psCurrentVidMode[1]));
	CurrentAPILevel = GRHI->APILevel;

	return true;
}

bool CRenderDevice::InitRenderDevice(ERHI_API_LAYER API)
{
	PROF_EVENT("InitRenderDevice");
#ifdef IXR_WINDOWS
	CImGuiManager& ImManager = CImGuiManager::Instance();

	ImManager.PlatformNewFrameCallback = ImGui_ImplSDL3_NewFrame;
	ImManager.PlatformDestroyCallback = ImGui_ImplSDL3_Shutdown;
	ImManager.PlatformInitCallback = []() { ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window); };

	ImManager.InitPlatform();

	ImManager.ApplyMainViewport(DrawMainViewport);
	ImManager.Subscribe("Dockspace", CImGuiManager::ERenderPriority::eHight, []()
	{
		auto& States = Engine.External.EditorStates;

		if (ImGui::BeginMainMenuBar()) 
		{
			if (ImGui::BeginMenu("File"))
			 {
				if (ImGui::MenuItem("Exit", "")) 
				{
					g_pEventManager->Event.Defer("KERNEL:disconnect");
					g_pEventManager->Event.Defer("KERNEL:quit");
				}

				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("View")) {
				ImGui::MenuItem("Actor InfoPortions", nullptr, &States[static_cast<u8>(EditorUI::ActorInfos)]);
				ImGui::MenuItem("Scenes Viewer", nullptr, &States[static_cast<u8>(EditorUI::ScenesViewer)]);

				// TODO: Необходима доработка, лог выводится некорректно
				//ImGui::MenuItem("Console", nullptr, &States[static_cast<u8>(EditorUI::CmdConsole)]);
				
				ImGui::MenuItem("Console variables", nullptr, &States[static_cast<u8>(EditorUI::CmdVars)]);
				bool IsDockWindow = psImGuiDockWindow != 0;
				ImGui::Separator();
				bool IsGameWindow = psImGuiGameWindow != 0;
				ImGui::MenuItem("Game window", nullptr, &IsGameWindow);
				ImGui::SetItemTooltip("The game's picture as a window to move, size and dock; Ctrl+Alt brings it back to the whole screen");
				psImGuiGameWindow = IsGameWindow ? 1 : 0;
				ImGui::MenuItem("Dock window", nullptr, &IsDockWindow);
				ImGui::SetItemTooltip("An empty window to drag onto another screen and dock panels into");
				psImGuiDockWindow = IsDockWindow ? 1 : 0;
				
				// TODO: Необходима доработка
				// ImGui::MenuItem("Effectors", nullptr, &States[static_cast<u8>(EditorUI::CameraEffectors)]);

				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Game"))
			{
				ImGui::MenuItem("Level Inspector", nullptr, &States[static_cast<u8>(EditorUI::LevelInspector)]);
				ImGui::MenuItem("Spawn Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SpawnManager)]);
				ImGui::MenuItem("DOF setup", nullptr, &States[static_cast<u8>(EditorUI::Game_DofSetup)]);

				if (ImGui::BeginMenu("World##InGame"))
				{
					ImGui::MenuItem("Search Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SearchManager)]);
					ImGui::MenuItem("Graph Editor", nullptr, &States[static_cast<u8>(EditorUI::Game_GraphEditor)]);

					ImGui::EndMenu();
				}

				if (ImGui::BeginMenu("Weapon & HUD##InGame"))
				{
					ImGui::MenuItem("Weapon Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_WeaponManager)]);
					ImGui::MenuItem("Hud Adjust", nullptr, &States[static_cast<u8>(EditorUI::Game_HudAdjustManager)]);
					ImGui::MenuItem("Hud Adjust (Legacy)", nullptr, &States[static_cast<u8>(EditorUI::HudAdjust)]);
					if (pSettings->section_exist("camera_rig") && pSettings->read_if_exists<bool>("camera_rig", "enabled", false))
					{
						ImGui::MenuItem("Camera Rig", nullptr, &States[static_cast<u8>(EditorUI::Game_CameraRig)]);
					}
					ImGui::MenuItem("OMF", nullptr, &States[static_cast<u8>(EditorUI::Tools_OMFEditor)]);
					ImGui::MenuItem("Watch Debug", nullptr, &States[static_cast<u8>(EditorUI::Game_WatchDebug)]);

					ImGui::EndMenu();
				}

				if (ImGui::BeginMenu("Actor##InGame"))
				{
					ImGui::MenuItem("Actor Animations", nullptr, &States[static_cast<u8>(EditorUI::Game_ActorAnimations)]);
					ImGui::MenuItem("3rd Adjust", nullptr, &States[static_cast<u8>(EditorUI::Game_3rdAdjust)]);

					ImGui::EndMenu();
				}

				if (ImGui::BeginMenu("UI##InGame"))
				{
					ImGui::MenuItem("3D Icons Adjust", nullptr, &States[static_cast<u8>(EditorUI::Game_3DIconAdjust)]);

					ImGui::EndMenu();
				}

				ImGui::MenuItem("Demo record", nullptr, &States[static_cast<u8>(EditorUI::Game_DemoRecord)]);
				ImGui::MenuItem("Cinema Film Maker Editor", nullptr, &States[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)]);
				ImGui::MenuItem("Cutscene Adjust", nullptr, &States[static_cast<u8>(EditorUI::CutsceneAdjust)]);
				if (ImGui::BeginMenu("Editors##InGame"))
				{
					ImGui::MenuItem("Cinema Editor", nullptr, &States[static_cast<u8>(EditorUI::Game_CinemaEditor)]);
					ImGui::MenuItem("Weather Editor", nullptr, &States[static_cast<u8>(EditorUI::Weather)]);
					ImGui::MenuItem("Detail Layers Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_DetailLayersEditor)]);
					ImGui::MenuItem("AI Grid Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_AIGridEditor)]);
					ImGui::MenuItem("Car Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_CarEditor)]);
					ImGui::MenuItem("PPE Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)]);
					ImGui::SetItemTooltip("Post-Process Effector");

					ImGui::MenuItem("Texture Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_TextureEditor)]);
					ImGui::MenuItem("Quest Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_QuestEditor)]);
					ImGui::MenuItem("Input", nullptr, &States[static_cast<u8>(EditorUI::Tools_InputManager)]);

					ImGui::EndMenu();
				}

				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Debug"))
			{
				ImGui::MenuItem("Audio Editor", nullptr, &States[static_cast<u8>(EditorUI::Audio_General)]);
				ImGui::MenuItem("UI Debug", nullptr, &States[static_cast<u8>(EditorUI::UI_General)]);
				ImGui::MenuItem("Shader Debug", nullptr, &States[static_cast<u8>(EditorUI::Shaders)]);
				ImGui::MenuItem("Render Debug", nullptr, &States[static_cast<u8>(EditorUI::DebugDraw)]);
				ImGui::MenuItem("ECS Viewer", nullptr, &States[static_cast<u8>(EditorUI::ECSViewer)]);
				ImGui::MenuItem("SVG Storage Viewer Debug", nullptr, &States[static_cast<u8>(EditorUI::Tools_RenderDebug_SVGStorageViewer)]);
				ImGui::MenuItem("Profiler", nullptr, &States[static_cast<u8>(EditorUI::Profiler)]);
			#if defined(IXRAY_PROFILER)
				if (ImGui::MenuItem("Optick Start Capture"))
				{
					PROF_START_CAPTURE();
				}

				if (ImGui::MenuItem("Optick Stop Capture"))
				{
					PROF_STOP_CAPTURE();

					string256 currentDate{};
					string256 currentTime{};

					Time time{};
					xr_strconcat(currentDate, time.GetYearString().c_str(), ".", time.GetMonthString().c_str(), ".", time.GetDayString().c_str());
					xr_strconcat(currentTime, time.GetHoursString().c_str(), ".", time.GetMinutesString().c_str(), ".", time.GetSecondsString().c_str());
					string_path optickFileName{};
					xr_strconcat(optickFileName, "ixray-optick-", currentDate, "-", currentTime, "-", Core.UserName, ".opt");

					PROF_SAVE_CAPTURE(optickFileName);
				}
			#endif
				
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Scripting"))
			{
				ImGui::MenuItem("Lua: Run code", nullptr, &States[static_cast<u8>(EditorUI::LuaCodespace)]);
				ImGui::MenuItem("Lua: Attach to VSCode", nullptr, &States[static_cast<u8>(EditorUI::LuaDebug)]);


				ImGui::EndMenu();
			}

			ImGui::MenuItem("Click `Alt`");
			static float ShownScale = 0.0f;
			ShownScale = ImGui::IsAnyItemActive() && ShownScale > 0.0f ? ShownScale : psImGuiScale;
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.0f);
			ImGui::SliderFloat("UI scale", &ShownScale, 0.5f, 3.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			if (ImGui::IsItemDeactivatedAfterEdit())
			{
				psImGuiScale = ShownScale;
			}
			ImGui::SetItemTooltip("Size of every ImGui window and font; applied when the slider is released (console: imgui_scale)");
			ImGui::EndMainMenuBar();
		}

		const ImGuiViewport* Viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(Viewport->WorkPos);
		ImGui::SetNextWindowSize(Viewport->WorkSize);
		ImGui::SetNextWindowViewport(Viewport->ID);
		ImGui::SetNextWindowBgAlpha(0);

		constexpr ImGuiWindowFlags dockspace_window_flags = 0
			| ImGuiWindowFlags_NoTitleBar
			| ImGuiWindowFlags_NoCollapse
			| ImGuiWindowFlags_NoResize
			| ImGuiWindowFlags_NoMove
			| ImGuiWindowFlags_NoDocking
			| ImGuiWindowFlags_NoNavFocus;
		ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));
		if (ImGui::Begin("DockSpaceViewport_Main", nullptr, dockspace_window_flags))
		{
			ImGui::DockSpace(ImGui::GetID("DockSpace"), ImVec2(0.0f, 0.0f), ImGuiDockNodeFlags_PassthruCentralNode);
		}
		ImGui::End();
		ImGui::PopStyleVar();
		ImGui::PopStyleVar();
		ImGui::PopStyleVar();

		if (psImGuiDockWindow)
		{
			static ImVec2 TitleMin, TitleMax;
			static bool IsTitleHeld = false;
			IsTitleHeld = ImGui::IsMouseDown(ImGuiMouseButton_Left) && (IsTitleHeld || (ImGui::IsMouseClicked(ImGuiMouseButton_Left) && ImGui::IsMouseHoveringRect(TitleMin, TitleMax, false)));
			bool IsDockWindow = true;
			ImGui::SetNextWindowSize(ImVec2(1280.0f, 720.0f), ImGuiCond_FirstUseEver);
			if (ImGui::Begin("Dock window", &IsDockWindow, ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoCollapse | (IsTitleHeld ? 0 : ImGuiWindowFlags_NoMove)))
			{
				ImGui::DockSpace(ImGui::GetID("DockWindowSpace"));
			}
			TitleMin = ImGui::GetWindowPos();
			TitleMax = ImVec2(TitleMin.x + ImGui::GetWindowSize().x, TitleMin.y + ImGui::GetFrameHeight());
			ImGui::End();
			psImGuiDockWindow = IsDockWindow ? 1 : 0;
		}
	});
#ifdef DEBUG_DRAW
	ImManager.Subscribe("Profiler", CImGuiManager::ERenderPriority::eLow, DrawProfilerOverlay);
#endif
#endif
	// The swap chain takes the window's size; the targets the video mode
	ReadWindowState();
	Device.TargetWidth = OutputWidth = psCurrentVidMode[0];
	Device.TargetHeight = OutputHeight = psCurrentVidMode[1];
	GRHI->SwapChain = WantedSwapChain();
	GRHI->CreateDevice(API);

	PresentRect.set(0, 0, int(OutputWidth), int(OutputHeight));
	g_UiView.set(int(psCurrentVidMode[0]), int(psCurrentVidMode[1]));
	CurrentAPILevel = GRHI->APILevel;
	return true;
}

void CRenderDevice::DestroyRenderDevice()
{
	CImGuiManager::Instance().Destroy();
	xr_delete(GRHI);

	free_vid_mode_list();
}

void* CRenderDevice::GetRenderDevice()
{
	if (GRHI == nullptr)
	{
		return nullptr;
	}

	return GRHI->DevicePtr->RawDevice;
}

void* CRenderDevice::GetSwapchain()
{
	return GRHI->GetSwapchain();
}

u32	CRenderDevice::GetSwapchainWidth()
{
	return TargetWidth;
}

u32	CRenderDevice::GetSwapchainHeight()
{
	return TargetHeight;
}

u32 CRenderDevice::GetTimeDeltaSafe(u32 starttime)
{
	u32 curtime = dwTimeGlobal;
	u32 result = curtime - starttime;

	if (result > curtime)
		result = u32(-1) - starttime + curtime;

	return result;
}

u32 CRenderDevice::GetTimeDeltaSafe(u32 starttime, u32 endtime)
{
	u32 result = endtime - starttime;

	if (result > endtime)
		result = u32(-1) - starttime + endtime;

	return result;
}

void CRenderDevice::ResizeBuffers(u32 Width, u32 Height)
{
	// The editors draw their viewport at its own size: the targets follow it
	if (IsEditorMode())
	{
		psCurrentVidMode[0] = Width;
		psCurrentVidMode[1] = Height;
	}

	GRHI->ResizeBuffers({ Width, Height, u32(psFrameQueue), psDeviceFlags.test(rsVSync) });

	Device.TargetWidth = OutputWidth = psCurrentVidMode[0];
	Device.TargetHeight = OutputHeight = psCurrentVidMode[1];
	PresentRect.set(0, 0, int(OutputWidth), int(OutputHeight));
}

void CRenderDevice::ResizeWindow(u32 width, u32 height)
{
	if (Autotest::Active())
		psDeviceFlags.set(rsFullscreen, FALSE);

	// Borderless at the desktop size of the window's display; the display mode is never changed
	if (psDeviceFlags.is(rsFullscreen))
	{
		SDL_SetWindowFullscreenMode(g_AppInfo.Window, nullptr);
		SDL_SetWindowFullscreen(g_AppInfo.Window, true);
	}
	else 
	{
		SDL_SetWindowFullscreen(g_AppInfo.Window, 0);
		SDL_SetWindowBordered(g_AppInfo.Window, true);
		SDL_SetWindowResizable(g_AppInfo.Window, true);

		SDL_SetWindowMinimumSize(g_AppInfo.Window, 640, 480);
		ReadWindowState();

		if (Window.PixelWidth != s32(width) || Window.PixelHeight != s32(height))
		{
			const float Density = std::max(SDL_GetWindowPixelDensity(g_AppInfo.Window), 0.01f);
			SDL_SetWindowSize(g_AppInfo.Window, int(float(width) / Density + 0.5f), int(float(height) / Density + 0.5f));

			const bool bCentered = !Core.ParamsData.test(ECoreParams::no_center_screen);
			if (bCentered) {
				SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
			}
		}
	}

	if (!g_dedicated_server && !Autotest::Active())
	{
		SDL_ShowWindow(g_AppInfo.Window);
		SDL_RaiseWindow(g_AppInfo.Window);
	}

	// The swap chain is made for the size the window has now, not for the one its events will report
	SDL_SyncWindow(g_AppInfo.Window);
	ReadWindowState();

	if (GRHI->DevicePtr != nullptr)
	{
		const RHISwapChainDesc Wanted = WantedSwapChain();
		ResizeBuffers(Wanted.Width, Wanted.Height);
	}
}

RENDERDOC_API_1_6_0* CRenderDevice::GetRenderDocAPI()
{
	return pRDocAPI;
}

void CRenderDevice::BeginRender()
{
	PROF_EVENT("CRenderDevice::BeginRender");

#ifdef IXR_WINDOWS
	CImGuiManager::Instance().NewPlatformFrame();
	CImGuiManager::Instance().UpdateCapture();
#endif
}

extern int main_menu_fps_limit, fps_limit;

void CRenderDevice::EndRender()
{
	PROF_EVENT("CRenderDevice::EndRender")
    
	using tick = long long;
	using clock = std::chrono::high_resolution_clock;
    
	bool main_menu_active = g_pGamePersistent						   &&
							g_pGamePersistent->m_pMainMenu			   &&
							g_pGamePersistent->m_pMainMenu->IsActive();

	const int target_fps = s_StartupActive ? 60 : (main_menu_active ? main_menu_fps_limit : fps_limit);
	if (target_fps > 0)
	{
		static tick prev_ticks = clock::now().time_since_epoch().count();
		tick curr_ticks = clock::now().time_since_epoch().count();
        
		tick current_ticks = curr_ticks - prev_ticks;
		tick target_ticks = 1'000'000'000 / target_fps;
		
		if (current_ticks < target_ticks)
		{
			// The system wakes a sleeper late by up to a millisecond or so: the last stretch is spun
			constexpr tick Spin = 1'500'000;
			const tick Deadline = prev_ticks + target_ticks;
			PROF_EVENT("FPS limiter")

			if (Deadline - curr_ticks > Spin)
			{
				SDL_DelayNS(u64(Deadline - curr_ticks - Spin));
			}

			while (clock::now().time_since_epoch().count() < Deadline)
			{
			}
		}
		prev_ticks = clock::now().time_since_epoch().count();
	}
}
