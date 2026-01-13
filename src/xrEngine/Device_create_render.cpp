#include "stdafx.h"

#include "../xrCore/_std_extensions.h"
#include "imgui_impl_sdl3.h"
#include "IGame_Persistent.h"

static ERHI_API_LAYER CurrentAPILevel = ERHI_API_LAYER::D3D11;

extern ENGINE_API BOOL g_appLoaded;
void DrawMainViewport()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));

	const ImGuiViewport* Viewport = ImGui::GetMainViewport();
	ImGui::SetNextWindowViewport(Viewport->ID);

	if (g_appLoaded)
	{
		ImGui::SetNextWindowBgAlpha(0.f);
	}

	ImGui::SetNextWindowPos(Viewport->Pos);
	ImGui::SetNextWindowSize(ImVec2((float)Device.TargetWidth, (float)Device.TargetHeight));
	if (ImGui::Begin("Main", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs)) {
		ImGui::SetCursorPos(ImVec2(0, 0));
		ImGui::GetWindowDrawList()->AddRect(Viewport->Pos, ImVec2((float)Device.TargetWidth + Viewport->Pos.x, (float)Device.TargetHeight + Viewport->Pos.y), 0xFFFFFFFF);
		ImGui::SetCursorPos(ImVec2(0, 0));
		ImGui::Image(GRHI->DevicePtr->RenderSRV, ImVec2((float)Device.TargetWidth, (float)Device.TargetHeight));
	}
	ImGui::End();

	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
}

void free_vid_mode_list()
{
	for (int i = 0; vid_mode_token[i].name; i++)
	{
		xr_free(vid_mode_token[i].name);
	}

	xr_free(vid_mode_token);
	vid_mode_token = nullptr;
}

void fill_vid_mode_list()
{
	if (vid_mode_token != nullptr)
		return;

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
}

bool CRenderDevice::InitRenderDeviceEditor()
{
	GRHI = new CRHI;
	fill_vid_mode_list();

	if (!GRHI->CreateDevice(ERHI_API_LAYER::D3D9))
	{
		return false;
	}

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	CurrentAPILevel = ERHI_API_LAYER::D3D9;

	return true;
}

bool CRenderDevice::InitRenderDevice(ERHI_API_LAYER API)
{
	PROF_EVENT("InitRenderDevice");
	GRHI = new CRHI;

	fill_vid_mode_list();

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
				
				// TODO: Необходима доработка
				// ImGui::MenuItem("Effectors", nullptr, &States[static_cast<u8>(EditorUI::CameraEffectors)]);

				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Game"))
			{
				ImGui::MenuItem("Spawn Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SpawnManager)]);
				ImGui::MenuItem("Weapon Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_WeaponManager)]);
				ImGui::MenuItem("Search Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SearchManager)]);


				//ImGui::MenuItem("Time Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_TimeManager)]);
				ImGui::MenuItem("Hud Adjust", nullptr, &States[static_cast<u8>(EditorUI::Game_HudAdjustManager)]);
				ImGui::MenuItem("Hud Adjust (Legacy)", nullptr, &States[static_cast<u8>(EditorUI::HudAdjust)]);
				ImGui::MenuItem("LevelInspector", nullptr, &States[static_cast<u8>(EditorUI::LevelInspector)]);
				if (ImGui::BeginMenu("Editors##InGame"))
				{
				ImGui::MenuItem("Weather Editor", nullptr, &States[static_cast<u8>(EditorUI::Weather)]);
				ImGui::MenuItem("Car Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_CarEditor)]);
				ImGui::MenuItem("PPE Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_PostProcessEffectorEditor)]);
				ImGui::SetItemTooltip("Post-Process Effector");

				ImGui::MenuItem("Texture Editor", nullptr, &States[static_cast<u8>(EditorUI::Tools_TextureEditor)]);
					ImGui::MenuItem("OMF", nullptr, &States[static_cast<u8>(EditorUI::Tools_OMFEditor)]);
					ImGui::MenuItem("Input", nullptr, &States[static_cast<u8>(EditorUI::Tools_InputManager)]);

					ImGui::EndMenu();
				}

				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Debug"))
			{
				ImGui::MenuItem("UI Debug", nullptr, &States[static_cast<u8>(EditorUI::UI_General)]);
				ImGui::MenuItem("Shader Debug", nullptr, &States[static_cast<u8>(EditorUI::Shaders)]);
				ImGui::MenuItem("Render Debug", nullptr, &States[static_cast<u8>(EditorUI::DebugDraw)]);
				ImGui::MenuItem("SVG Storage Viewer Debug", nullptr, &States[static_cast<u8>(EditorUI::Tools_RenderDebug_SVGStorageViewer)]);
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
	});

	GRHI->CreateDevice(API);

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	CurrentAPILevel = API;
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
	GRHI->ResizeBuffers(Width, Height);

	Device.TargetWidth = Width;
	Device.TargetHeight = Height;
}

void CRenderDevice::ResizeWindow(u32 width, u32 height)
{
	if (psDeviceFlags.is(rsFullscreen)) 
	{
		SDL_DisplayMode displayMode;
		displayMode.w = psCurrentVidMode[0];
		displayMode.h = psCurrentVidMode[1];
		SDL_SetWindowFullscreenMode(g_AppInfo.Window, &displayMode);
		SDL_SetWindowFullscreen(g_AppInfo.Window, SDL_WINDOW_FULLSCREEN);
	}
	else 
	{
		SDL_SetWindowFullscreen(g_AppInfo.Window, 0);
		SDL_SetWindowSize(g_AppInfo.Window, width, height);

		const bool bCentered = !Core.ParamsData.test(ECoreParams::no_center_screen);
		if (bCentered) {
			SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		}
	}

	ResizeBuffers(width, height);
}

RENDERDOC_API_1_6_0* CRenderDevice::GetRenderDocAPI()
{
	return pRDocAPI;
}

void CRenderDevice::BeginRender()
{
	PROF_EVENT("CRenderDevice::BeginRender");

	CImGuiManager::Instance().NewPlatformFrame();
	CImGuiManager::Instance().UpdateCapture();
}

void CRenderDevice::EndRender()
{
}