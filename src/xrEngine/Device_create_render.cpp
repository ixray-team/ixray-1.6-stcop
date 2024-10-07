#include "stdafx.h"

#include "../xrCore/_std_extensions.h"
#include "imgui_impl_sdl3.h"
#include "IGame_Persistent.h"

#include <d3d11.h>


static ERHI_API CurrentAPILevel = ERHI_API::DX11;


extern ENGINE_API BOOL g_appLoaded;
void DrawMainViewport()
{
#ifndef _EDITOR
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
		ImGui::Image(g_RenderRHI->RenderSRV, ImVec2((float)Device.TargetWidth, (float)Device.TargetHeight));
	}
	ImGui::End();

	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
	ImGui::PopStyleVar();
#endif
}

void free_vid_mode_list()
{
#ifndef _EDITOR
	for (int i = 0; vid_mode_token[i].name; i++) {
		xr_free(vid_mode_token[i].name);
	}

	xr_free(vid_mode_token);
	vid_mode_token = nullptr;
#endif
}

RHI_API void fill_vid_mode_list();

bool CRenderDevice::InitRenderDeviceEditor()
{
	fill_vid_mode_list();

	if (!g_RenderRHI->Create())
	{
		return false;
	}

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	CurrentAPILevel = ERHI_API::DX11;

	return true;
}

bool CRenderDevice::InitRenderDevice(ERHI_API API)
{
	fill_vid_mode_list();
#ifndef _EDITOR
	CImGuiManager& ImManager = CImGuiManager::Instance();

	ImManager.PlatformNewFrameCallback = ImGui_ImplSDL3_NewFrame;
	ImManager.PlatformDestroyCallback = ImGui_ImplSDL3_Shutdown;
	ImManager.PlatformInitCallback = []() { ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window); };

	ImManager.InitPlatform();

	ImManager.ApplyMainViewport(DrawMainViewport);
	ImManager.Subscribe("Dockspace", CImGuiManager::ERenderPriority::eHight,[]() 
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

			if (ImGui::BeginMenu("Edit")) {
				ImGui::MenuItem("Console variables", nullptr, &States[static_cast<u8>(EditorUI::CmdVars)]);
				ImGui::MenuItem("Hud Adjust", nullptr, &States[static_cast<u8>(EditorUI::HudAdjust)]);
				ImGui::MenuItem("Weather Editor", nullptr, &States[static_cast<u8>(EditorUI::Weather)]);
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("View")) {
				ImGui::MenuItem("Debug Render", nullptr, &States[static_cast<u8>(EditorUI::DebugDraw)]);
				ImGui::MenuItem("Console variables", nullptr, &States[static_cast<u8>(EditorUI::CmdVars)]);
				ImGui::MenuItem("Actor InfoPortions", nullptr, &States[static_cast<u8>(EditorUI::ActorInfos)]);
				ImGui::MenuItem("Scenes Viewer", nullptr, &States[static_cast<u8>(EditorUI::ScenesViewer)]);
				ImGui::MenuItem("Console", nullptr, &States[static_cast<u8>(EditorUI::CmdConsole)]);
				ImGui::MenuItem("Effectors", nullptr, &States[static_cast<u8>(EditorUI::CameraEffectors)]);
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Game"))
			{
				ImGui::MenuItem("Time Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_TimeManager)]);
				ImGui::MenuItem("Spawn Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SpawnManager)]);
				ImGui::MenuItem("Weapon Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_WeaponManager)]);
				ImGui::MenuItem("Search Manager", nullptr, &States[static_cast<u8>(EditorUI::Game_SearchManager)]);
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Tools")) 
			{
				ImGui::MenuItem("Lua: Run code", nullptr, &States[static_cast<u8>(EditorUI::LuaCodespace)]);
				ImGui::MenuItem("Lua: Attach to VSCode", nullptr, &States[static_cast<u8>(EditorUI::LuaDebug)]);
				ImGui::MenuItem("Shader Debug", nullptr, &States[static_cast<u8>(EditorUI::Shaders)]);
				if (ImGui::MenuItem("Optick Start Capture"))
				{
					PROF_START_CAPTURE();
				}

				if (ImGui::MenuItem("Optick Stop Capture"))
				{
					PROF_STOP_CAPTURE();
					PROF_SAVE_CAPTURE("ixr.opt");
				}
				
				ImGui::EndMenu();
			}

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
#endif
	switch (API) 
	{
	case ERHI_API::DX11:
		if (!g_RenderRHI->Create())
		{
			return false;
		}
		break;

	default:
		break;
	}

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	CurrentAPILevel = API;
	return true;
}

void CRenderDevice::DestroyRenderDevice()
{
#ifndef _EDITOR
	CImGuiManager::Instance().Destroy();
#endif
	switch (CurrentAPILevel) 
	{
	case ERHI_API::DX11:
		g_RenderRHI->Destroy();
		break;

	default:
		break;
	}

	free_vid_mode_list();
}

u32	CRenderDevice::GetSwapchainWidth()
{
	return TargetWidth;
}

u32	CRenderDevice::GetSwapchainHeight()
{
	return TargetHeight;
}

void CRenderDevice::ResizeBuffers(u32 Width, u32 Height)
{
	switch (CurrentAPILevel)
	{
	case ERHI_API::DX11:
		g_RenderRHI->ResizeBuffers(Width, Height);
		break;

	default:
		break;
	}

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
		if (bCentered) 
		{
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
#ifndef _EDITOR
	CImGuiManager::Instance().NewPlatformFrame();
	CImGuiManager::Instance().UpdateCapture();
#endif
}

void CRenderDevice::EndRender()
{
}