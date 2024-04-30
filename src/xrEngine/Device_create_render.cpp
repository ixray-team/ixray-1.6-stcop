#include "stdafx.h"
#include "../xrCore/_std_extensions.h"
#include "imgui_impl_sdl3.h"
#include "IGame_Persistent.h"

#include "IRender_RHI.h"

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
		ImGui::Image(g_RenderRHI->GetRenderSRV(), ImVec2((float)Device.TargetWidth, (float)Device.TargetHeight));
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

bool CRenderDevice::InitRenderDevice(IRender_RHI::APILevel API)
{
	g_RenderRHI->FillModes();
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

		if (ImGui::BeginMainMenuBar()) {
			if (ImGui::BeginMenu("File")) {
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
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("Tools")) 
			{
				ImGui::MenuItem("Lua: Run code", nullptr, &States[static_cast<u8>(EditorUI::LuaCodespace)]);
				ImGui::MenuItem("Lua: Attach to VSCode", nullptr, &States[static_cast<u8>(EditorUI::LuaDebug)]);
				ImGui::MenuItem("Shader Debug", nullptr, &States[static_cast<u8>(EditorUI::Shaders)]);
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

	g_RenderRHI->Create(API);

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];

	return true;
}

void CRenderDevice::DestroyRenderDevice()
{
#ifndef _EDITOR
	CImGuiManager::Instance().Destroy();
#endif

	g_RenderRHI->Destroy();
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
	g_RenderRHI->ResizeBuffers(Width, Height);

	Device.TargetWidth = Width;
	Device.TargetHeight = Height;
}

void CRenderDevice::ResizeWindow(u32 width, u32 height)
{   
	Width = width;
	Height = height;

	if (!psDeviceFlags.is(rsFullscreen))
		SDL_SetWindowFullscreen(g_AppInfo.Window, 0);
	else
		SDL_SetWindowFullscreen(g_AppInfo.Window, SDL_WINDOW_FULLSCREEN);

	SDL_SyncWindow(g_AppInfo.Window);

	// Get the index of the primary display
	SDL_DisplayID displayIndex = SDL_GetDisplayForWindow(g_AppInfo.Window);
	if (displayIndex < 0) {
		Msg("! Failed to get display index: %i", SDL_GetError());
		return;
	}

	// Get the bounds of the primary display
	SDL_Rect displayBounds;
	if (SDL_GetDisplayBounds(displayIndex, &displayBounds) != 0) {
		Msg("! Failed to get display bounds: %i", SDL_GetError());
		return;
	}

	// Calculate maximum width and height based on available display area
	int maxWidth = displayBounds.w;
	int maxHeight = displayBounds.h;

	// Ensure the new window size fits within the maximum width and height
	Width = std::min(Width, maxWidth);
	Height = std::min(Height, maxHeight);

	// Set the new window size
	SDL_SetWindowSize(g_AppInfo.Window, Width, Height);

	// Adjust the window position to keep it within the bounds of the primary display
	const bool bCentered = !Core.ParamsData.test(ECoreParams::no_center_screen);
	if (bCentered)
	{
		SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	}
	else
	{
		// Get the position of the window after resizing
		int windowX, windowY;
		SDL_GetWindowPosition(g_AppInfo.Window, &windowX, &windowY);

		windowX = std::max(windowX, displayBounds.x);
		windowY = std::max(windowY, displayBounds.y);
		windowX = std::min(windowX, displayBounds.x + displayBounds.w - Width);
		windowY = std::min(windowY, displayBounds.y + displayBounds.h - Height);
		SDL_SetWindowPosition(g_AppInfo.Window, windowX, windowY);
	}

	ResizeBuffers(Width, Height);

	if (!psDeviceFlags.is(rsFullscreen) && bCentered)
	{
		SDL_PollEvent(nullptr);
		SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	}
}

void CRenderDevice::BeginRender()
{
#ifndef _EDITOR
	CImGuiManager::Instance().NewPlatformFrame();
	CImGuiManager::Instance().UpdateCapture();
#endif
}

void CRenderDevice::EndRender()
{
}