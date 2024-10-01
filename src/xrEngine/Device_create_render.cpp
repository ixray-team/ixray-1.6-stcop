#include "stdafx.h"

#include "../xrCore/_std_extensions.h"
#include "imgui_impl_sdl3.h"
#include "IGame_Persistent.h"

#include <d3d11.h>
#include <d3d9.h>

#pragma comment(lib, "d3d9.lib")
#pragma comment(lib, "d3d11.lib")
#pragma comment(lib, "dxgi.lib")

static APILevel CurrentAPILevel = APILevel::DX11;

D3D_FEATURE_LEVEL FeatureLevel = D3D_FEATURE_LEVEL::D3D_FEATURE_LEVEL_11_0;
void* HWSwapchain = nullptr;

void* HWRenderDevice = nullptr;
void* HWRenderContext = nullptr;

void* RenderTexture = nullptr;
void* RenderSRV = nullptr;
void* RenderRTV = nullptr;

void* RenderDSV = nullptr;
void* SwapChainRTV = nullptr;

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
		ImGui::Image(RenderSRV, ImVec2((float)Device.TargetWidth, (float)Device.TargetHeight));
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

struct _uniq_mode
{
	_uniq_mode(LPCSTR v) :_val(v) {}
	LPCSTR _val;
	bool operator() (LPCSTR _other) { return !_stricmp(_val, _other); }
};

bool sort_vid_mode(DXGI_MODE_DESC& left, DXGI_MODE_DESC& right) {
	auto leftString = xr_string::ToString(left.Width) + xr_string::ToString(left.Height);
	auto rightString = xr_string::ToString(right.Width) + xr_string::ToString(right.Height);

	if (leftString.length() == rightString.length()) {
		if (left.Width > right.Width) {
			return true;
		}
		else if (left.Width == right.Width) {
			return left.Height > right.Height;
		}

		return false;
	}

	return leftString.length() > rightString.length();
}

void fill_vid_mode_list()
{
#ifndef _EDITOR
	if (vid_mode_token != nullptr)		return;
	xr_vector<LPCSTR>	_tmp;
	xr_vector<DXGI_MODE_DESC>	modes;

	IDXGIOutput* pOutput = nullptr;
	IDXGIAdapter* pAdapter = nullptr;
	IDXGIFactory* pFactory = nullptr;
	R_CHK(CreateDXGIFactory(IID_PPV_ARGS(&pFactory)));
	pFactory->EnumAdapters(0, &pAdapter);
	pAdapter->EnumOutputs(0, &pOutput);
	pAdapter->Release();
	pFactory->Release();
	VERIFY(pOutput);

	UINT num = 0;
	DXGI_FORMAT format = DXGI_FORMAT_R8G8B8A8_UNORM;
	UINT flags = 0;

	// Get the number of display modes available
	pOutput->GetDisplayModeList(format, flags, &num, 0);

	// Get the list of display modes
	modes.resize(num);
	pOutput->GetDisplayModeList(format, flags, &num, &modes.front());

	_RELEASE(pOutput);

	std::sort(modes.begin(), modes.end(), sort_vid_mode);

	for (u32 i = 0; i < num; ++i)
	{
		DXGI_MODE_DESC& desc = modes[i];
		string32 str;

		if (desc.Width < 800)
			continue;

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if (_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
			continue;

		_tmp.push_back(nullptr);
		_tmp.back() = xr_strdup(str);
	}

	u32 _cnt = (u32)_tmp.size() + 1;

	vid_mode_token = xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt - 1].id = -1;
	vid_mode_token[_cnt - 1].name = nullptr;

#ifdef DEBUG
	Msg("Available video modes[%d]:", _tmp.size());
#endif // DEBUG
	for (u32 i = 0; i < _tmp.size(); ++i)
	{
		vid_mode_token[i].id = i;
		vid_mode_token[i].name = _tmp[i];
#ifdef DEBUG
		Msg("[%s]", _tmp[i]);
#endif // DEBUG
	}
#endif
}

bool CreateD3D9();
bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);
void DestroyD3D9();

#ifndef _EDITOR
bool CreateD3D11();
bool UpdateBuffersD3D11();
void ResizeBuffersD3D11(u16 Width, u16 Height);
void DestroyD3D11();
#endif

bool CRenderDevice::InitRenderDeviceEditor()
{
	fill_vid_mode_list();

	if (!CreateD3D9())
	{
		return false;
	}

	Device.TargetWidth = psCurrentVidMode[0];
	Device.TargetHeight = psCurrentVidMode[1];
	CurrentAPILevel = APILevel::DX9;

	return true;
}

bool CRenderDevice::InitRenderDevice(APILevel API)
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
	switch (API) {

	case APILevel::DX9:
		if (!CreateD3D9()) {
			return false;
		}
		break;

#ifndef _EDITOR
	case APILevel::DX11:
		if (!CreateD3D11()) {
			return false;
		}
		break;

#endif
	default:
		break;
	}

	if (HWRenderDevice == nullptr) {
		return false;
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

	case APILevel::DX9:
		DestroyD3D9();
		break;

#ifndef _EDITOR
	case APILevel::DX11:
		DestroyD3D11();
		break;
#endif

	default:
		break;
	}

	free_vid_mode_list();
}

void* CRenderDevice::GetRenderDevice()
{
	return HWRenderDevice;
}

void* CRenderDevice::GetRenderContext()
{
	return HWRenderContext;
}

void* CRenderDevice::GetRenderTexture()
{
	// FX: Use ImGui render for Debug Draw mode
#ifdef DEBUG_DRAW
	return RenderRTV;
#else
	return SwapChainRTV;
#endif
}

void* CRenderDevice::GetDepthTexture()
{
	return RenderDSV;
}

void* CRenderDevice::GetSwapchainTexture()
{
	return SwapChainRTV;
}

void* CRenderDevice::GetSwapchain()
{
	return HWSwapchain;
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
	switch (CurrentAPILevel) {

	case APILevel::DX9:
		ResizeBuffersD3D9(Width, Height);
		break;
#ifndef _EDITOR
	case APILevel::DX11:
		ResizeBuffersD3D11(Width, Height);
		break;
#endif
	default:
		break;
	}

	Device.TargetWidth = Width;
	Device.TargetHeight = Height;
}

void CRenderDevice::ResizeWindow(u32 width, u32 height)
{
	if (psDeviceFlags.is(rsFullscreen)) {
		SDL_DisplayMode displayMode;
		displayMode.w = psCurrentVidMode[0];
		displayMode.h = psCurrentVidMode[1];
		SDL_SetWindowFullscreenMode(g_AppInfo.Window, &displayMode);
		SDL_SetWindowFullscreen(g_AppInfo.Window, SDL_WINDOW_FULLSCREEN);
	} else {
		SDL_SetWindowFullscreen(g_AppInfo.Window, 0);
		SDL_SetWindowSize(g_AppInfo.Window, width, height);

		const bool bCentered = !Core.ParamsData.test(ECoreParams::no_center_screen);
		if (bCentered) {
			SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		}
	}

	ResizeBuffers(width, height);
}

D3D_FEATURE_LEVEL CRenderDevice::GetFeatureLevel()
{
	return FeatureLevel;
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