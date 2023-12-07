#include "stdafx.h"
#include "../xrCore/_std_extensions.h"
#include "imgui_impl_sdl3.h"
#include "imgui_internal.h"

#include <d3d11.h>
#include <d3d9.h>
#include "imgui.h"

#pragma comment(lib, "d3d9.lib")
#pragma comment(lib, "d3d11.lib")
#pragma comment(lib, "dxgi.lib")

namespace ImGui
{
	ImFont* LightFont = nullptr;
	ImFont* RegularFont = nullptr;
	ImFont* MediumFont = nullptr;
	ImFont* BoldFont = nullptr;
}

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

struct DrawCommand
{
	int Order;
	xr_string Name;
	std::function<void()> Function;
};

static xr_vector<DrawCommand>* DrawCommands = nullptr;

void free_vid_mode_list()
{
	for (int i = 0; vid_mode_token[i].name; i++) {
		xr_free(vid_mode_token[i].name);
	}

	xr_free(vid_mode_token);
	vid_mode_token = NULL;
}

struct _uniq_mode
{
	_uniq_mode(LPCSTR v) :_val(v) {}
	LPCSTR _val;
	bool operator() (LPCSTR _other) { return !_stricmp(_val, _other); }
};

void fill_vid_mode_list()
{
	if (vid_mode_token != NULL)		return;
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

	for (u32 i = 0; i < num; ++i)
	{
		DXGI_MODE_DESC& desc = modes[i];
		string32		str;

		if (desc.Width < 800)
			continue;

		xr_sprintf(str, sizeof(str), "%dx%d", desc.Width, desc.Height);

		if (_tmp.end() != std::find_if(_tmp.begin(), _tmp.end(), _uniq_mode(str)))
			continue;

		_tmp.push_back(NULL);
		_tmp.back() = xr_strdup(str);
	}

	u32 _cnt = (u32)_tmp.size() + 1;

	vid_mode_token = xr_alloc<xr_token>(_cnt);

	vid_mode_token[_cnt - 1].id = -1;
	vid_mode_token[_cnt - 1].name = NULL;

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
}

bool CreateD3D9();
bool CreateD3D11();

bool UpdateBuffersD3D11();
void ResizeBuffersD3D11(u16 Width, u16 Height);

bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);

void DestroyD3D9();
void DestroyD3D11();

static void LoadImGuiFont(ImFont*& FontHandle, const char* Font)
{
	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, Font);
	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	if (FS.exist(FullPath))
	{
		FontHandle = ImGui::GetIO().Fonts->AddFontFromFileTTF(ANSI_TO_UTF8(FullPath).c_str(), 16.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		R_ASSERT(FontHandle);
	}
}

static void InitImGui()
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	//io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;       // Enable Multi-Viewport / Platform Windows
	//io.BackendFlags |= ImGuiBackendFlags_PlatformHasViewports;
	//io.BackendFlags |= ImGuiBackendFlags_RendererHasViewports;

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();

	ImGuiStyle& Style = ImGui::GetStyle();
	Style.WindowPadding.x = 8;
	Style.WindowPadding.y = 8;
	Style.FramePadding.x = 5;
	Style.FramePadding.y = 5;
	Style.CellPadding.x = 2;
	Style.CellPadding.y = 2;
	Style.ItemSpacing.x = 8;
	Style.ItemSpacing.y = 4;
	Style.ItemInnerSpacing.x = 6;
	Style.ItemInnerSpacing.y = 6;
	Style.ScrollbarSize = 16;
	Style.GrabMinSize = 16;
	Style.FrameRounding = 2;
	Style.PopupRounding = 2;
	Style.ScrollbarRounding = 2;
	Style.GrabRounding = 2;
	Style.TabRounding = 2;

	ImVec4* colors = Style.Colors;
	colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	colors[ImGuiCol_TextDisabled] = ImVec4(0.26f, 0.26f, 0.26f, 1.00f);
	colors[ImGuiCol_WindowBg] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PopupBg] = ImVec4(0.08f, 0.08f, 0.08f, 1.00f);
	colors[ImGuiCol_Border] = ImVec4(0.16f, 0.16f, 0.18f, 0.50f);
	colors[ImGuiCol_BorderShadow] = ImVec4(0.05f, 0.05f, 0.05f, 0.00f);
	colors[ImGuiCol_FrameBg] = ImVec4(0.13f, 0.13f, 0.13f, 0.54f);
	colors[ImGuiCol_FrameBgHovered] = ImVec4(0.27f, 0.27f, 0.27f, 0.54f);
	colors[ImGuiCol_FrameBgActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_TitleBg] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgActive] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.04f, 0.04f, 0.04f, 1.00f);
	colors[ImGuiCol_MenuBarBg] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_ScrollbarBg] = ImVec4(0.02f, 0.02f, 0.02f, 0.53f);
	colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.31f, 0.31f, 0.31f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.41f, 0.41f, 0.41f, 1.00f);
	colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_CheckMark] = ImVec4(0.83f, 0.83f, 0.83f, 0.54f);
	colors[ImGuiCol_SliderGrab] = ImVec4(0.51f, 0.51f, 0.51f, 1.00f);
	colors[ImGuiCol_SliderGrabActive] = ImVec4(0.95f, 0.95f, 0.95f, 1.00f);
	colors[ImGuiCol_Button] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_ButtonHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_ButtonActive] = ImVec4(0.75f, 0.75f, 0.75f, 0.54f);
	colors[ImGuiCol_Header] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_HeaderHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_HeaderActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_Separator] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_SeparatorHovered] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_SeparatorActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_ResizeGrip] = ImVec4(0.25f, 0.25f, 0.25f, 0.54f);
	colors[ImGuiCol_ResizeGripHovered] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_ResizeGripActive] = ImVec4(0.75f, 0.74f, 0.74f, 0.54f);
	colors[ImGuiCol_Tab] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabHovered] = ImVec4(0.30f, 0.30f, 0.30f, 1.00f);
	colors[ImGuiCol_TabActive] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_TabUnfocused] = ImVec4(0.21f, 0.21f, 0.21f, 0.54f);
	colors[ImGuiCol_TabUnfocusedActive] = ImVec4(0.42f, 0.42f, 0.42f, 0.54f);
	colors[ImGuiCol_DockingPreview] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DockingEmptyBg] = ImVec4(0.05f, 0.05f, 0.05f, 1.00f);
	colors[ImGuiCol_PlotLines] = ImVec4(0.61f, 0.61f, 0.61f, 1.00f);
	colors[ImGuiCol_PlotLinesHovered] = ImVec4(1.00f, 0.43f, 0.35f, 1.00f);
	colors[ImGuiCol_PlotHistogram] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.60f, 0.00f, 1.00f);
	colors[ImGuiCol_TableHeaderBg] = ImVec4(0.19f, 0.19f, 0.20f, 1.00f);
	colors[ImGuiCol_TableBorderStrong] = ImVec4(0.31f, 0.31f, 0.35f, 1.00f);
	colors[ImGuiCol_TableBorderLight] = ImVec4(0.23f, 0.23f, 0.25f, 1.00f);
	colors[ImGuiCol_TableRowBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_TableRowBgAlt] = ImVec4(1.00f, 1.00f, 1.00f, 0.06f);
	colors[ImGuiCol_TextSelectedBg] = ImVec4(0.46f, 0.46f, 0.46f, 0.54f);
	colors[ImGuiCol_DragDropTarget] = ImVec4(1.00f, 1.00f, 0.00f, 0.90f);
	colors[ImGuiCol_NavHighlight] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
	colors[ImGuiCol_NavWindowingHighlight] = ImVec4(1.00f, 1.00f, 1.00f, 0.70f);
	colors[ImGuiCol_NavWindowingDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.20f);
	colors[ImGuiCol_ModalWindowDimBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.35f);

	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 2;

	LoadImGuiFont(ImGui::RegularFont, "RobotoMono.ttf");
	LoadImGuiFont(ImGui::LightFont, "RobotoMono-Light.ttf");
	LoadImGuiFont(ImGui::MediumFont, "RobotoMono-Medium.ttf");
	LoadImGuiFont(ImGui::BoldFont, "RobotoMono-Bold.ttf");
	
	io.Fonts->Build();
}

bool CRenderDevice::InitRenderDevice(APILevel API)
{
	if (DrawCommands == nullptr) {
		DrawCommands = xr_new<xr_vector<DrawCommand>>();
	}

	fill_vid_mode_list();
	InitImGui();
	if (!ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window)) {
		return false;
	}

	AddUICommand("default", 0, [this]() {
		ImGui::SetNextWindowPos(ImVec2(0, 0));
		ImGui::SetNextWindowSize(ImVec2(TargetWidth, TargetHeight));

		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
		ImGui::Begin("Main", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs);
		ImGui::Image(RenderSRV, ImVec2(TargetWidth, TargetHeight));
		ImGui::End();
		ImGui::PopStyleVar();
		ImGui::PopStyleVar();

		ImGui::ShowDemoWindow();
	});

	switch (API) {

	case APILevel::DX9:
		if (!CreateD3D9()) {
			return false;
		}
		break;

	case APILevel::DX11:
		if (!CreateD3D11()) {
			return false;
		}
		break;

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
	switch (CurrentAPILevel) {

	case APILevel::DX9:
		DestroyD3D9();
		break;

	case APILevel::DX11:
		DestroyD3D11();
		break;

	default:
		break;
	}

	free_vid_mode_list();
	xr_delete(DrawCommands);
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
	return RenderRTV;
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

void CRenderDevice::ResizeBuffers(u16 Width, u16 Height)
{
	switch (CurrentAPILevel) {

	case APILevel::DX9:
		ResizeBuffersD3D9(Width, Height);
		break;

	case APILevel::DX11:
		ResizeBuffersD3D11(Width, Height);
		break;

	default:
		break;
	}

	const bool Centered = strstr(Core.Params, "-no_center_screen") == nullptr;
	SDL_SetWindowSize(g_AppInfo.Window, Width, Height);
	SDL_SetWindowPosition(g_AppInfo.Window, Centered ? SDL_WINDOWPOS_CENTERED : 0, Centered ? SDL_WINDOWPOS_CENTERED : 0);
	Device.TargetWidth = Width;
	Device.TargetHeight = Height;
}

D3D_FEATURE_LEVEL CRenderDevice::GetFeatureLevel()
{
	return FeatureLevel;
}

RENDERDOC_API_1_6_0* CRenderDevice::GetRenderDocAPI()
{
	return nullptr;
}

bool CRenderDevice::IsCapturingInputs()
{
	return CaptureInputs;
}

void CRenderDevice::BeginRender()
{
	ImGui_ImplSDL3_NewFrame();
	if (ImGui::IsKeyPressed(ImGuiKey_I) && ImGui::IsKeyDown(ImGuiKey_LeftAlt)) {
		if (ImGui::IsKeyDown(ImGuiKey_LeftCtrl)) {
			DrawUIRender = !DrawUIRender;
		} else {
			CaptureInputs = !CaptureInputs;
		}
	}

	if (CaptureInputs) {
		SDL_ShowCursor();
	} else {
		SDL_HideCursor();
	}
}

void CRenderDevice::EndRender()
{
}

void CRenderDevice::DrawUI()
{
	if (DrawUIRender) {
		for (const auto& Command : *DrawCommands) {
			Command.Function();
		}

		if (!CaptureInputs) {
			for (auto& Window : ImGui::GetCurrentContext()->Windows) {
				Window->Flags |= ImGuiWindowFlags_NoInputs;
			}
		}
	} else {
		ImGui::SetNextWindowPos(ImVec2(0, 0));
		ImGui::SetNextWindowSize(ImVec2(TargetWidth, TargetHeight));

		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
		ImGui::Begin("Main", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs);
		ImGui::Image(RenderSRV, ImVec2(TargetWidth, TargetHeight));
		ImGui::End();
		ImGui::PopStyleVar();
		ImGui::PopStyleVar();
	}
}

void CRenderDevice::AddUICommand(const char* Name, int Order, std::function<void()>&& Function)
{
	auto It = std::find_if(DrawCommands->begin(), DrawCommands->end(), [Name](const DrawCommand& a) { return !a.Name.compare(Name); });
	VERIFY(It == DrawCommands->end());
	if (It != DrawCommands->end()) {
		return;
	}

	DrawCommands->emplace_back(DrawCommand(Order, Name, Function));
	std::sort(DrawCommands->begin(), DrawCommands->end(), [](const auto& a, const auto& b) { return a.Order < b.Order; });
}

void CRenderDevice::RemoveUICommand(const char* Name)
{
	auto It = std::find_if(DrawCommands->begin(), DrawCommands->end(), [Name](const DrawCommand& a) { return !a.Name.compare(Name); });
	VERIFY(It != DrawCommands->end());
	if (It == DrawCommands->end()) {
		return;
	}

	DrawCommands->erase(It);
	std::sort(DrawCommands->begin(), DrawCommands->end(), [](const auto& a, const auto& b) { return a.Order < b.Order; });
}
