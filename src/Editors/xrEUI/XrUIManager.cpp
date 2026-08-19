#include "stdafx.h"
#include "../../xrCore/RenderDebugPolicy.h"
#include "../../xrCore/RenderDocIntegration.h"
#include "../../xrEngine/stdafx.h"
#include "../../xrRHI/Layout/ImGui/RHIImGuiLayout.h"
#include "imgui_impl_sdl3.h"
#include "spectrum.h"
#include <SDL3/SDL.h>
#include "xrUITheme.h"
#include "ImGuizmo.h"
#include "imgui_internal.h"
#include "font/fa7.h"
#include "IconsFontAwesome7.h"
#include "../xrEUI/ModernUI.h"

XREUI_API XrUIManager* GUIManager = nullptr;

namespace
{
class FDx9UIRendererBackend final : public IXrUIRendererBackend
{
public:
	[[nodiscard]] EXrUIRendererPlatform GetPlatform() const noexcept override
	{
		return EXrUIRendererPlatform::D3D;
	}

	[[nodiscard]] bool SupportsPlatformViewports() const noexcept override
	{
		return true;
	}

	[[nodiscard]] bool OwnsMainPresentation() const noexcept override
	{
		return false;
	}

	[[nodiscard]] bool Initialize() override
	{
		if (!GRHI || !GRHI->DevicePtr ||
			GRHI->APILevel != ERHI_API_LAYER::D3D9)
		{
			return false;
		}
		RHIUtils::ImGui::Init();
		return true;
	}

	void Shutdown() override
	{
		RHIUtils::ImGui::Destroy();
	}

	void BeginFrame() override
	{
		RHIUtils::ImGui::NewFrame();
	}

	void RenderDrawData(ImDrawData& DrawData) override
	{
		(void)DrawData;
		RHIUtils::ImGui::DrawData();
	}

	void InvalidateDeviceObjects() override
	{
		RHIUtils::ImGui::Reset();
	}

	void CreateDeviceObjects() override
	{
		// DX9 backend пересоздаёт объекты в следующем NewFrame.
	}
};
} // namespace

XrUIManager::XrUIManager()
{
}

XrUIManager::~XrUIManager()
{
}

xr_map<xr_string, ImFont*> FontsStorage;
xr_string ImCurrentFont;
xr_vector<xr_string> LazyFonts;

void LoadImGuiFont(const char* Font)
{
	LazyFonts.push_back(Font);
}

void LoadImGuiFontBase(const char* Font, float scale)
{
	string_path FullPath;
	xr_string FixFontName = "editors\\" + xr_string(Font);
	FS.update_path(FullPath, _game_fonts_, FixFontName.data());
	ImFontConfig FontConfig = {};
	FontConfig.OversampleH = 3;

	if (FS.TryLoad(FullPath))
	{
		if (!FontsStorage.contains(Font))
		{
			FontsStorage[Font] = ImGui::GetIO().Fonts->AddFontFromFileTTF(Platform::ANSI_TO_UTF8(FullPath).c_str(), XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::FontSize), &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		}

		ImCurrentFont = Font;
		// ImGui::GetIO().Fonts->AddFontDefault(&FontsStorage[Font]);

		ImGui::GetIO().FontDefault = FontsStorage[Font];
	}
}

void XrUIManager::Initialize(HWND hWnd, const char* ini_path)
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();
	ImGuiIO& io = ImGui::GetIO();
	// Both editor renderer backends implement ImGui 1.92 texture uploads. This
	// flag must be visible before fonts are added, otherwise the atlas keeps the
	// legacy ownership mode and NRI receives an unresolved font texture ID.
	io.BackendFlags |= ImGuiBackendFlags_RendererHasTextures;
	xr_strcpy(m_name_ini, ini_path);
	io.IniFilename = xr_strdup(Platform::ANSI_TO_UTF8(m_name_ini).c_str());
	io.ConfigWindowsMoveFromTitleBarOnly = true;

	const HDC screen = GetDC(nullptr);
	m_ScaleDpi = GetDeviceCaps(screen, LOGPIXELSX) / 96.f;
	ReleaseDC(nullptr, screen);

	ImGui::GetStyle().ScaleAllSizes(m_ScaleDpi);
	io.ConfigDpiScaleFonts = true;
	io.ConfigDpiScaleViewports = true;

	CUIThemeManager::Get().InitDefault();
	Push(&CUIThemeManager::Get(), false);

	FS_FileSet Files;
	string_path Fonts = {};
	FS.update_path(Fonts, _game_fonts_, "editors\\");
	FS.file_list(Files, Fonts, 1, "*.ttf");

	auto OldFont = ImCurrentFont;
	for (auto& File : Files)
	{
		xr_string FileName = xr_path(File.name).xfilename();
		LoadImGuiFontBase(FileName.c_str(), m_ScaleDpi);
	}

	if (!OldFont.empty())
	{
		ImCurrentFont = OldFont;
	}

	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;

	static const ImWchar icons_ranges[] = {ICON_MIN_FA, ICON_MAX_FA, 0};
	ImFontConfig icons_config = {};
	icons_config.MergeMode = true;
	float FontSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::FontSize);
	FontsStorage["_fa"] = io.Fonts->AddFontFromMemoryCompressedTTF(FontAwesome_compressed_data, FontAwesome_compressed_size, FontSize * 0.75f, &icons_config, icons_ranges);

	// io.Fonts->Build();

	if (!m_RenderBackend)
	{
		m_RenderBackend = new FDx9UIRendererBackend();
		m_OwnRenderBackend = true;
	}

	switch (m_RenderBackend->GetPlatform())
	{
		case EXrUIRendererPlatform::D3D:
			R_ASSERT(ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window));
			break;
		case EXrUIRendererPlatform::Vulkan:
			R_ASSERT(ImGui_ImplSDL3_InitForVulkan(g_AppInfo.Window));
			break;
		case EXrUIRendererPlatform::Other:
			R_ASSERT(ImGui_ImplSDL3_InitForOther(g_AppInfo.Window));
			break;
	}

	R_ASSERT2(m_RenderBackend->Initialize(), "Failed to initialize the editor ImGui renderer backend");
	m_RenderBackendInitialized = true;
	if (m_RenderBackend->SupportsPlatformViewports())
	{
		io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
	}
	else
	{
		io.ConfigFlags &= ~ImGuiConfigFlags_ViewportsEnable;
	}
}

void XrUIManager::Destroy()
{
	m_MainPresentationPending = false;
	if (m_RenderBackendInitialized && m_RenderBackend)
	{
		m_RenderBackend->Shutdown();
	}
	m_RenderBackendInitialized = false;
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();
	if (m_OwnRenderBackend)
	{
		xr_delete(m_RenderBackend);
	}
	m_RenderBackend = nullptr;
	m_OwnRenderBackend = false;
}

bool XrUIManager::InstallRenderBackend(IXrUIRendererBackend* Backend) noexcept
{
	if (m_RenderBackendInitialized)
	{
		return false;
	}
	if (m_OwnRenderBackend)
	{
		xr_delete(m_RenderBackend);
	}
	m_RenderBackend = Backend;
	m_OwnRenderBackend = false;
	return true;
}

bool XrUIManager::ProcessEvent(void* Event)
{
	if (!ImGui_ImplSDL3_ProcessEvent((SDL_Event*)Event))
	{
		return false;
	}

	return true;
}

void XrUIManager::BeginFrame()
{
	R_ASSERT2(!m_MainPresentationPending, "The external editor renderer did not present the previous ImGui frame");

	for (auto str : LazyFonts)
	{
		LoadImGuiFontBase(str.c_str(), m_ScaleDpi);
	}

	LazyFonts.clear();

	ImGui_ImplSDL3_NewFrame();
	R_ASSERT(m_RenderBackendInitialized && m_RenderBackend);
	m_RenderBackend->BeginFrame();
}

void XrUIManager::EndFrame()
{
	ImGui::Render();
	if (ImDrawData* DrawData = ImGui::GetDrawData())
	{
		if (m_RenderBackend->OwnsMainPresentation())
		{
			m_MainPresentationPending = true;
		}
		else
		{
			m_RenderBackend->RenderDrawData(*DrawData);
		}
	}

	for (size_t i = m_UIArray.size(); i > 0; i--)
	{
		if (m_UIArray[i - 1]->IsClosed())
		{
			if (!m_UIArray[i - 1]->Flags.test(IEditorWnd::F_NoDelete))
			{
				xr_delete(m_UIArray[i - 1]);
			}
			m_UIArray.erase(m_UIArray.begin() + (i - 1));
			i = m_UIArray.size();
			if (i == 0)
			{
				return;
			}
		}
	}
}

void XrUIManager::PresentMainFrame()
{
	if (!m_MainPresentationPending)
	{
		return;
	}

	R_ASSERT(m_RenderBackendInitialized && m_RenderBackend);
	R_ASSERT2(m_RenderBackend->OwnsMainPresentation(), "Only an external editor renderer can own deferred main presentation");
	bool RenderDocCaptureStarted = false;
	void* RenderDocWindowHandle = nullptr;
	if (!m_RenderDocCaptureAttempted &&
		HasRenderCommandLineFlag(
			Core.Params ? Core.Params : "", "-renderdoc-capture"
		))
	{
		m_RenderDocCaptureAttempted = true;
		RenderDocWindowHandle = SDL_GetPointerProperty(
			SDL_GetWindowProperties(g_AppInfo.Window),
			SDL_PROP_WINDOW_WIN32_HWND_POINTER,
			nullptr
		);
		RenderDocCaptureStarted =
			xrRenderDoc::BeginCapture(RenderDocWindowHandle);
		Msg(RenderDocCaptureStarted
				? "* RenderDoc: explicit editor frame capture started"
				: "! RenderDoc: explicit editor frame capture could not start");
	}
	if (ImDrawData* DrawData = ImGui::GetDrawData())
	{
		m_RenderBackend->RenderDrawData(*DrawData);
	}
	if (RenderDocCaptureStarted)
	{
		Msg(xrRenderDoc::EndCapture(RenderDocWindowHandle)
				? "* RenderDoc: explicit editor frame capture completed"
				: "! RenderDoc: explicit editor frame capture could not complete");
	}
	m_MainPresentationPending = false;
}

void XrUIManager::MDIUpdate()
{
	ImGuiIO& io = ImGui::GetIO();
	if (m_RenderBackend && m_RenderBackend->SupportsPlatformViewports() &&
		(io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable))
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
}

void XrUIManager::ResetBegin()
{
	for (auto Ptr : m_UIArray)
	{
		Ptr->ResetBegin();
	}

	if (m_RenderBackendInitialized && m_RenderBackend)
	{
		m_RenderBackend->InvalidateDeviceObjects();
	}
}

void XrUIManager::ResetEnd(void* NewDevice)
{
	if (m_RenderBackendInitialized && m_RenderBackend)
	{
		m_RenderBackend->CreateDeviceObjects();
	}

	for (auto Ptr : m_UIArray)
	{
		Ptr->ResetEnd();
	}
}

void XrUIManager::OnDrawUI()
{
}

void XrUIManager::ApplyShortCutInput(DWORD Key)
{
	if ((ImGui::GetIO().WantTextInput))
	{
		return;
	}
	bool IsFail = true;
	if (Key >= SDL_SCANCODE_A && Key <= SDL_SCANCODE_Z)
	{
		IsFail = false;
	}
	else if (Key >= SDL_SCANCODE_1 && Key <= SDL_SCANCODE_0)
	{
		IsFail = false;
	}
	else
	{
		switch (Key)
		{
			case SDL_SCANCODE_LEFT:
			case SDL_SCANCODE_RIGHT:
			case SDL_SCANCODE_UP:
			case SDL_SCANCODE_DOWN:
			case SDL_SCANCODE_KP_0:
			case SDL_SCANCODE_KP_1:
			case SDL_SCANCODE_KP_2:
			case SDL_SCANCODE_KP_3:
			case SDL_SCANCODE_KP_4:
			case SDL_SCANCODE_KP_5:
			case SDL_SCANCODE_KP_6:
			case SDL_SCANCODE_KP_7:
			case SDL_SCANCODE_KP_8:
			case SDL_SCANCODE_KP_9:
			case SDL_SCANCODE_F1:
			case SDL_SCANCODE_F2:
			case SDL_SCANCODE_F3:
			case SDL_SCANCODE_F4:
			case SDL_SCANCODE_F5:
			case SDL_SCANCODE_F6:
			case SDL_SCANCODE_F7:
			case SDL_SCANCODE_F8:
			case SDL_SCANCODE_F9:
			case SDL_SCANCODE_F10:
			case SDL_SCANCODE_F11:
			case SDL_SCANCODE_F12:
			case SDL_SCANCODE_DELETE:
			case SDL_SCANCODE_RIGHTBRACKET:
			case SDL_SCANCODE_LEFTBRACKET:
			case SDL_SCANCODE_MENU:
			case SDL_SCANCODE_MINUS:
			case SDL_SCANCODE_EQUALS:
			case SDL_SCANCODE_BACKSLASH:
			// case SDL_SCANCODE_ADD:
			// case SDL_SCANCODE_SUBTRACT:
			// case SDL_SCANCODE_MULTIPLY:
			// case SDL_SCANCODE_DIVIDE:
			// case SDL_SCANCODE_OEM_PLUS:
			// case SDL_SCANCODE_OEM_MINUS:
			// case SDL_SCANCODE_OEM_1:
			// case SDL_SCANCODE_OEM_COMMA:
			// case SDL_SCANCODE_OEM_PERIOD:
			// case SDL_SCANCODE_OEM_2:
			// case SDL_SCANCODE_OEM_4:
			// case SDL_SCANCODE_OEM_5:
			// case SDL_SCANCODE_OEM_6:
			// case SDL_SCANCODE_OEM_7:
			case SDL_SCANCODE_SPACE:
			case SDL_SCANCODE_CANCEL:
			case SDL_SCANCODE_RETURN:
				IsFail = false;
				break;
			default:
				break;
		}
	}
	if (IsFail)
	{
		return;
	}

	int ShiftState = ssNone;

	if (ImGui::GetIO().KeyShift)
	{
		ShiftState |= ssShift;
	}
	if (ImGui::GetIO().KeyCtrl)
	{
		ShiftState |= ssCtrl;
	}
	if (ImGui::GetIO().KeyAlt)
	{
		ShiftState |= ssAlt;
	}


	if (ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		ShiftState |= ssLeft;
	}
	if (ImGui::IsMouseDown(ImGuiMouseButton_Right))
	{
		ShiftState |= ssRight;
	}
	ApplyShortCut(Key, ShiftState);
}

void XrUIManager::Push(IEditorWnd* ui, bool need_deleted)
{
	m_UIArray.push_back(ui);
	ui->Flags.set(!need_deleted, IEditorWnd::F_NoDelete);
}

void XrUIManager::Remove(IEditorWnd* ui)
{
	auto Iter = std::find(m_UIArray.begin(), m_UIArray.end(), ui);

	if (Iter != m_UIArray.end())
	{
		m_UIArray.erase(Iter);
	}
}

void XrUIManager::PushBegin(IEditorWnd* ui, bool need_deleted)
{
	m_UIArray.insert(m_UIArray.begin(), ui);
	ui->Flags.set(!need_deleted, IEditorWnd::F_NoDelete);
}

void XrUIManager::Draw()
{
	// BeginFrame();

	ImGui::NewFrame();
	ImGuizmo::BeginFrame();

	ImGui::PushFont(FontsStorage["_fa"]);
	ImGui::PushFont(FontsStorage[ImCurrentFont]);
	// ImGui::DockSpaceOverViewport();
	{
		m_MenuBarHeight = ScaleByDpi(64.f);
		m_MenuBarButtonHeight = m_MenuBarHeight - XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);


		float headerSize = 0.f + XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::PanelPadding) * 2 - 1 // WinAPI WindowBorder
			;


		ImGuiViewport* viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + m_MenuBarHeight + headerSize));
		ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, viewport->Size.y - headerSize - m_MenuBarHeight));
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGuiWindowFlags window_flags = 0 | ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus;
		float separatorSize = ImGui::GetStyle().DockingSeparatorSize + 1 /*WinAPI WindowBorder*/;
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(separatorSize, separatorSize));
		ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
		ImGui::PushStyleColor(ImGuiCol_Border, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
		ImGui::Begin("MyDockspace", NULL, window_flags);
		ImGuiID dockMain = ImGui::GetID("MyDockspace");

		////// Save off menu bar height for later.

		ImGui::DockSpace(dockMain);
		ImGui::End();
		ImGui::PopStyleColor(2); // Border, WindowBG
		ImGui::PopStyleVar(1);	 // WindowPadding
	}

	bool CopyBool = IsEnableInput;

	if (!CopyBool)
	{
		ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
	}

	OnDrawUI();

	for (IEditorWnd* ui : m_UIArray)
	{
		ui->BeginDraw();
		ui->Draw();
		ui->EndDraw();
	}

	if (!CopyBool)
	{
		ImGui::PopItemFlag();
	}

	ImGui::PopFont();
	ImGui::PopFont();
	// ImGui::EndFrame();

	// EndFrame();
}

static bool ImGui_ImplWin32_UpdateMouseCursor()
{
	ImGuiIO& io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_NoMouseCursorChange)
	{
		return false;
	}

	ImGuiMouseCursor imgui_cursor = ImGui::GetMouseCursor();
	if (imgui_cursor == ImGuiMouseCursor_None || io.MouseDrawCursor)
	{
		// Hide OS mouse cursor if imgui is drawing it or if it wants no cursor
		::SetCursor(NULL);
	}
	else
	{
		// Show OS mouse cursor
		LPTSTR win32_cursor = IDC_ARROW;
		switch (imgui_cursor)
		{
			case ImGuiMouseCursor_Arrow:
				win32_cursor = IDC_ARROW;
				break;
			case ImGuiMouseCursor_TextInput:
				win32_cursor = IDC_IBEAM;
				break;
			case ImGuiMouseCursor_ResizeAll:
				win32_cursor = IDC_SIZEALL;
				break;
			case ImGuiMouseCursor_ResizeEW:
				win32_cursor = IDC_SIZEWE;
				break;
			case ImGuiMouseCursor_ResizeNS:
				win32_cursor = IDC_SIZENS;
				break;
			case ImGuiMouseCursor_ResizeNESW:
				win32_cursor = IDC_SIZENESW;
				break;
			case ImGuiMouseCursor_ResizeNWSE:
				win32_cursor = IDC_SIZENWSE;
				break;
			case ImGuiMouseCursor_Hand:
				win32_cursor = IDC_HAND;
				break;
			case ImGuiMouseCursor_NotAllowed:
				win32_cursor = IDC_NO;
				break;
		}
		::SetCursor(::LoadCursor(NULL, win32_cursor));
	}
	return true;
}
