#include "stdafx.h"
#include "../../xrEngine/stdafx.h"
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
		//ImGui::GetIO().Fonts->AddFontDefault(&FontsStorage[Font]);

		ImGui::GetIO().FontDefault = FontsStorage[Font];
	}
}

void XrUIManager::Initialize(HWND hWnd, const char* ini_path)
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();
	ImGuiIO& io = ImGui::GetIO();
	xr_strcpy(m_name_ini, ini_path);
	io.IniFilename = xr_strdup(Platform::ANSI_TO_UTF8(m_name_ini).c_str());
	io.ConfigWindowsMoveFromTitleBarOnly = true;

#ifdef IXR_WINDOWS
	const HDC screen = GetDC(nullptr);
	m_ScaleDpi = GetDeviceCaps(screen, LOGPIXELSX) / 96.f;
	ReleaseDC(nullptr, screen);
#else
	m_ScaleDpi = SDL_GetWindowDisplayScale(g_AppInfo.Window);
#endif
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
	io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;

	static const ImWchar icons_ranges[] = { ICON_MIN_FA, ICON_MAX_FA, 0 };
	ImFontConfig icons_config = {};
	icons_config.MergeMode = true;
	float FontSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::FontSize);
	FontsStorage["_fa"] = io.Fonts->AddFontFromMemoryCompressedTTF(FontAwesome_compressed_data, FontAwesome_compressed_size, FontSize * 0.75f, &icons_config, icons_ranges);

	//io.Fonts->Build();

	//ImGui_ImplWin32_Init(hWnd);
	ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window);
	RHIUtils::ImGui::Init();
}

void XrUIManager::Destroy()
{
	RHIUtils::ImGui::Destroy();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();
}

bool XrUIManager::ProcessEvent(void* Event)
{
	if (!ImGui_ImplSDL3_ProcessEvent((SDL_Event*)Event))
		return false;

	return true;
}

void XrUIManager::BeginFrame()
{
	for (auto str : LazyFonts)
	{
		LoadImGuiFontBase(str.c_str(), m_ScaleDpi);
	}

	LazyFonts.clear();

	ImGui_ImplSDL3_NewFrame();
	RHIUtils::ImGui::NewFrame();
}

void XrUIManager::EndFrame()
{
	ImGui::Render();
	RHIUtils::ImGui::DrawData();

	for (size_t i = ActualWindows.size(); i > 0; i--)
	{
		if (ActualWindows[i - 1]->IsClosed())
		{
			if (!ActualWindows[i - 1]->Flags.test(IEditorWnd::F_NoDelete))
			{
				xr_delete(ActualWindows[i - 1]);
			}
			ActualWindows.erase(ActualWindows.begin() + (i - 1));
			i = ActualWindows.size();
			if (i == 0)return;
		}
	}
}

void XrUIManager::MDIUpdate()
{
	ImGuiIO& io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_ViewportsEnable)
	{
		ImGui::UpdatePlatformWindows();
		ImGui::RenderPlatformWindowsDefault();
	}
}

void XrUIManager::ResetBegin()
{
	for (auto Ptr : ActualWindows)
	{
		Ptr->ResetBegin();
	}

	RHIUtils::ImGui::Reset();
}

void XrUIManager::ResetEnd(void* NewDevice)
{
	for (auto Ptr : ActualWindows)
	{
		Ptr->ResetEnd();
	}
}

void XrUIManager::OnDrawUI()
{
}

void XrUIManager::ApplyShortCutInput(DWORD Key)
{
	if ((ImGui::GetIO().WantTextInput))return;
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
		//case SDL_SCANCODE_ADD:
		//case SDL_SCANCODE_SUBTRACT:
		//case SDL_SCANCODE_MULTIPLY:
		//case SDL_SCANCODE_DIVIDE:
		//case SDL_SCANCODE_OEM_PLUS:
		//case SDL_SCANCODE_OEM_MINUS:
		//case SDL_SCANCODE_OEM_1:
		//case SDL_SCANCODE_OEM_COMMA:
		//case SDL_SCANCODE_OEM_PERIOD:
		//case SDL_SCANCODE_OEM_2:
		//case SDL_SCANCODE_OEM_4:
		//case SDL_SCANCODE_OEM_5:
		//case SDL_SCANCODE_OEM_6:
		//case SDL_SCANCODE_OEM_7:
		case SDL_SCANCODE_SPACE:
		case SDL_SCANCODE_CANCEL:
		case SDL_SCANCODE_RETURN:
			IsFail = false;
			break;
		default:
			break;
		}
	}
	if (IsFail)return;

	int ShiftState = ssNone;

	if (ImGui::GetIO().KeyShift)ShiftState |= ssShift;
	if (ImGui::GetIO().KeyCtrl)ShiftState |= ssCtrl;
	if (ImGui::GetIO().KeyAlt)ShiftState |= ssAlt;


	if (ImGui::IsMouseDown(ImGuiMouseButton_Left))ShiftState |= ssLeft;
	if (ImGui::IsMouseDown(ImGuiMouseButton_Right))ShiftState |= ssRight;
	ApplyShortCut(Key, ShiftState);
}

void XrUIManager::Push(IEditorWnd* ui, bool need_deleted)
{
	if (std::ranges::find(ActualWindows, ui) == ActualWindows.end())
	{
		ui->Flags.set(!need_deleted, IEditorWnd::F_NoDelete);

		if (Rendering)
		{
			NextWindows.push_back(ui);
			return;
		}

		ActualWindows.push_back(ui);
	}
}

void XrUIManager::Remove(IEditorWnd* ui)
{
	auto Iter = std::find(ActualWindows.begin(), ActualWindows.end(), ui);
	
	if (Iter != ActualWindows.end())
	{
		ActualWindows.erase(Iter);
	}
}

void XrUIManager::PushBegin(IEditorWnd* ui, bool need_deleted)
{
	ActualWindows.insert(ActualWindows.begin(), ui);
	ui->Flags.set(!need_deleted, IEditorWnd::F_NoDelete);
}

void XrUIManager::Draw()
{
	//BeginFrame(); 

	ImGui::NewFrame();
    ImGuizmo::BeginFrame();

	ImGui::PushFont(FontsStorage["_fa"]);
	ImGui::PushFont(FontsStorage[ImCurrentFont]);
	//ImGui::DockSpaceOverViewport();
	{
		m_MenuBarHeight = ScaleByDpi(64.f);
		m_MenuBarButtonHeight = m_MenuBarHeight - XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);


		float headerSize = 0.f
			+ XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::PanelPadding) * 2
			- 1 // WinAPI WindowBorder
			;


		ImGuiViewport* viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + m_MenuBarHeight + headerSize));
		ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, viewport->Size.y - headerSize - m_MenuBarHeight));
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGuiWindowFlags window_flags = 0
			| ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoDocking
			| ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse
			| ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
			| ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus;
        float separatorSize = ImGui::GetStyle().DockingSeparatorSize + 1 /*WinAPI WindowBorder*/;
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(separatorSize, separatorSize));
		ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
		ImGui::PushStyleColor(ImGuiCol_Border, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);

		ImGui::Begin("MyDockspace", NULL, window_flags);
		ImGui::DockSpace(ImGui::GetID("MyDockspace"));
		ImGui::End();

		ImGui::PopStyleColor(2); // Border, WindowBG
		ImGui::PopStyleVar(1); // WindowPadding

	}

	bool CopyBool = IsEnableInput;

	if (!CopyBool)
	{
		ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
	}

	if (!NextWindows.empty())
	{
		ActualWindows.insert(ActualWindows.end(), NextWindows.begin(), NextWindows.end());
		NextWindows.clear();
	}

	OnDrawUI();
	
	Rendering = true;
	for (IEditorWnd* ui : ActualWindows)
	{
		if (ui->TabIndex < 0 || ui->TabIndex == ActiveTabIndex)
		{
			ui->BeginDraw();
			ui->Draw();
			ui->EndDraw();
		}
	}
	Rendering = false;

	if (!CopyBool)
	{
		ImGui::PopItemFlag();
	}

	ImGui::PopFont();
	ImGui::PopFont();
	//ImGui::EndFrame();

	//EndFrame();
}

#if 0
static bool ImGui_ImplWin32_UpdateMouseCursor()
{
	ImGuiIO& io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_NoMouseCursorChange)
		return false;

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
		case ImGuiMouseCursor_Arrow:        win32_cursor = IDC_ARROW; break;
		case ImGuiMouseCursor_TextInput:    win32_cursor = IDC_IBEAM; break;
		case ImGuiMouseCursor_ResizeAll:    win32_cursor = IDC_SIZEALL; break;
		case ImGuiMouseCursor_ResizeEW:     win32_cursor = IDC_SIZEWE; break;
		case ImGuiMouseCursor_ResizeNS:     win32_cursor = IDC_SIZENS; break;
		case ImGuiMouseCursor_ResizeNESW:   win32_cursor = IDC_SIZENESW; break;
		case ImGuiMouseCursor_ResizeNWSE:   win32_cursor = IDC_SIZENWSE; break;
		case ImGuiMouseCursor_Hand:         win32_cursor = IDC_HAND; break;
		case ImGuiMouseCursor_NotAllowed:   win32_cursor = IDC_NO; break;
		}
		::SetCursor(::LoadCursor(NULL, win32_cursor));
	}
	return true;
}

#endif