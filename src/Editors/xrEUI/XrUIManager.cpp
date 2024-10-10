#include "stdafx.h"
#include "../../xrEngine/stdafx.h"
#include "imgui_impl_dx9.h"
#include "imgui_impl_sdl3.h"
#include "spectrum.h"
#include <SDL3/SDL.h>
#include "xrUITheme.h"
#include "ImGuizmo.h"

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

void LoadImGuiFontBase(const char* Font)
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
			FontsStorage[Font] = ImGui::GetIO().Fonts->AddFontFromFileTTF(Platform::ANSI_TO_UTF8(FullPath).c_str(), 14.0f, &FontConfig, ImGui::GetIO().Fonts->GetGlyphRangesCyrillic());
		}

		ImCurrentFont = Font;
		//ImGui::GetIO().Fonts->AddFontDefault(&FontsStorage[Font]);
	}
}

void XrUIManager::Initialize(HWND hWnd, IDirect3DDevice9* device, const char* ini_path)
{
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();

	ImGuiPlatformIO& platform_io = ImGui::GetPlatformIO();
	ImGuiIO& io = ImGui::GetIO();
	xr_strcpy(m_name_ini, ini_path);
	io.IniFilename = m_name_ini;
	io.ConfigWindowsMoveFromTitleBarOnly = true;

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
		LoadImGuiFontBase(FileName.c_str());
	}

	if (!OldFont.empty())
	{
		ImCurrentFont = OldFont;
	}

	io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;         // Enable Docking
	io.Fonts->Build();
	//ImGui_ImplWin32_Init(hWnd);
	ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window);
	ImGui_ImplDX9_Init(device);
}

void XrUIManager::Destroy()
{
	ImGui_ImplDX9_Shutdown();
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
		LoadImGuiFontBase(str.c_str());
	}

	LazyFonts.clear();

	ImGui_ImplSDL3_NewFrame();
	ImGui_ImplDX9_NewFrame();
}

void XrUIManager::EndFrame()
{
	ImGui::GetForegroundDrawList()->AddCircle({ 66, 56 }, 55, 512351, 4);
	ImGui::Render();
	ImGui_ImplDX9_RenderDrawData(ImGui::GetDrawData());

	for (size_t i = m_UIArray.size(); i > 0; i--)
	{
		if (m_UIArray[i - 1]->IsClosed())
		{
			if (!m_UIArray[i - 1]->Flags.test(XrUI::F_NoDelete))
			{
				xr_delete(m_UIArray[i - 1]);
			}
			m_UIArray.erase(m_UIArray.begin() + (i - 1));
			i = m_UIArray.size();
			if (i == 0)return;
		}
	}
}

void XrUIManager::ResetBegin()
{
	for (auto Ptr : m_UIArray)
	{
		Ptr->ResetBegin();
	}

	ImGui_ImplDX9_Shutdown();
}

void XrUIManager::ResetEnd(void* NewDevice)
{
	ImGui_ImplDX9_Init((IDirect3DDevice9*)NewDevice);

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

void XrUIManager::Push(XrUI* ui, bool need_deleted)
{
	m_UIArray.push_back(ui);
	ui->Flags.set(!need_deleted, XrUI::F_NoDelete);
}

void XrUIManager::PushBegin(XrUI* ui, bool need_deleted)
{
	m_UIArray.insert(m_UIArray.begin(), ui);
	ui->Flags.set(!need_deleted, XrUI::F_NoDelete);
}

void XrUIManager::Draw()
{
	//BeginFrame(); 

	ImGui::NewFrame();
    ImGuizmo::BeginFrame();

	ImGui::PushFont(FontsStorage[ImCurrentFont]);
	//ImGui::DockSpaceOverViewport();
	{
		ImGuiViewport* viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + UIToolBarSize / 2));
		ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, viewport->Size.y - (UIToolBarSize / 2)));
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGuiWindowFlags window_flags = 0
			| ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoDocking
			| ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse
			| ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
			| ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus;

		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, UIToolBarSize / 2));
		ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(3, 2));
		ImGui::Begin("MyDockspace", NULL, window_flags);
		ImGuiID dockMain = ImGui::GetID("MyDockspace");

		m_MenuBarHeight = ImGui::GetWindowBarHeight();
		// Save off menu bar height for later.

		ImGui::DockSpace(dockMain);
		ImGui::End();
		ImGui::PopStyleVar(4);

	}

	bool CopyBool = IsEnableInput;

	if (!CopyBool)
	{
		ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
	}
	
	OnDrawUI();

	for (XrUI* ui : m_UIArray)
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
	//ImGui::EndFrame();

	//EndFrame();
}

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