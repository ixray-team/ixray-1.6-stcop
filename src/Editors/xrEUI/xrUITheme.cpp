#include "stdafx.h"
#include "xrUITheme.h"
#include "imgui_user.h"
#include "../xrEUI/ModernUI.h"

void LoadImGuiFont(const char* Font);
extern xr_string ImCurrentFont;

using namespace XRay::ImGui;

CUIThemeManager::CUIThemeManager()
{
	bOpen = false;
}

CUIThemeManager& CUIThemeManager::Get()
{
	static CUIThemeManager Instance;

	return Instance;
}

void CUIThemeManager::Draw()
{
	if (!bOpen)
		return;

	if (ImGui::Begin("Theme Editor", &bOpen))
	{
		IsDocked = ImGui::IsWindowDocked();
		IsFocused = ImGui::IsWindowFocused();

		ImVec4* colors = ImGui::GetStyle().Colors;
		ImVec2 button_size = ImVec2(100.0f, 20.0f);

		if (ImGui::Button("Theme 1"))
		{
			InitDefault(0);
		}
		ImGui::SameLine();

		if (ImGui::Button("Theme 2"))
		{
			InitDefault(1);
		}

		ImGui::BeginChild("Theme", ImVec2(0, -ImGui::GetFrameHeightWithSpacing() - 10), true);
		{
			// Main Colors
			ImGui::SeparatorText("Main Colors");
			ImGui::ColorEdit4("Window Background", (float*)&colors[ImGuiCol_WindowBg]);
			ImGui::ColorEdit4("Child Background", (float*)&colors[ImGuiCol_ChildBg]);
			ImGui::ColorEdit4("Popup Background", (float*)&colors[ImGuiCol_PopupBg]);
			ImGui::ColorEdit4("Menu Bar Background", (float*)&colors[ImGuiCol_MenuBarBg]);

			// Text
			ImGui::SeparatorText("Text Colors");
			ImGui::ColorEdit4("Text", (float*)&colors[ImGuiCol_Text]);
			ImGui::ColorEdit4("Text Disabled", (float*)&colors[ImGuiCol_TextDisabled]);
			ImGui::ColorEdit4("Text Selected BG", (float*)&colors[ImGuiCol_TextSelectedBg]);
			ImGui::ColorEdit4("Text Link", (float*)&colors[ImGuiCol_TextLink]);

			// Frames & Borders
			ImGui::SeparatorText("Frames & Borders");
			ImGui::ColorEdit4("Frame Background", (float*)&colors[ImGuiCol_FrameBg]);
			ImGui::ColorEdit4("Frame Hovered", (float*)&colors[ImGuiCol_FrameBgHovered]);
			ImGui::ColorEdit4("Frame Active", (float*)&colors[ImGuiCol_FrameBgActive]);
			ImGui::ColorEdit4("Border", (float*)&colors[ImGuiCol_Border]);
			ImGui::ColorEdit4("Border Shadow", (float*)&colors[ImGuiCol_BorderShadow]);

			// Buttons
			ImGui::SeparatorText("Buttons");
			ImGui::ColorEdit4("Button", (float*)&colors[ImGuiCol_Button]);
			ImGui::ColorEdit4("Button Hovered", (float*)&colors[ImGuiCol_ButtonHovered]);
			ImGui::ColorEdit4("Button Active", (float*)&colors[ImGuiCol_ButtonActive]);

			// Headers
			ImGui::SeparatorText("Headers");
			ImGui::ColorEdit4("Header", (float*)&colors[ImGuiCol_Header]);
			ImGui::ColorEdit4("Header Hovered", (float*)&colors[ImGuiCol_HeaderHovered]);
			ImGui::ColorEdit4("Header Active", (float*)&colors[ImGuiCol_HeaderActive]);

			// Titles
			ImGui::SeparatorText("Window Titles");
			ImGui::ColorEdit4("Title Background", (float*)&colors[ImGuiCol_TitleBg]);
			ImGui::ColorEdit4("Title Active", (float*)&colors[ImGuiCol_TitleBgActive]);
			ImGui::ColorEdit4("Title Collapsed", (float*)&colors[ImGuiCol_TitleBgCollapsed]);

			// Scrollbars
			ImGui::SeparatorText("Scrollbars");
			ImGui::ColorEdit4("Scrollbar Background", (float*)&colors[ImGuiCol_ScrollbarBg]);
			ImGui::ColorEdit4("Scrollbar Grab", (float*)&colors[ImGuiCol_ScrollbarGrab]);
			ImGui::ColorEdit4("Scrollbar Grab Hovered", (float*)&colors[ImGuiCol_ScrollbarGrabHovered]);
			ImGui::ColorEdit4("Scrollbar Grab Active", (float*)&colors[ImGuiCol_ScrollbarGrabActive]);

			// Sliders
			ImGui::SeparatorText("Sliders");
			ImGui::ColorEdit4("Slider Grab", (float*)&colors[ImGuiCol_SliderGrab]);
			ImGui::ColorEdit4("Slider Grab Active", (float*)&colors[ImGuiCol_SliderGrabActive]);

			// Checkboxes & Radio Buttons
			ImGui::SeparatorText("Checkboxes & Radio");
			ImGui::ColorEdit4("Check Mark", (float*)&colors[ImGuiCol_CheckMark]);

			// Separators
			ImGui::SeparatorText("Separators");
			ImGui::ColorEdit4("Separator", (float*)&colors[ImGuiCol_Separator]);
			ImGui::ColorEdit4("Separator Hovered", (float*)&colors[ImGuiCol_SeparatorHovered]);
			ImGui::ColorEdit4("Separator Active", (float*)&colors[ImGuiCol_SeparatorActive]);

			// Resize Grips
			ImGui::SeparatorText("Resize Grips");
			ImGui::ColorEdit4("Resize Grip", (float*)&colors[ImGuiCol_ResizeGrip]);
			ImGui::ColorEdit4("Resize Grip Hovered", (float*)&colors[ImGuiCol_ResizeGripHovered]);
			ImGui::ColorEdit4("Resize Grip Active", (float*)&colors[ImGuiCol_ResizeGripActive]);

			// Tables
			ImGui::SeparatorText("Tables");
			ImGui::ColorEdit4("Table Header", (float*)&colors[ImGuiCol_TableHeaderBg]);
			ImGui::ColorEdit4("Table Border Strong", (float*)&colors[ImGuiCol_TableBorderStrong]);
			ImGui::ColorEdit4("Table Border Light", (float*)&colors[ImGuiCol_TableBorderLight]);
			ImGui::ColorEdit4("Table Row BG", (float*)&colors[ImGuiCol_TableRowBg]);
			ImGui::ColorEdit4("Table Row BG Alt", (float*)&colors[ImGuiCol_TableRowBgAlt]);

			// Tabs
			ImGui::SeparatorText("Tabs");
			ImGui::ColorEdit4("Tab", (float*)&colors[ImGuiCol_Tab]);
			ImGui::ColorEdit4("Tab Hovered", (float*)&colors[ImGuiCol_TabHovered]);
			ImGui::ColorEdit4("Tab Selected", (float*)&colors[ImGuiCol_TabSelected]);
			ImGui::ColorEdit4("Tab Unfocused", (float*)&colors[ImGuiCol_TabUnfocused]);
			ImGui::ColorEdit4("Tab Selected Unfocused", (float*)&colors[ImGuiCol_TabUnfocusedActive]);

			// Plots
			ImGui::SeparatorText("Plots");
			ImGui::ColorEdit4("Plot Lines", (float*)&colors[ImGuiCol_PlotLines]);
			ImGui::ColorEdit4("Plot Lines Hovered", (float*)&colors[ImGuiCol_PlotLinesHovered]);
			ImGui::ColorEdit4("Plot Histogram", (float*)&colors[ImGuiCol_PlotHistogram]);
			ImGui::ColorEdit4("Plot Histogram Hovered", (float*)&colors[ImGuiCol_PlotHistogramHovered]);

			// Navigation
			ImGui::SeparatorText("Navigation");
			ImGui::ColorEdit4("Nav Cursor", (float*)&colors[ImGuiCol_NavCursor]);
			ImGui::ColorEdit4("Nav Highlight", (float*)&colors[ImGuiCol_NavWindowingHighlight]);
			ImGui::ColorEdit4("Nav Dim Background", (float*)&colors[ImGuiCol_NavWindowingDimBg]);

			// Docking
			ImGui::SeparatorText("Docking");
			ImGui::ColorEdit4("Docking Preview", (float*)&colors[ImGuiCol_DockingPreview]);
			ImGui::ColorEdit4("Docking Empty BG", (float*)&colors[ImGuiCol_DockingEmptyBg]);

			// Tree Nodes
			ImGui::SeparatorText("Tree Nodes");
			ImGui::ColorEdit4("Tree Lines", (float*)&colors[ImGuiCol_TreeLines]);

			// Drag & Drop
			ImGui::SeparatorText("Drag & Drop");
			ImGui::ColorEdit4("Drag Drop Target", (float*)&colors[ImGuiCol_DragDropTarget]);

			// Modal
			ImGui::SeparatorText("Modal Windows");
			ImGui::ColorEdit4("Modal Dim Background", (float*)&colors[ImGuiCol_ModalWindowDimBg]);

			// Input Text
			ImGui::SeparatorText("Input Text");
			ImGui::ColorEdit4("Text Cursor", (float*)&colors[ImGuiCol_InputTextCursor]);

			// Log Messages
			ImGui::SeparatorText("Log Messages");
			ImGui::ColorEdit4("Error", (float*)&log_color_error);
			ImGui::ColorEdit4("Warning", (float*)&log_color_warning);
			ImGui::ColorEdit4("Debug", (float*)&log_color_debug);
			ImGui::ColorEdit4("Default", (float*)&log_color_default);

			ImGui::SeparatorText("Fonts");
			FS_FileSet Files;
			string_path Fonts = {};
			FS.update_path(Fonts, _game_fonts_, "editors\\");
			FS.file_list(Files, Fonts, 1, "*.ttf");

			if (ImGui::BeginCombo("Main font", ImCurrentFont.data()))
			{
				for (auto& File : Files)
				{
					xr_string FileName = xr_path(File.name).xfilename();
					bool is_selected = (ImCurrentFont == FileName);
					if (ImGui::Selectable(FileName.c_str(), is_selected))
					{
						LoadImGuiFont(FileName.c_str());
					}

					if (is_selected)
						ImGui::SetItemDefaultFocus();
				}
				ImGui::EndCombo();
			}

			ImGui::SeparatorText("Other");
			ImGui::PushItemWidth(150);
			ImGui::SliderFloat("Active window transparent", &TransparentDefault, 0.1f, 1.f, "%.1f");
			ImGui::SliderFloat("Inactive window transparent", &TransparentUnfocused, 0.1f, 1.f, "%.1f");
			ImGui::PopItemWidth();

			ImGui::SeparatorText("Paddings");
			ImGui::PushItemWidth(150);

			ImGuiStyle& style = ImGui::GetStyle();
			ImGui::SliderFloat("Cell Padding X", &style.CellPadding.x, 0.f, 10.f, "%.1f");
			ImGui::SliderFloat("Cell Padding Y", &style.CellPadding.y, 0.f, 10.f, "%.1f");

			ImGui::SliderFloat("DockingSeparatorSize", &style.DockingSeparatorSize, 0.f, 10.f, "%.1f");


			if (!IsDocked)
				IsDocked = ImGui::IsWindowDocked();
			if (!IsFocused)
				IsFocused = ImGui::IsWindowFocused();
		}
		ImGui::EndChild();
		ImGui::Spacing();
	}
	ImGui::End();
}

void CUIThemeManager::InitDefault(int InThemeID)
{
	if (InThemeID >= 0)
	{
		ThemeID = InThemeID;
	}

	XRay::ImGui::MakeEditorTheme();
	XRay::ImGui::SetupColorsList(ThemeID);

	ImGuiStyle& style = ImGui::GetStyle();
	ImVec4* colors = style.Colors;
	colors[ImGuiCol_MenuBarBg]				= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_FrameBg]				= GetEditorColor(EEditorColors::TableTint);
	colors[ImGuiCol_WindowBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_Border]					= GetEditorColor(EEditorColors::PanelBorderTint);
	colors[ImGuiCol_ChildBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_TableRowBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_TableRowBgAlt]			= GetEditorColor(EEditorColors::ButtonTint);
	colors[ImGuiCol_TableBorderLight]		= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_TableBorderStrong]		= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_TableHeaderBg]			= GetEditorColor(EEditorColors::ButtonTint);

	colors[ImGuiCol_Tab]					= GetEditorColor(EEditorColors::TabBarTint);
	colors[ImGuiCol_TabSelected]			= GetEditorColor(EEditorColors::PanelBorderTint);

	colors[ImGuiCol_TabActive]				= GetEditorColor(EEditorColors::PanelBorderTint);
	colors[ImGuiCol_TabHovered]				= GetEditorColor(EEditorColors::TabHover);

	colors[ImGuiCol_TabDimmed]				= GetEditorColor(EEditorColors::TabBarTint);
	colors[ImGuiCol_TabDimmedSelected]		= GetEditorColor(EEditorColors::PanelBorderTint);

	colors[ImGuiCol_TitleBg]				= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_TitleBgActive]			= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_BorderShadow]			= ImVec4(0.f, 0.f, 0.f, 0.f);

	colors[ImGuiCol_Button]					= GetEditorColor(EEditorColors::ButtonTint);
	colors[ImGuiCol_ButtonHovered]			= GetEditorColor(EEditorColors::ButtonHover);
	colors[ImGuiCol_ButtonActive]			= GetEditorColor(EEditorColors::ButtonActive);

	colors[ImGuiCol_Header]					= GetEditorColor(EEditorColors::PanelBorderTint);
	colors[ImGuiCol_HeaderHovered]			= GetEditorColor(EEditorColors::TabHover);
	colors[ImGuiCol_HeaderActive]			= GetEditorColor(EEditorColors::TabActive);
	colors[ImGuiCol_DockingEmptyBg]			= GetEditorColor(EEditorColors::BackgroundTint);

	colors[ImGuiCol_Separator]				= GetEditorColor(EEditorColors::PanelBorderTint);

	colors[ImGuiCol_ScrollbarBg]			= GetEditorColor(EEditorColors::TableTint);
	colors[ImGuiCol_ScrollbarGrab]			= GetEditorColor(EEditorColors::ButtonTint);
	colors[ImGuiCol_ScrollbarGrabHovered]	= GetEditorColor(EEditorColors::ButtonHover);
	colors[ImGuiCol_ScrollbarGrabActive]	= GetEditorColor(EEditorColors::ButtonActive);
	//colors[ImGuiCol_SeparatorHovered]		= GetEditorColor(EEditorColors::Accent);
	//colors[ImGuiCol_SeparatorActive]		= GetEditorColor(EEditorColors::Accent);


	//style.ChildBorderSize = 0.0f;
	//style.PopupBorderSize = 0.0f;
	style.TabBorderSize = 0.0f;
	//style.WindowBorderSize					= 1.0f;
	style.WindowBorderSize					= 0.0f;
	style.ChildBorderSize					= 0.0f;

	style.WindowPadding						= { WindowPadding, WindowPadding };
	style.FrameBorderSize					= 0.0f;
	style.FramePadding						= { GetEditorSize(EEditorSizes::ButtonPaddingW), (GetEditorSize(EEditorSizes::ButtonSize) - GetEditorSize(EEditorSizes::FontSize)) / 2 };
	style.FrameRounding						= GetEditorSize(EEditorSizes::ButtonRadius);
	style.ChildRounding						= 0.0f;
	style.ItemSpacing						= ImVec2(4.f, 2.f);
	style.ItemInnerSpacing					= ImVec2(2.f, 2.f);
	style.DockingSeparatorSize				= GetEditorSize(EEditorSizes::DockingGap);
    style.IndentSpacing						= 8.0f;
    style.SeparatorTextBorderSize			= 2.0f;
    style.SeparatorTextBorderSize			= 2.0f;
	style.WindowMenuButtonPosition			= ImGuiDir_Right;

    style.CellPadding						= { GetEditorSize(ButtonTextPaddingY), GetEditorSize(TableTextPaddingY) };

	log_color_default = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	log_color_error = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	log_color_warning = ImVec4(1.00f, 1.00f, 0.00f, 1.00f);
	log_color_debug = ImVec4(0.50f, 0.50f, 0.50f, 1.00f);

	IsLoaded = true;
}

void CUIThemeManager::Show(bool value)
{
	bOpen = value;
}