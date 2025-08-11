#include "stdafx.h"
#include "xrUITheme.h"
#include "imgui_user.h"

#define _game_fonts_ "$game_fonts$"

#include <fstream>
#include <json/json.hpp>
using json = nlohmann::json;

void LoadImGuiFont(const char* Font);
extern xr_string ImCurrentFont;

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
		ImGui::Separator();

		ImGui::SeparatorText("Paddings");
		ImGui::PushItemWidth(150);

		ImGuiStyle& style = ImGui::GetStyle();
		ImGui::SliderFloat("Cell Padding X", &style.CellPadding.x, 0.f, 10.f, "%.1f");
		ImGui::SliderFloat("Cell Padding Y", &style.CellPadding.y, 0.f, 10.f, "%.1f");

		ImGui::Separator();

		if (ImGui::Button("Default"))
		{
			InitDefault(true);
		}
		ImGui::SameLine();
		if (ImGui::Button("Save to..."))
		{
			SaveTo();
		}
		ImGui::SameLine();
		if (ImGui::Button("Load from..."))
		{
			LoadFrom();
		}
	}

	ImGui::End();
}

void CUIThemeManager::InitDefault(bool Forced)
{
	ImVec4* colors = ImGui::GetStyle().Colors;

	Load();
	if (!Forced && IsLoaded)
	{
		IsLoaded = true;
		return;
	}

	XRay::ImGui::MakeEditorTheme();

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

#define FastJSonReadImColor(color) \
{ \
	if (!JSONData["Theme"].contains(#color))\
		return; \
	\
	float r = JSONData["Theme"][#color]["r"];\
	float g = JSONData["Theme"][#color]["g"];\
	float b = JSONData["Theme"][#color]["b"];\
	float a = JSONData["Theme"][#color]["a"];\
	\
	colors[color].x = r; \
	colors[color].y = g; \
	colors[color].z = b; \
	colors[color].w = a; \
} \

#define FastJSonReadImColorAlt(color) \
{ \
	if (!JSONData["Theme"].contains(#color))\
		return; \
	\
	float r = JSONData["Theme"][#color]["r"];\
	float g = JSONData["Theme"][#color]["g"];\
	float b = JSONData["Theme"][#color]["b"];\
	float a = JSONData["Theme"][#color]["a"];\
	\
	color.x = r; \
	color.y = g; \
	color.z = b; \
	color.w = a; \
} \

#define FastJSonWriteImColor(color) \
{ \
	JSONData["Theme"][#color]["r"] = colors[color].x;\
	JSONData["Theme"][#color]["g"] = colors[color].y;\
	JSONData["Theme"][#color]["b"] = colors[color].z;\
	JSONData["Theme"][#color]["a"] = colors[color].w;\
} \

#define FastJSonWriteImColorAlt(color) \
{ \
	JSONData["Theme"][#color]["r"] = color.x;\
	JSONData["Theme"][#color]["g"] = color.y;\
	JSONData["Theme"][#color]["b"] = color.z;\
	JSONData["Theme"][#color]["a"] = color.w;\
} \

void CUIThemeManager::Save()
{
	if (!IsLoaded)
		return;

	json JSONData = {};
	ImGuiStyle& style = ImGui::GetStyle();
	ImVec4* colors = style.Colors;
	// Original order with additions
	FastJSonWriteImColor(ImGuiCol_WindowBg);
	FastJSonWriteImColor(ImGuiCol_ChildBg);
	FastJSonWriteImColor(ImGuiCol_MenuBarBg);
	FastJSonWriteImColor(ImGuiCol_Text);
	FastJSonWriteImColor(ImGuiCol_TextDisabled);
	FastJSonWriteImColor(ImGuiCol_TextSelectedBg);
	FastJSonWriteImColor(ImGuiCol_TextLink);
	FastJSonWriteImColor(ImGuiCol_TableHeaderBg);
	FastJSonWriteImColor(ImGuiCol_TableBorderStrong);
	FastJSonWriteImColor(ImGuiCol_TableBorderLight);
	FastJSonWriteImColor(ImGuiCol_TableRowBg);
	FastJSonWriteImColor(ImGuiCol_TableRowBgAlt);
	FastJSonWriteImColor(ImGuiCol_FrameBg);
	FastJSonWriteImColor(ImGuiCol_FrameBgHovered);
	FastJSonWriteImColor(ImGuiCol_FrameBgActive);
	FastJSonWriteImColor(ImGuiCol_CheckMark);
	FastJSonWriteImColor(ImGuiCol_Border);
	FastJSonWriteImColor(ImGuiCol_BorderShadow);
	FastJSonWriteImColor(ImGuiCol_TitleBg);
	FastJSonWriteImColor(ImGuiCol_TitleBgActive);
	FastJSonWriteImColor(ImGuiCol_TitleBgCollapsed);
	FastJSonWriteImColor(ImGuiCol_Tab);
	FastJSonWriteImColor(ImGuiCol_TabHovered);
	FastJSonWriteImColor(ImGuiCol_TabActive);
	FastJSonWriteImColor(ImGuiCol_TabUnfocused);
	FastJSonWriteImColor(ImGuiCol_TabUnfocusedActive);
	FastJSonWriteImColor(ImGuiCol_TabSelected);
	FastJSonWriteImColor(ImGuiCol_TabSelectedOverline);
	FastJSonWriteImColor(ImGuiCol_TabDimmed);
	FastJSonWriteImColor(ImGuiCol_TabDimmedSelected);
	FastJSonWriteImColor(ImGuiCol_TabDimmedSelectedOverline);
	FastJSonWriteImColor(ImGuiCol_Button);
	FastJSonWriteImColor(ImGuiCol_ButtonHovered);
	FastJSonWriteImColor(ImGuiCol_ButtonActive);
	FastJSonWriteImColor(ImGuiCol_Header);
	FastJSonWriteImColor(ImGuiCol_HeaderHovered);
	FastJSonWriteImColor(ImGuiCol_HeaderActive);
	FastJSonWriteImColor(ImGuiCol_PopupBg);
	FastJSonWriteImColor(ImGuiCol_Separator);
	FastJSonWriteImColor(ImGuiCol_SeparatorHovered);
	FastJSonWriteImColor(ImGuiCol_SeparatorActive);
	FastJSonWriteImColor(ImGuiCol_ResizeGrip);
	FastJSonWriteImColor(ImGuiCol_ResizeGripHovered);
	FastJSonWriteImColor(ImGuiCol_ResizeGripActive);
	FastJSonWriteImColor(ImGuiCol_ScrollbarBg);
	FastJSonWriteImColor(ImGuiCol_ScrollbarGrab);
	FastJSonWriteImColor(ImGuiCol_ScrollbarGrabHovered);
	FastJSonWriteImColor(ImGuiCol_ScrollbarGrabActive);
	FastJSonWriteImColor(ImGuiCol_SliderGrab);
	FastJSonWriteImColor(ImGuiCol_SliderGrabActive);
	FastJSonWriteImColor(ImGuiCol_InputTextCursor);
	FastJSonWriteImColor(ImGuiCol_DockingPreview);
	FastJSonWriteImColor(ImGuiCol_DockingEmptyBg);
	FastJSonWriteImColor(ImGuiCol_PlotLines);
	FastJSonWriteImColor(ImGuiCol_PlotLinesHovered);
	FastJSonWriteImColor(ImGuiCol_PlotHistogram);
	FastJSonWriteImColor(ImGuiCol_PlotHistogramHovered);
	FastJSonWriteImColor(ImGuiCol_TreeLines);
	FastJSonWriteImColor(ImGuiCol_DragDropTarget);
	FastJSonWriteImColor(ImGuiCol_NavCursor);
	FastJSonWriteImColor(ImGuiCol_NavWindowingHighlight);
	FastJSonWriteImColor(ImGuiCol_NavWindowingDimBg);
	FastJSonWriteImColor(ImGuiCol_ModalWindowDimBg);
	FastJSonWriteImColorAlt(log_color_default);
	FastJSonWriteImColorAlt(log_color_error);
	FastJSonWriteImColorAlt(log_color_warning);
	FastJSonWriteImColorAlt(log_color_debug);

	JSONData["Theme"]["InactiveAlpha"] = TransparentDefault;
	JSONData["Theme"]["ActiveAlpha"] = TransparentUnfocused;
	JSONData["Theme"]["Font"] = ImCurrentFont;
	JSONData["Theme"]["CellPadding"] = { style.CellPadding.x, style.CellPadding.y };

	string_path jfn;
	FS.update_path(jfn, "$app_data_root$", EFS.ChangeFileExt("editor_theme", ".json").c_str());

	std::ofstream o(jfn);
	o << JSONData;
}


void CUIThemeManager::SaveTo()
{
	if (!IsLoaded)
		return;

	xr_string jfn;
	if (EFS.GetSaveName("$themes$", jfn, 0, 6, "*.json"))
	{
		json JSONData = {};
		ImGuiStyle& style = ImGui::GetStyle();
		ImVec4* colors = style.Colors;
		// Original order with additions
		FastJSonWriteImColor(ImGuiCol_WindowBg);
		FastJSonWriteImColor(ImGuiCol_ChildBg);
		FastJSonWriteImColor(ImGuiCol_MenuBarBg);
		FastJSonWriteImColor(ImGuiCol_Text);
		FastJSonWriteImColor(ImGuiCol_TextDisabled);
		FastJSonWriteImColor(ImGuiCol_TextSelectedBg);
		FastJSonWriteImColor(ImGuiCol_TextLink);
		FastJSonWriteImColor(ImGuiCol_TableHeaderBg);
		FastJSonWriteImColor(ImGuiCol_TableBorderStrong);
		FastJSonWriteImColor(ImGuiCol_TableBorderLight);
		FastJSonWriteImColor(ImGuiCol_TableRowBg);
		FastJSonWriteImColor(ImGuiCol_TableRowBgAlt);
		FastJSonWriteImColor(ImGuiCol_FrameBg);
		FastJSonWriteImColor(ImGuiCol_FrameBgHovered);
		FastJSonWriteImColor(ImGuiCol_FrameBgActive);
		FastJSonWriteImColor(ImGuiCol_CheckMark);
		FastJSonWriteImColor(ImGuiCol_Border);
		FastJSonWriteImColor(ImGuiCol_BorderShadow);
		FastJSonWriteImColor(ImGuiCol_TitleBg);
		FastJSonWriteImColor(ImGuiCol_TitleBgActive);
		FastJSonWriteImColor(ImGuiCol_TitleBgCollapsed);
		FastJSonWriteImColor(ImGuiCol_Tab);
		FastJSonWriteImColor(ImGuiCol_TabHovered);
		FastJSonWriteImColor(ImGuiCol_TabActive);
		FastJSonWriteImColor(ImGuiCol_TabUnfocused);
		FastJSonWriteImColor(ImGuiCol_TabUnfocusedActive);
		FastJSonWriteImColor(ImGuiCol_TabSelected);
		FastJSonWriteImColor(ImGuiCol_TabSelectedOverline);
		FastJSonWriteImColor(ImGuiCol_TabDimmed);
		FastJSonWriteImColor(ImGuiCol_TabDimmedSelected);
		FastJSonWriteImColor(ImGuiCol_TabDimmedSelectedOverline);
		FastJSonWriteImColor(ImGuiCol_Button);
		FastJSonWriteImColor(ImGuiCol_ButtonHovered);
		FastJSonWriteImColor(ImGuiCol_ButtonActive);
		FastJSonWriteImColor(ImGuiCol_Header);
		FastJSonWriteImColor(ImGuiCol_HeaderHovered);
		FastJSonWriteImColor(ImGuiCol_HeaderActive);
		FastJSonWriteImColor(ImGuiCol_PopupBg);
		FastJSonWriteImColor(ImGuiCol_Separator);
		FastJSonWriteImColor(ImGuiCol_SeparatorHovered);
		FastJSonWriteImColor(ImGuiCol_SeparatorActive);
		FastJSonWriteImColor(ImGuiCol_ResizeGrip);
		FastJSonWriteImColor(ImGuiCol_ResizeGripHovered);
		FastJSonWriteImColor(ImGuiCol_ResizeGripActive);
		FastJSonWriteImColor(ImGuiCol_ScrollbarBg);
		FastJSonWriteImColor(ImGuiCol_ScrollbarGrab);
		FastJSonWriteImColor(ImGuiCol_ScrollbarGrabHovered);
		FastJSonWriteImColor(ImGuiCol_ScrollbarGrabActive);
		FastJSonWriteImColor(ImGuiCol_SliderGrab);
		FastJSonWriteImColor(ImGuiCol_SliderGrabActive);
		FastJSonWriteImColor(ImGuiCol_InputTextCursor);
		FastJSonWriteImColor(ImGuiCol_DockingPreview);
		FastJSonWriteImColor(ImGuiCol_DockingEmptyBg);
		FastJSonWriteImColor(ImGuiCol_PlotLines);
		FastJSonWriteImColor(ImGuiCol_PlotLinesHovered);
		FastJSonWriteImColor(ImGuiCol_PlotHistogram);
		FastJSonWriteImColor(ImGuiCol_PlotHistogramHovered);
		FastJSonWriteImColor(ImGuiCol_TreeLines);
		FastJSonWriteImColor(ImGuiCol_DragDropTarget);
		FastJSonWriteImColor(ImGuiCol_NavCursor);
		FastJSonWriteImColor(ImGuiCol_NavWindowingHighlight);
		FastJSonWriteImColor(ImGuiCol_NavWindowingDimBg);
		FastJSonWriteImColor(ImGuiCol_ModalWindowDimBg);
		FastJSonWriteImColorAlt(log_color_default);
		FastJSonWriteImColorAlt(log_color_error);
		FastJSonWriteImColorAlt(log_color_warning);
		FastJSonWriteImColorAlt(log_color_debug);

		JSONData["Theme"]["InactiveAlpha"] = TransparentDefault;
		JSONData["Theme"]["ActiveAlpha"] = TransparentUnfocused;
		JSONData["Theme"]["Font"] = ImCurrentFont;
		JSONData["Theme"]["CellPadding"] = { style.CellPadding.x, style.CellPadding.y };


		std::ofstream o(jfn.c_str());
		o << JSONData;
	}
}


void CUIThemeManager::LoadFrom()
{
	json JSONData = {};
	xr_string jfn;
	if (EFS.GetOpenName("$themes$", jfn, false, NULL, 6, "*.json"))
	{
		std::ifstream f(jfn.c_str());
		f >> JSONData;

		if (!JSONData.contains("Theme"))
		{
			return;
		}

		ImGuiStyle& style = ImGui::GetStyle();
		ImVec4* colors = style.Colors;

		FastJSonReadImColor(ImGuiCol_WindowBg);
		FastJSonReadImColor(ImGuiCol_ChildBg);
		FastJSonReadImColor(ImGuiCol_PopupBg);
		FastJSonReadImColor(ImGuiCol_MenuBarBg);
		FastJSonReadImColor(ImGuiCol_Text);
		FastJSonReadImColor(ImGuiCol_TextDisabled);
		FastJSonReadImColor(ImGuiCol_TextSelectedBg);
		FastJSonReadImColor(ImGuiCol_TextLink);
		FastJSonReadImColor(ImGuiCol_FrameBg);
		FastJSonReadImColor(ImGuiCol_FrameBgHovered);
		FastJSonReadImColor(ImGuiCol_FrameBgActive);
		FastJSonReadImColor(ImGuiCol_Border);
		FastJSonReadImColor(ImGuiCol_BorderShadow);
		FastJSonReadImColor(ImGuiCol_TitleBg);
		FastJSonReadImColor(ImGuiCol_TitleBgActive);
		FastJSonReadImColor(ImGuiCol_TitleBgCollapsed);
		FastJSonReadImColor(ImGuiCol_ScrollbarBg);
		FastJSonReadImColor(ImGuiCol_ScrollbarGrab);
		FastJSonReadImColor(ImGuiCol_ScrollbarGrabHovered);
		FastJSonReadImColor(ImGuiCol_ScrollbarGrabActive);
		FastJSonReadImColor(ImGuiCol_SliderGrab);
		FastJSonReadImColor(ImGuiCol_SliderGrabActive);
		FastJSonReadImColor(ImGuiCol_CheckMark);
		FastJSonReadImColor(ImGuiCol_Button);
		FastJSonReadImColor(ImGuiCol_ButtonHovered);
		FastJSonReadImColor(ImGuiCol_ButtonActive);
		FastJSonReadImColor(ImGuiCol_Header);
		FastJSonReadImColor(ImGuiCol_HeaderHovered);
		FastJSonReadImColor(ImGuiCol_HeaderActive);
		FastJSonReadImColor(ImGuiCol_Separator);
		FastJSonReadImColor(ImGuiCol_SeparatorHovered);
		FastJSonReadImColor(ImGuiCol_SeparatorActive);
		FastJSonReadImColor(ImGuiCol_ResizeGrip);
		FastJSonReadImColor(ImGuiCol_ResizeGripHovered);
		FastJSonReadImColor(ImGuiCol_ResizeGripActive);
		FastJSonReadImColor(ImGuiCol_InputTextCursor);
		FastJSonReadImColor(ImGuiCol_Tab);
		FastJSonReadImColor(ImGuiCol_TabHovered);
		FastJSonReadImColor(ImGuiCol_TabActive);
		FastJSonReadImColor(ImGuiCol_TabUnfocused);
		FastJSonReadImColor(ImGuiCol_TabUnfocusedActive);
		FastJSonReadImColor(ImGuiCol_TabSelected);
		FastJSonReadImColor(ImGuiCol_TabSelectedOverline);
		FastJSonReadImColor(ImGuiCol_TabDimmed);
		FastJSonReadImColor(ImGuiCol_TabDimmedSelected);
		FastJSonReadImColor(ImGuiCol_TabDimmedSelectedOverline);
		FastJSonReadImColor(ImGuiCol_DockingPreview);
		FastJSonReadImColor(ImGuiCol_DockingEmptyBg);
		FastJSonReadImColor(ImGuiCol_TableHeaderBg);
		FastJSonReadImColor(ImGuiCol_TableBorderStrong);
		FastJSonReadImColor(ImGuiCol_TableBorderLight);
		FastJSonReadImColor(ImGuiCol_TableRowBg);
		FastJSonReadImColor(ImGuiCol_TableRowBgAlt);
		FastJSonReadImColor(ImGuiCol_PlotLines);
		FastJSonReadImColor(ImGuiCol_PlotLinesHovered);
		FastJSonReadImColor(ImGuiCol_PlotHistogram);
		FastJSonReadImColor(ImGuiCol_PlotHistogramHovered);
		FastJSonReadImColor(ImGuiCol_TreeLines);
		FastJSonReadImColor(ImGuiCol_DragDropTarget);
		FastJSonReadImColor(ImGuiCol_NavCursor);
		FastJSonReadImColor(ImGuiCol_NavWindowingHighlight);
		FastJSonReadImColor(ImGuiCol_NavWindowingDimBg);
		FastJSonReadImColor(ImGuiCol_ModalWindowDimBg);
		FastJSonReadImColorAlt(log_color_default);
		FastJSonReadImColorAlt(log_color_error);
		FastJSonReadImColorAlt(log_color_warning);
		FastJSonReadImColorAlt(log_color_debug);


		if (JSONData["Theme"].contains("InactiveAlpha"))
		{
			TransparentDefault = JSONData["Theme"]["InactiveAlpha"];
		}

		if (JSONData["Theme"].contains("ActiveAlpha"))
		{
			TransparentUnfocused = JSONData["Theme"]["ActiveAlpha"];
		}

		if (JSONData["Theme"].contains("Font"))
		{
			ImCurrentFont = JSONData["Theme"]["Font"];
		}
		
		if (JSONData["Theme"].contains("CellPadding"))
		{
			style.CellPadding.x = JSONData["Theme"]["CellPadding"][0];
			style.CellPadding.y = JSONData["Theme"]["CellPadding"][1];
		}

		IsLoaded = true;
	}
}

void CUIThemeManager::Load()
{
	json JSONData = {};
	string_path jfn;
	FS.update_path(jfn, "$app_data_root$", EFS.ChangeFileExt("editor_theme", ".json").c_str());

	if (std::filesystem::exists(jfn))
	{
		std::ifstream f(jfn);
		f >> JSONData;
	}

	if (!JSONData.contains("Theme"))
	{
		return;
	}

	ImGuiStyle& style = ImGui::GetStyle();
	ImVec4* colors = style.Colors;

	FastJSonReadImColor(ImGuiCol_WindowBg);
	FastJSonReadImColor(ImGuiCol_ChildBg);
	FastJSonReadImColor(ImGuiCol_PopupBg);
	FastJSonReadImColor(ImGuiCol_MenuBarBg);
	FastJSonReadImColor(ImGuiCol_Text);
	FastJSonReadImColor(ImGuiCol_TextDisabled);
	FastJSonReadImColor(ImGuiCol_TextSelectedBg);
	FastJSonReadImColor(ImGuiCol_TextLink);
	FastJSonReadImColor(ImGuiCol_FrameBg);
	FastJSonReadImColor(ImGuiCol_FrameBgHovered);
	FastJSonReadImColor(ImGuiCol_FrameBgActive);
	FastJSonReadImColor(ImGuiCol_Border);
	FastJSonReadImColor(ImGuiCol_BorderShadow);
	FastJSonReadImColor(ImGuiCol_TitleBg);
	FastJSonReadImColor(ImGuiCol_TitleBgActive);
	FastJSonReadImColor(ImGuiCol_TitleBgCollapsed);
	FastJSonReadImColor(ImGuiCol_ScrollbarBg);
	FastJSonReadImColor(ImGuiCol_ScrollbarGrab);
	FastJSonReadImColor(ImGuiCol_ScrollbarGrabHovered);
	FastJSonReadImColor(ImGuiCol_ScrollbarGrabActive);
	FastJSonReadImColor(ImGuiCol_SliderGrab);
	FastJSonReadImColor(ImGuiCol_SliderGrabActive);
	FastJSonReadImColor(ImGuiCol_CheckMark);
	FastJSonReadImColor(ImGuiCol_Button);
	FastJSonReadImColor(ImGuiCol_ButtonHovered);
	FastJSonReadImColor(ImGuiCol_ButtonActive);
	FastJSonReadImColor(ImGuiCol_Header);
	FastJSonReadImColor(ImGuiCol_HeaderHovered);
	FastJSonReadImColor(ImGuiCol_HeaderActive);
	FastJSonReadImColor(ImGuiCol_Separator);
	FastJSonReadImColor(ImGuiCol_SeparatorHovered);
	FastJSonReadImColor(ImGuiCol_SeparatorActive);
	FastJSonReadImColor(ImGuiCol_ResizeGrip);
	FastJSonReadImColor(ImGuiCol_ResizeGripHovered);
	FastJSonReadImColor(ImGuiCol_ResizeGripActive);
	FastJSonReadImColor(ImGuiCol_InputTextCursor);
	FastJSonReadImColor(ImGuiCol_Tab);
	FastJSonReadImColor(ImGuiCol_TabHovered);
	FastJSonReadImColor(ImGuiCol_TabActive);
	FastJSonReadImColor(ImGuiCol_TabUnfocused);
	FastJSonReadImColor(ImGuiCol_TabUnfocusedActive);
	FastJSonReadImColor(ImGuiCol_TabSelected);
	FastJSonReadImColor(ImGuiCol_TabSelectedOverline);
	FastJSonReadImColor(ImGuiCol_TabDimmed);
	FastJSonReadImColor(ImGuiCol_TabDimmedSelected);
	FastJSonReadImColor(ImGuiCol_TabDimmedSelectedOverline);
	FastJSonReadImColor(ImGuiCol_DockingPreview);
	FastJSonReadImColor(ImGuiCol_DockingEmptyBg);
	FastJSonReadImColor(ImGuiCol_TableHeaderBg);
	FastJSonReadImColor(ImGuiCol_TableBorderStrong);
	FastJSonReadImColor(ImGuiCol_TableBorderLight);
	FastJSonReadImColor(ImGuiCol_TableRowBg);
	FastJSonReadImColor(ImGuiCol_TableRowBgAlt);
	FastJSonReadImColor(ImGuiCol_PlotLines);
	FastJSonReadImColor(ImGuiCol_PlotLinesHovered);
	FastJSonReadImColor(ImGuiCol_PlotHistogram);
	FastJSonReadImColor(ImGuiCol_PlotHistogramHovered);
	FastJSonReadImColor(ImGuiCol_TreeLines);
	FastJSonReadImColor(ImGuiCol_DragDropTarget);
	FastJSonReadImColor(ImGuiCol_NavCursor);
	FastJSonReadImColor(ImGuiCol_NavWindowingHighlight);
	FastJSonReadImColor(ImGuiCol_NavWindowingDimBg);
	FastJSonReadImColor(ImGuiCol_ModalWindowDimBg);
	FastJSonReadImColorAlt(log_color_default);
	FastJSonReadImColorAlt(log_color_error);
	FastJSonReadImColorAlt(log_color_warning);
	FastJSonReadImColorAlt(log_color_debug);

	if (JSONData["Theme"].contains("InactiveAlpha"))
	{
		TransparentDefault = JSONData["Theme"]["InactiveAlpha"];
	}

	if (JSONData["Theme"].contains("ActiveAlpha"))
	{
		TransparentUnfocused = JSONData["Theme"]["ActiveAlpha"];
	}

	if (JSONData["Theme"].contains("Font"))
	{
		ImCurrentFont = JSONData["Theme"]["Font"];
	}

	if (JSONData["Theme"].contains("CellPadding"))
	{
		style.CellPadding.x = JSONData["Theme"]["CellPadding"][0];
		style.CellPadding.y = JSONData["Theme"]["CellPadding"][1];
	}

	IsLoaded = true;
}

#undef FastJSonReadImColor
#undef FastJSonWriteImColor