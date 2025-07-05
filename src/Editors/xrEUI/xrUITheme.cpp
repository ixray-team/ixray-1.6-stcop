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

		ImGui::SeparatorText("General");
		ImGui::ColorEdit4("Default Color", (float*)&colors[ImGuiCol_WindowBg]);
		ImGui::ColorEdit4("Header Color", (float*)&colors[ImGuiCol_MenuBarBg]);

		ImGui::SeparatorText("Text");
		ImGui::ColorEdit4("Text Color", (float*)&colors[ImGuiCol_Text]);
		ImGui::ColorEdit4("Text Disabled Color", (float*)&colors[ImGuiCol_TextDisabled]);
		ImGui::ColorEdit4("Text Selected Background", (float*)&colors[ImGuiCol_TextSelectedBg]);

		ImGui::SeparatorText("Properties");
		ImGui::ColorEdit4("Header Color ##", (float*)&colors[ImGuiCol_TableHeaderBg]);
		ImGui::ColorEdit4("Header Border Color", (float*)&colors[ImGuiCol_TableBorderStrong]);
		ImGui::ColorEdit4("Row Color", (float*)&colors[ImGuiCol_TableRowBgAlt]);
		ImGui::ColorEdit4("Row Color 2", (float*)&colors[ImGuiCol_TableRowBg]);
		ImGui::ColorEdit4("Row Border Color", (float*)&colors[ImGuiCol_TableBorderLight]);

		ImGui::SeparatorText("Items");
		ImGui::ColorEdit4("Item Color", (float*)&colors[ImGuiCol_FrameBg]);
		ImGui::ColorEdit4("CheckItem Color", (float*)&colors[ImGuiCol_CheckMark]);
		ImGui::ColorEdit4("Item Border Color", (float*)&colors[ImGuiCol_Border]);
		ImGui::ColorEdit4("Title Color", (float*)&colors[ImGuiCol_TitleBg]);
		ImGui::ColorEdit4("Active title Color", (float*)&colors[ImGuiCol_TitleBgActive]);

		ImGui::SeparatorText("Buttons");
		ImGui::ColorEdit4("Button", (float*)&colors[ImGuiCol_Button]);
		ImGui::ColorEdit4("Button Hovered", (float*)&colors[ImGuiCol_ButtonHovered]);
		ImGui::ColorEdit4("Button Active", (float*)&colors[ImGuiCol_ButtonActive]);
			;
		ImGui::SeparatorText("Tabs");
		ImGui::ColorEdit4("Tab Hovered", (float*)&colors[ImGuiCol_TabHovered]);
		ImGui::ColorEdit4("Tab Unfocused", (float*)&colors[ImGuiCol_TabUnfocused]);
		ImGui::ColorEdit4("Tab Active", (float*)&colors[ImGuiCol_TabActive]);
		ImGui::ColorEdit4("Tab Active Unfocused", (float*)&colors[ImGuiCol_TabUnfocusedActive]);

		ImGui::SeparatorText("Context");
		ImGui::ColorEdit4("Context Header", (float*)&colors[ImGuiCol_Header]);
		ImGui::ColorEdit4("Context Hovered", (float*)&colors[ImGuiCol_HeaderHovered]);
		ImGui::ColorEdit4("Context PopupBg", (float*)&colors[ImGuiCol_PopupBg]);

		ImGui::SeparatorText("Log");
		ImGui::ColorEdit4("Error message", (float*)&log_color_error);
		ImGui::ColorEdit4("Warning message", (float*)&log_color_warning);
		ImGui::ColorEdit4("Debug message", (float*)&log_color_debug);
		ImGui::ColorEdit4("Default message", (float*)&log_color_default);

		ImGui::SeparatorText("Plot");
		ImGui::ColorEdit4("Plot Lines", (float*)&colors[ImGuiCol_PlotLines]);
		ImGui::ColorEdit4("Plot Lines Hovered", (float*)&colors[ImGuiCol_PlotLinesHovered]);
		ImGui::ColorEdit4("Plot Histogram", (float*)&colors[ImGuiCol_PlotHistogram]);
		ImGui::ColorEdit4("Plot Histogram Hovered", (float*)&colors[ImGuiCol_PlotHistogramHovered]);

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

	FastJSonWriteImColor(ImGuiCol_WindowBg);
	FastJSonWriteImColor(ImGuiCol_MenuBarBg);
	FastJSonWriteImColor(ImGuiCol_Text);
	FastJSonWriteImColor(ImGuiCol_TableHeaderBg);
	FastJSonWriteImColor(ImGuiCol_TableBorderStrong);
	FastJSonWriteImColor(ImGuiCol_TableBorderLight);
	FastJSonWriteImColor(ImGuiCol_TableRowBg);
	FastJSonWriteImColor(ImGuiCol_TableRowBgAlt);
	FastJSonWriteImColor(ImGuiCol_FrameBg);
	FastJSonWriteImColor(ImGuiCol_CheckMark);
	FastJSonWriteImColor(ImGuiCol_Border);
	FastJSonWriteImColor(ImGuiCol_TitleBg);
	FastJSonWriteImColor(ImGuiCol_TabUnfocusedActive);
	FastJSonWriteImColor(ImGuiCol_TabUnfocused);
	FastJSonWriteImColor(ImGuiCol_TabActive);
	FastJSonWriteImColor(ImGuiCol_TabHovered);
	FastJSonWriteImColor(ImGuiCol_TitleBgActive);
	FastJSonWriteImColor(ImGuiCol_ButtonHovered);
	FastJSonWriteImColor(ImGuiCol_ButtonActive);
	FastJSonWriteImColor(ImGuiCol_Button);
	FastJSonWriteImColor(ImGuiCol_Header);
	FastJSonWriteImColor(ImGuiCol_HeaderHovered);
	FastJSonWriteImColor(ImGuiCol_PopupBg);
	FastJSonWriteImColorAlt(log_color_default);
	FastJSonWriteImColorAlt(log_color_error);
	FastJSonWriteImColorAlt(log_color_warning);
	FastJSonWriteImColorAlt(log_color_debug);
	FastJSonWriteImColor(ImGuiCol_TextDisabled);
	FastJSonWriteImColor(ImGuiCol_TextSelectedBg);
	FastJSonWriteImColor(ImGuiCol_PlotLines);
	FastJSonWriteImColor(ImGuiCol_PlotLinesHovered);
	FastJSonWriteImColor(ImGuiCol_PlotHistogram);
	FastJSonWriteImColor(ImGuiCol_PlotHistogramHovered);

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

		FastJSonWriteImColor(ImGuiCol_WindowBg);
		FastJSonWriteImColor(ImGuiCol_MenuBarBg);
		FastJSonWriteImColor(ImGuiCol_Text);
		FastJSonWriteImColor(ImGuiCol_TableHeaderBg);
		FastJSonWriteImColor(ImGuiCol_TableBorderStrong);
		FastJSonWriteImColor(ImGuiCol_TableBorderLight);
		FastJSonWriteImColor(ImGuiCol_TableRowBg);
		FastJSonWriteImColor(ImGuiCol_TableRowBgAlt);
		FastJSonWriteImColor(ImGuiCol_FrameBg);
		FastJSonWriteImColor(ImGuiCol_CheckMark);
		FastJSonWriteImColor(ImGuiCol_Border);
		FastJSonWriteImColor(ImGuiCol_TitleBg);
		FastJSonWriteImColor(ImGuiCol_TabUnfocusedActive);
		FastJSonWriteImColor(ImGuiCol_TabUnfocused);
		FastJSonWriteImColor(ImGuiCol_TabActive);
		FastJSonWriteImColor(ImGuiCol_TabHovered);
		FastJSonWriteImColor(ImGuiCol_TitleBgActive);
		FastJSonWriteImColor(ImGuiCol_ButtonHovered);
		FastJSonWriteImColor(ImGuiCol_ButtonActive);
		FastJSonWriteImColor(ImGuiCol_Button);
		FastJSonWriteImColor(ImGuiCol_Header);
		FastJSonWriteImColor(ImGuiCol_HeaderHovered);
		FastJSonWriteImColor(ImGuiCol_PopupBg);
		FastJSonWriteImColorAlt(log_color_default);
		FastJSonWriteImColorAlt(log_color_error);
		FastJSonWriteImColorAlt(log_color_warning);
		FastJSonWriteImColorAlt(log_color_debug);
		FastJSonWriteImColor(ImGuiCol_TextDisabled);
		FastJSonWriteImColor(ImGuiCol_TextSelectedBg);
		FastJSonWriteImColor(ImGuiCol_PlotLines);
		FastJSonWriteImColor(ImGuiCol_PlotLinesHovered);
		FastJSonWriteImColor(ImGuiCol_PlotHistogram);
		FastJSonWriteImColor(ImGuiCol_PlotHistogramHovered);

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
		FastJSonReadImColor(ImGuiCol_MenuBarBg);
		FastJSonReadImColor(ImGuiCol_Text);
		FastJSonReadImColor(ImGuiCol_TableHeaderBg);
		FastJSonReadImColor(ImGuiCol_TableBorderStrong);
		FastJSonReadImColor(ImGuiCol_TableBorderLight);
		FastJSonReadImColor(ImGuiCol_TableRowBg);
		FastJSonReadImColor(ImGuiCol_TableRowBgAlt);
		FastJSonReadImColor(ImGuiCol_FrameBg);
		FastJSonReadImColor(ImGuiCol_CheckMark);
		FastJSonReadImColor(ImGuiCol_Border);
		FastJSonReadImColor(ImGuiCol_TitleBg);
		FastJSonReadImColor(ImGuiCol_TabUnfocusedActive);
		FastJSonReadImColor(ImGuiCol_TabUnfocused);
		FastJSonReadImColor(ImGuiCol_TabActive);
		FastJSonReadImColor(ImGuiCol_TabHovered);
		FastJSonReadImColor(ImGuiCol_TitleBgActive);
		FastJSonReadImColor(ImGuiCol_ButtonHovered);
		FastJSonReadImColor(ImGuiCol_ButtonActive);
		FastJSonReadImColor(ImGuiCol_Button);
		FastJSonReadImColor(ImGuiCol_Header);
		FastJSonReadImColor(ImGuiCol_HeaderHovered);
		FastJSonReadImColor(ImGuiCol_PopupBg);
		FastJSonReadImColorAlt(log_color_default);
		FastJSonReadImColorAlt(log_color_error);
		FastJSonReadImColorAlt(log_color_warning);
		FastJSonReadImColorAlt(log_color_debug);
		FastJSonReadImColor(ImGuiCol_TextDisabled);
		FastJSonReadImColor(ImGuiCol_TextSelectedBg);
		FastJSonReadImColor(ImGuiCol_PlotLines);
		FastJSonReadImColor(ImGuiCol_PlotLinesHovered);
		FastJSonReadImColor(ImGuiCol_PlotHistogram);
		FastJSonReadImColor(ImGuiCol_PlotHistogramHovered);


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
	FastJSonReadImColor(ImGuiCol_MenuBarBg);
	FastJSonReadImColor(ImGuiCol_Text);
	FastJSonReadImColor(ImGuiCol_TableHeaderBg);
	FastJSonReadImColor(ImGuiCol_TableBorderStrong);
	FastJSonReadImColor(ImGuiCol_TableBorderLight);
	FastJSonReadImColor(ImGuiCol_TableRowBg);
	FastJSonReadImColor(ImGuiCol_TableRowBgAlt);
	FastJSonReadImColor(ImGuiCol_FrameBg);
	FastJSonReadImColor(ImGuiCol_CheckMark);
	FastJSonReadImColor(ImGuiCol_Border);
	FastJSonReadImColor(ImGuiCol_TitleBg);
	FastJSonReadImColor(ImGuiCol_TabUnfocusedActive);
	FastJSonReadImColor(ImGuiCol_TabUnfocused);
	FastJSonReadImColor(ImGuiCol_TabActive);
	FastJSonReadImColor(ImGuiCol_TabHovered);
	FastJSonReadImColor(ImGuiCol_TitleBgActive);
	FastJSonReadImColor(ImGuiCol_ButtonHovered);
	FastJSonReadImColor(ImGuiCol_ButtonActive);
	FastJSonReadImColor(ImGuiCol_Button);
	FastJSonReadImColor(ImGuiCol_Header);
	FastJSonReadImColor(ImGuiCol_HeaderHovered);
	FastJSonReadImColor(ImGuiCol_PopupBg);
	FastJSonReadImColorAlt(log_color_default);
	FastJSonReadImColorAlt(log_color_error);
	FastJSonReadImColorAlt(log_color_warning);
	FastJSonReadImColorAlt(log_color_debug);
	FastJSonReadImColor(ImGuiCol_TextDisabled);
	FastJSonReadImColor(ImGuiCol_TextSelectedBg);
	FastJSonReadImColor(ImGuiCol_PlotLines);
	FastJSonReadImColor(ImGuiCol_PlotLinesHovered);
	FastJSonReadImColor(ImGuiCol_PlotHistogram);
	FastJSonReadImColor(ImGuiCol_PlotHistogramHovered);

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