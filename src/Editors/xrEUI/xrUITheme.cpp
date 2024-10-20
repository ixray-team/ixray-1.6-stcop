#include "stdafx.h"
#include "xrUITheme.h"

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
		ImGui::ColorEdit4("Text Color", (float*)&colors[ImGuiCol_Text]);

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

	colors[ImGuiCol_TextDisabled] = ImVec4(0.50f, 0.50f, 0.50f, 1.00f);
	colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_BorderShadow] = ImVec4(0.00f, 0.00f, 0.00f, 0.24f);
	colors[ImGuiCol_FrameBgHovered] = ImVec4(0.19f, 0.19f, 0.19f, 0.54f);
	colors[ImGuiCol_FrameBgActive] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_ScrollbarBg] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.34f, 0.34f, 0.34f, 0.54f);
	colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.40f, 0.40f, 0.40f, 0.54f);
	colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_SliderGrab] = ImVec4(0.34f, 0.34f, 0.34f, 0.54f);
	colors[ImGuiCol_SliderGrabActive] = ImVec4(0.56f, 0.56f, 0.56f, 0.54f);
	colors[ImGuiCol_HeaderActive] = ImVec4(0.20f, 0.22f, 0.23f, 0.33f);
	colors[ImGuiCol_Separator] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_SeparatorHovered] = ImVec4(0.44f, 0.44f, 0.44f, 0.29f);
	colors[ImGuiCol_SeparatorActive] = ImVec4(0.40f, 0.44f, 0.47f, 1.00f);
	colors[ImGuiCol_ResizeGrip] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_ResizeGripHovered] = ImVec4(0.44f, 0.44f, 0.44f, 0.29f);
	colors[ImGuiCol_ResizeGripActive] = ImVec4(0.40f, 0.44f, 0.47f, 1.00f);
	colors[ImGuiCol_Tab] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_DockingPreview] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_DockingEmptyBg] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotLines] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotLinesHovered] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogram] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_TextSelectedBg] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_DragDropTarget] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_NavHighlight] = ImVec4(1.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_NavWindowingHighlight] = ImVec4(1.00f, 0.00f, 0.00f, 0.70f);
	colors[ImGuiCol_NavWindowingDimBg] = ImVec4(1.00f, 0.00f, 0.00f, 0.20f);
	colors[ImGuiCol_ModalWindowDimBg] = ImVec4(1.00f, 0.00f, 0.00f, 0.35f);

	ImGuiStyle& style = ImGui::GetStyle();
	style.WindowPadding = ImVec2(8.00f, 8.00f);
	style.FramePadding = ImVec2(5.00f, 2.00f);
	style.CellPadding = ImVec2(6.00f, 6.00f);
	style.ItemSpacing = ImVec2(6.00f, 6.00f);
	style.ItemInnerSpacing = ImVec2(6.00f, 6.00f);
	style.TouchExtraPadding = ImVec2(0.00f, 0.00f);
	style.IndentSpacing = 25;
	style.ScrollbarSize = 15;
	style.GrabMinSize = 10;
	style.WindowBorderSize = 1;
	style.ChildBorderSize = 1;
	style.PopupBorderSize = 1;
	style.FrameBorderSize = 1;
	style.TabBorderSize = 1;
	style.WindowRounding = 7;
	style.ChildRounding = 4;
	style.FrameRounding = 3;
	style.PopupRounding = 4;
	style.ScrollbarRounding = 9;
	style.GrabRounding = 3;
	style.LogSliderDeadzone = 4;
	style.TabRounding = 4;

	Load();
	if (!Forced && IsLoaded)
	{
		IsLoaded = true;
		return;
	}

	colors[ImGuiCol_WindowBg] = ImVec4(0.10f, 0.10f, 0.10f, 1.00f);
	colors[ImGuiCol_MenuBarBg] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
	colors[ImGuiCol_TableHeaderBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TableBorderStrong] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TableBorderLight] = ImVec4(0.28f, 0.28f, 0.28f, 0.29f);
	colors[ImGuiCol_TableRowBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
	colors[ImGuiCol_TableRowBgAlt] = ImVec4(1.00f, 1.00f, 1.00f, 0.06f);
	colors[ImGuiCol_FrameBg] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_CheckMark] = ImVec4(0.33f, 0.67f, 0.86f, 1.00f);
	colors[ImGuiCol_Border] = ImVec4(0.19f, 0.19f, 0.19f, 0.29f);
	colors[ImGuiCol_TitleBg] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
	colors[ImGuiCol_TabHovered] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_TabActive] = ImVec4(0.20f, 0.20f, 0.20f, 0.36f);
	colors[ImGuiCol_TabUnfocused] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_TabUnfocusedActive] = ImVec4(0.14f, 0.14f, 0.14f, 1.00f);
	colors[ImGuiCol_TitleBgActive] = ImVec4(0.06f, 0.06f, 0.06f, 1.00f);
	colors[ImGuiCol_Button] = ImVec4(0.05f, 0.05f, 0.05f, 0.54f);
	colors[ImGuiCol_ButtonHovered] = ImVec4(0.19f, 0.19f, 0.19f, 0.54f);
	colors[ImGuiCol_ButtonActive] = ImVec4(0.20f, 0.22f, 0.23f, 1.00f);
	colors[ImGuiCol_Header] = ImVec4(0.00f, 0.00f, 0.00f, 0.52f);
	colors[ImGuiCol_HeaderHovered] = ImVec4(0.00f, 0.00f, 0.00f, 0.36f);
	colors[ImGuiCol_PopupBg] = ImVec4(0.19f, 0.19f, 0.19f, 0.92f);

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

#define FastJSonWriteImColor(color) \
{ \
	JSONData["Theme"][#color]["r"] = colors[color].x;\
	JSONData["Theme"][#color]["g"] = colors[color].y;\
	JSONData["Theme"][#color]["b"] = colors[color].z;\
	JSONData["Theme"][#color]["a"] = colors[color].w;\
} \

void CUIThemeManager::Save()
{
	if (!IsLoaded)
		return;

	json JSONData = {};
	ImVec4* colors = ImGui::GetStyle().Colors;

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

	JSONData["Theme"]["InactiveAlpha"] = TransparentDefault;
	JSONData["Theme"]["ActiveAlpha"] = TransparentUnfocused;
	JSONData["Theme"]["Font"] = ImCurrentFont;

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
	if (EFS.GetSaveName("$app_data_root$", jfn, 0, 6, "*.json"))
	{
		json JSONData = {};
		ImVec4* colors = ImGui::GetStyle().Colors;

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

		JSONData["Theme"]["InactiveAlpha"] = TransparentDefault;
		JSONData["Theme"]["ActiveAlpha"] = TransparentUnfocused;
		JSONData["Theme"]["Font"] = ImCurrentFont;


		std::ofstream o(jfn.c_str());
		o << JSONData;
	}
}


void CUIThemeManager::LoadFrom()
{
	json JSONData = {};
	xr_string jfn;
	if (EFS.GetOpenName("$app_data_root$", jfn, false, NULL, 6, "*.json"))
	{
		std::ifstream f(jfn.c_str());
		f >> JSONData;

		if (!JSONData.contains("Theme"))
		{
			return;
		}

		ImVec4* colors = ImGui::GetStyle().Colors;

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

	ImVec4* colors = ImGui::GetStyle().Colors;

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

	IsLoaded = true;
}

#undef FastJSonReadImColor
#undef FastJSonWriteImColor