#include "stdafx.h"
#include "xrUITheme.h"
#include "imgui_user.h"
#include "../xrEUI/ModernUI.h"
#include "IconsFontAwesome7.h"

using namespace XRay::ImGui;

CUIThemeManager::CUIThemeManager()
{
	bOpen = false;
	// Visible in every editor tab
	TabIndex = -1;
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

	ImGui::SetNextWindowSize(ImVec2(ImGui::GetFontSize() * 52.0f, 0.0f), ImGuiCond_Appearing);
	if (ImGui::Begin("Theme Editor", &bOpen, ImGuiWindowFlags_AlwaysAutoResize))
	{
		IsDocked = ImGui::IsWindowDocked();
		IsFocused = ImGui::IsWindowFocused();

		static const char* PresetNames[] = { "Dark Blue", "Visual Studio", "Dark Orange", "Nordic", "Everforest", "Catppuccin Mocha", "Gruvbox", "Tokyo Night" };
		for (int PresetID = 0; PresetID < (int)std::size(PresetNames); ++PresetID)
		{
			if (PresetID % 4 != 0)
			{
				ImGui::SameLine();
			}
			DrawPreview(PresetID, PresetNames[PresetID]);
		}
	}
	ImGui::End();
}

void CUIThemeManager::DrawPreview(int PresetID, const char* Name)
{
	// Colors of the preset, the current theme is restored right after
	SetupColorsList(PresetID);
	const ImU32 Background = GetEditorColor(EEditorColors::BackgroundTint);
	const ImU32 Panel = GetEditorColor(EEditorColors::PanelTint);
	const ImU32 PanelBorder = GetEditorColor(EEditorColors::PanelBorderTint);
	const ImU32 Tab = GetEditorColor(EEditorColors::TabBarTint);
	const ImU32 Table = GetEditorColor(EEditorColors::TableTint);
	const ImU32 Button = GetEditorColor(EEditorColors::ButtonTint);
	const ImU32 Accent = GetEditorColor(EEditorColors::Accent);
	SetupColorsList(ThemeID);

	const float Unit = ImGui::GetFontSize();
	const ImVec2 Size = { Unit * 16.0f, Unit * 10.0f };

	ImGui::BeginGroup();
	ImGui::PushID(PresetID);
	const bool Clicked = ImGui::InvisibleButton("##preview", Size);
	const bool Hovered = ImGui::IsItemHovered();
	ImGui::PopID();

	const ImVec2 Min = ImGui::GetItemRectMin();
	const ImVec2 Max = ImGui::GetItemRectMax();
	const float Gap = Unit * 0.2f;
	ImDrawList* DrawList = ImGui::GetWindowDrawList();

	auto Rect = [&](float X0, float Y0, float X1, float Y1, ImU32 Color)
	{
		DrawList->AddRectFilled({ Min.x + X0 * Size.x, Min.y + Y0 * Size.y }, { Min.x + X1 * Size.x, Min.y + Y1 * Size.y }, Color);
	};

	DrawList->AddRectFilled(Min, Max, Background);

	// Workspace tabs and toolbar
	Rect(0.02f, 0.03f, 0.14f, 0.09f, PanelBorder);
	Rect(0.15f, 0.03f, 0.25f, 0.09f, Tab);
	Rect(0.26f, 0.03f, 0.36f, 0.09f, Tab);
	for (int Index = 0; Index < 6; ++Index)
	{
		const float X = 0.02f + Index * 0.045f;
		Rect(X, 0.12f, X + 0.035f, 0.18f, Index == 3 ? Accent : Button);
	}

	// Left panel with the selected edit mode
	Rect(0.02f, 0.21f, 0.24f, 0.97f, Panel);
	for (int Index = 0; Index < 7; ++Index)
	{
		const float Y = 0.25f + Index * 0.07f;
		Rect(0.04f, Y, 0.22f, Y + 0.05f, Index == 0 ? Accent : Button);
	}

	// Viewport
	Rect(0.26f, 0.21f, 0.74f, 0.7f, IM_COL32(18, 18, 22, 255));
	Rect(0.26f, 0.21f, 0.74f, 0.27f, PanelBorder);

	// Content browser
	Rect(0.26f, 0.72f, 0.74f, 0.97f, Panel);
	Rect(0.28f, 0.76f, 0.4f, 0.93f, Table);

	// Properties with toggles
	Rect(0.76f, 0.21f, 0.98f, 0.97f, Panel);
	for (int Index = 0; Index < 6; ++Index)
	{
		const float Y = 0.25f + Index * 0.08f;
		Rect(0.78f, Y, 0.86f, Y + 0.06f, Table);
		Rect(0.87f, Y, 0.96f, Y + 0.06f, Index % 2 == 0 ? Accent : Button);
	}

	const bool Selected = PresetID == ThemeID;
	if (Selected || Hovered)
	{
		const ImU32 Frame = Selected ? (ImU32)GetEditorColor(EEditorColors::Accent) : ImGui::GetColorU32(ImGuiCol_TextDisabled);
		DrawList->AddRect({ Min.x - Gap, Min.y - Gap }, { Max.x + Gap, Max.y + Gap }, Frame, 0.0f, 0, 2.0f);
	}

	if (Selected)
	{
		ImGui::Text(ICON_FA_CHECK " %s", Name);
	}
	else
	{
		ImGui::TextUnformatted(Name);
	}
	ImGui::EndGroup();

	if (Clicked && !Selected)
	{
		InitDefault(PresetID);
	}
}

void CUIThemeManager::InitDefault(int InThemeID)
{
	if (InThemeID >= 0)
	{
		ThemeID = InThemeID;
	}

	float ScreenDPI = GUIManager->GetScaleDpi();
	XRay::ImGui::MakeEditorTheme();
	XRay::ImGui::SetupColorsList(ThemeID);
	XRay::ImGui::SetupSizesList(0);
	XRay::ImGui::InitSizes();

	ImGuiStyle& style = ImGui::GetStyle();
	//style.ScaleAllSizes(ScreenDPI);
	ImVec4* colors = style.Colors;
	colors[ImGuiCol_MenuBarBg]				= GetEditorColor(EEditorColors::BackgroundTint);
	colors[ImGuiCol_FrameBg]				= GetEditorColor(EEditorColors::TableTint);
	colors[ImGuiCol_WindowBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_Border]					= GetEditorColor(EEditorColors::PanelBorderTint);
	colors[ImGuiCol_ChildBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_TableRowBg]				= GetEditorColor(EEditorColors::PanelTint);
	colors[ImGuiCol_TableRowBgAlt]			= GetEditorColor(EEditorColors::PanelTint);
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
	colors[ImGuiCol_PopupBg]				= GetEditorColor(EEditorColors::PanelTint);

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
	style.WindowBorderSize					= 1.0f;
	style.ChildBorderSize					= 0.0f;

	style.WindowPadding						= { GetEditorSize(EEditorSizes::WindowPadding),  GetEditorSize(EEditorSizes::WindowPadding) };
	style.FrameBorderSize					= 0.0f;
	style.FramePadding						= { GetEditorSize(EEditorSizes::ButtonPaddingW), (GetEditorSize(EEditorSizes::ButtonSize) - GetEditorSize(EEditorSizes::FontSize)) / 2 };
	style.FrameRounding						= GetEditorSize(EEditorSizes::ButtonRadius);
	style.TabRounding						= GetEditorSize(EEditorSizes::ButtonRadius);
	style.ChildRounding						= 0.0f;
	style.PopupRounding						= 0.0f;
	style.PopupBorderSize					= 1.0f;
	style.ItemSpacing						= ImVec2(4.f, 2.f);
	style.ItemInnerSpacing					= ImVec2(2.f, 2.f);
	style.DockingSeparatorSize				= GetEditorSize(EEditorSizes::DockingGap);
	style.ScrollbarSize						= GetEditorSize(EEditorSizes::ButtonSize) / 2;
	style.ScrollbarRounding					= GetEditorSize(EEditorSizes::ButtonSize) / 4;
	style.GrabRounding						= GetEditorSize(EEditorSizes::ButtonRadius);
	style.GrabMinSize						= style.GrabRounding * 2;
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