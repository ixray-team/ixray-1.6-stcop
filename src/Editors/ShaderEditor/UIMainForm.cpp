#include "stdafx.h"
#include "IconsFontAwesome6.h"

#include "../xrECore/Editor/EditorChooseEvents.h"
UIMainForm* MainForm = nullptr;
UIMainForm::UIMainForm()
{
    EnableReceiveCommands();
    if (!ExecCommand(COMMAND_INITIALIZE, (u32)0, (u32)0)) 
    {
        xrLogger::FlushLog();
        exit(-1);
    }
    ExecCommand(COMMAND_UPDATE_GRID);

    FillChooseEvents();
    m_TopBar = new UITopBarForm();
    m_Render = new UIRenderForm();
    m_Render->SetToolBarEvent(TOnRenderToolBar(this, &UIMainForm::DrawRenderToolBar));
    m_MainMenu = new UIMainMenuForm();
    m_LeftBar = new UILeftBarForm();
}

UIMainForm::~UIMainForm()
{
    ClearChooseEvents();
    xr_delete(m_LeftBar);
    xr_delete(m_MainMenu);
    xr_delete(m_Render);
    xr_delete(m_TopBar);
    ExecCommand(COMMAND_DESTROY, (u32)0, (u32)0);
}

void UIMainForm::Draw()
{
    m_MainMenu->Draw();
    m_TopBar->Draw();
    m_LeftBar->Draw();
    m_Render->Draw();
}

bool UIMainForm::Frame()
{
	if (EContext.UI)
	{
		return EContext.UI->Idle();
	}

    return false;
}

static const char* GetToolIcon(const char* Name)
{
	if (0 == xr_strcmp(Name, "Engine Shader"))			return ICON_FA_CUBE;
	if (0 == xr_strcmp(Name, "Compiler Shader"))		return ICON_FA_GEARS;
	if (0 == xr_strcmp(Name, "Game Materials"))			return ICON_FA_LAYER_GROUP;
	if (0 == xr_strcmp(Name, "Game Material Pairs"))	return ICON_FA_LINK;
	if (0 == xr_strcmp(Name, "Sound Environment"))		return ICON_FA_VOLUME_HIGH;
	return ICON_FA_CODE;
}

void UIMainForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float ToolbarPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
	const ImGuiStyle& Style = ImGui::GetStyle();

	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);
	ImGui::BeginChild("##RenderFormToolbar", {0, ButtonSize + ToolbarPadding * 2}, 0, ImGuiWindowFlags_NoScrollbar);
	ImGui::PopStyleColor();

	auto& Tools = STools->m_Tools;

	string256 Labels[16];
	float FullWidth = 0.0f;
	int Count = 0;

	for (auto& Tool : Tools)
	{
		if (Count >= (int)std::size(Labels))
		{
			break;
		}

		const char* Name = Tool.second->ToolsName();
		xr_sprintf(Labels[Count], "%s  %s", GetToolIcon(Name), Name);
		FullWidth += ImGui::CalcTextSize(Labels[Count]).x + Style.FramePadding.x * 2.0f;
		++Count;
	}

	const bool bIconsOnly = FullWidth + ToolbarPadding * 2.0f > ImGui::GetContentRegionAvail().x;
	const bool bShortcuts = !ImGui::GetIO().WantTextInput && ImGui::GetIO().KeyCtrl;

	ImGui::SetCursorPos({ToolbarPadding, ToolbarPadding});
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	int Index = 0;
	for (ISHTools* Tool : Tools | std::views::values)
	{
		if (Index >= Count)
		{
			break;
		}

		const char* Name = Tool->ToolsName();

		ImDrawFlags Rounding = ImDrawFlags_RoundCornersNone;
		if (Count == 1)
		{
			Rounding = ImDrawFlags_RoundCornersAll;
		}
		else if (Index == 0)
		{
			Rounding = ImDrawFlags_RoundCornersLeft;
		}
		else if (Index == Count - 1)
		{
			Rounding = ImDrawFlags_RoundCornersRight;
		}

		if (Index > 0)
		{
			ImGui::SameLine();
		}

		ImGui::PushID(Index);

		bool bActive = STools->m_Current == Tool;
		const char* Label = bIconsOnly ? GetToolIcon(Name) : Labels[Index];
		const bool bShortcut = bShortcuts && Index < 9 && ImGui::IsKeyPressed((ImGuiKey)(ImGuiKey_1 + Index), false);

		if ((XRay::ImGui::ToolbarButton("##tool", Label, &bActive, {0, ButtonSize}, Rounding) || bShortcut) && STools->m_Current != Tool)
		{
			STools->OnChangeEditor(Tool);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (Index < 9)
			{
				ImGui::SetTooltip("%s (Ctrl+%d)", Name, Index + 1);
			}
			else
			{
				ImGui::SetTooltip("%s", Name);
			}
		}

		ImGui::PopID();
		++Index;
	}

	ImGui::PopStyleVar();
	ImGui::EndChild();
}