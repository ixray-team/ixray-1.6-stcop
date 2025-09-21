#include "stdafx.h"

#include "../xrECore/Editor/EditorChooseEvents.h"
#include "IconsFontAwesome6.h"

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
    ExecCommand(COMMAND_RENDER_FOCUS);

    FillChooseEvents();

    m_TopBar = new UITopBarForm();
    m_Render = new UIRenderForm();
    m_MainMenu = new UIMainMenuForm();
    m_LeftBar = new UILeftBarForm();
    m_KeyForm = new UIKeyForm();
	m_Render->SetToolBarEvent(TOnRenderToolBar(this, &UIMainForm::DrawRenderToolBar));

    // Action
    m_tSelect       = EDevice->Resources->_CreateTexture("ed\\bar\\select");
	m_tMove         = EDevice->Resources->_CreateTexture("ed\\bar\\move");
	m_tScale        = EDevice->Resources->_CreateTexture("ed\\bar\\scale");
	m_tRotate       = EDevice->Resources->_CreateTexture("ed\\bar\\rotate");

    // Axis
    m_tX            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisX");
	m_tY            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisY");
	m_tZ            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZ");
	m_tZX           = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZX");
}

UIMainForm::~UIMainForm()
{
    ClearChooseEvents();
    xr_delete(m_KeyForm);
    xr_delete(m_LeftBar);
    xr_delete(m_MainMenu);
    xr_delete(m_Render);
    xr_delete(m_TopBar);

    // Action
    m_tSelect.destroy();
    m_tMove.destroy();
    m_tScale.destroy();
    m_tRotate.destroy();

	// Axis
	m_tX.destroy();
	m_tY.destroy();
	m_tZ.destroy();
	m_tZX.destroy();

    ExecCommand(COMMAND_DESTROY, (u32)0, (u32)0);
}

void UIMainForm::Draw()
{
    m_MainMenu->Draw();
    m_TopBar->Draw();
    m_LeftBar->Draw();
    m_KeyForm->Draw();
    m_Render->Draw();
}

void UIMainForm::ResetEnd()
{
}

void UIMainForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	ImGui::SameLine(0, ImGui::GetFontSize() * 1.5);
	// Action
	{
		ETAction Action = ATools->GetAction();
		ImGui::BeginGroup();
		// Select
		{
			bool bPushColor = false;
			if (Action == etaSelect)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}

			if (ImGui::Button(ICON_FA_ARROW_POINTER, ImVec2(22, 20)))
			{
				ATools->SetAction(etaSelect);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Select");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Move
		{
			bool bPushColor = false;
			if (Action == etaMove)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			//m_tMove->Load();
			//if (ImGui::ImageButton("##DrawRenderToolBar594", m_tMove->pSurface, ImVec2(16, ImGui::GetFontSize())))
			//
			if (ImGui::Button(ICON_FA_ARROWS_UP_DOWN_LEFT_RIGHT))
			{
				ATools->SetAction(etaMove);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Move");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Scale
		{
			bool bPushColor = false;
			if (Action == etaScale)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			//m_tScale->Load();
			//if (ImGui::ImageButton("##DrawRenderToolBar620", m_tScale->pSurface, ImVec2(16, ImGui::GetFontSize())))
			if (ImGui::Button(ICON_FA_UP_RIGHT_AND_DOWN_LEFT_FROM_CENTER))
			{
				ATools->SetAction(etaScale);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Scale");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Rotate
		{
			bool bPushColor = false;
			if (Action == etaRotate)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			//m_tRotate->Load();
			//if (ImGui::ImageButton("##DrawRenderToolBar646", m_tRotate->pSurface, ImVec2(16, ImGui::GetFontSize())))
			if (ImGui::Button(ICON_FA_ROTATE))
			{
				ATools->SetAction(etaRotate);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Rotate");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}

		ImGui::EndGroup();
	}

	ImGui::BeginGroup();
	// --------------------------------------------------------------------------------------------
	ETAxis Axis = ATools->GetAxis();
	// Ось X
	{
		bool bPushColor = false;
		if (Axis == etAxisX)
		{
			bPushColor = true;
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		}
		m_tX->Load();
		if (ImGui::ImageButton("##DrawRenderToolBar1462", m_tX->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisX, !ATools->GetSettings(etAxisX));
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("Select X Axis");
		}
		if (bPushColor)
		{
			ImGui::PopStyleColor();
			ImGui::PopStyleColor();
		}
	}
	ImGui::Spacing();
	// Ось Y
	{
		bool bPushColor = false;
		if (Axis == etAxisY)
		{
			bPushColor = true;
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		}
		m_tY->Load();
		if (ImGui::ImageButton("##DrawRenderToolBar1488", m_tY->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisY, !ATools->GetSettings(etAxisY));
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("Select Y Axis");
		}
		if (bPushColor)
		{
			ImGui::PopStyleColor();
			ImGui::PopStyleColor();
		}
	}
	ImGui::Spacing();
	// Ось Z
	{
		bool bPushColor = false;
		if (Axis == etAxisZ)
		{
			bPushColor = true;
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		}
		m_tZ->Load();
		if (ImGui::ImageButton("##DrawRenderToolBar1514", m_tZ->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisZ, !ATools->GetSettings(etAxisZ));
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("Select Z Axis");
		}
		if (bPushColor)
		{
			ImGui::PopStyleColor();
			ImGui::PopStyleColor();
		}
	}
	ImGui::Spacing();
	// Ось ZX
	{
		bool bPushColor = false;
		if (Axis == etAxisZX)
		{
			bPushColor = true;
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
		}
		m_tZX->Load();
		if (ImGui::ImageButton("##DrawRenderToolBar1540", m_tZX->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisZX, !ATools->GetSettings(etAxisZX));
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("Select ZX Axis");
		}
		if (bPushColor)
		{
			ImGui::PopStyleColor();
			ImGui::PopStyleColor();
		}
	}
	ImGui::EndGroup();
	ImGui::NewLine();
}

bool UIMainForm::Frame()
{
    if(UI)  return UI->Idle();
    return false;
}
