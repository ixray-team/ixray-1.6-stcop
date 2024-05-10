#include "stdafx.h"

extern ECORE_API BOOL g_force16BitTransformQuant;
extern ECORE_API BOOL g_force32BitTransformQuant;

UILeftBarForm::UILeftBarForm()
{
	m_RenderMode = Render_Editor;
	m_PickMode = 2;

	if (g_force16BitTransformQuant)
	{
		m_AnimMode = e16bit;
	}
	else if (g_force32BitTransformQuant)
	{
		m_AnimMode = e32bit;
	}
}

UILeftBarForm::~UILeftBarForm()
{
}

void UILeftBarForm::Draw()
{
	if (ImGui::Begin("LeftBar", 0))
	{
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (ImGui::TreeNode("Model"))
		{
			ImGui::AlignTextToFramePadding();
			ImGui::Text("Render:"); ImGui::SameLine();
			if (ImGui::RadioButton("Editor", m_RenderMode == Render_Editor))
			{
				ATools->PhysicsStopSimulate();
				m_RenderMode = Render_Editor;
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
				UI->RedrawScene();
			}

			ImGui::SameLine();
			if (ImGui::RadioButton("Engine", m_RenderMode == Render_Engine))
			{
				ATools->PhysicsStopSimulate();
				m_RenderMode = Render_Engine;
				if (!ATools->IsVisualPresent()) ExecCommand(COMMAND_MAKE_PREVIEW);
				if (!ATools->IsVisualPresent()) SetRenderMode(false);
				else						  SetRenderMode(true);
				ExecCommand(COMMAND_UPDATE_PROPERTIES);
				UI->RedrawScene();
			}


			ImGui::Checkbox("Auto Smooth", &EPrefs->IsEdgeSmooth);
			ImGui::SameLine(0, 10);

			if (ImGui::Button("Bone View")) 
			{
				ATools->BoneView->Show(true);
			}

			ImGui::SameLine(0, 10);
			if (ImGui::Button("Bone Parts"))
			{
				UIBoneForm::Show();
			}

			ImGui::Separator();
			ImGui::Text("Animation:"); ImGui::SameLine();

			if (ImGui::RadioButton("8bit", m_AnimMode == e8bit))
			{
				m_AnimMode = e8bit;
				g_force16BitTransformQuant = false;
				g_force32BitTransformQuant = false;
			}

			ImGui::SameLine();
			if (ImGui::RadioButton("16bit", m_AnimMode == e16bit))
			{
				m_AnimMode = e16bit;
				g_force16BitTransformQuant = true;
				g_force32BitTransformQuant = false;
			}
			
			ImGui::SameLine(); 
			if (ImGui::RadioButton("32bit", m_AnimMode == e32bit))
			{
				m_AnimMode = e32bit;
				g_force16BitTransformQuant = false;
				g_force32BitTransformQuant = true;
			}

			static const char* PickModeList[] = { "None","Surface","Bone" };
			ImGui::Combo("Pick mode", &m_PickMode, PickModeList, 3, -1);
			ImGui::TreePop();

		}

		ImGui::Separator();
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		//if (ImGui::TreeNode("Object Items"))
		{
			ImGui::BeginGroup();
			ATools->m_ObjectItems->Draw();
			ImGui::EndGroup();
		//	ImGui::TreePop();
		}
	}
	ImGui::End();

	if (ImGui::Begin("Item Properties", 0))
	{
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		ImGui::BeginGroup();
			ATools->m_Props->Draw();
		ImGui::EndGroup();
	}

	ImGui::End();
}

void UILeftBarForm::SetRenderMode(bool bEngineMode)
{
	if (ATools->IsVisualPresent() && bEngineMode)
		m_RenderMode = Render_Engine;
	else 		
		m_RenderMode = Render_Editor;
	ATools->PlayMotion();
}
