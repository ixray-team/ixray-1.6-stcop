#include "stdafx.h"

#include "../xrECore/Editor/EditMesh.h"

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

			const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize) * 0.7f;
			ImGui::TextUnformatted("Smooth Groups:");
			ImGui::SameLine();

			bool SMEdge = EPrefs->SmoothGroup == ESmoothGroup::Edges;
			bool SMNormals = EPrefs->SmoothGroup == ESmoothGroup::Normals;
			bool SMOther = EPrefs->SmoothGroup == ESmoothGroup::Other;
			ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
			if (XRay::ImGui::ToolbarButton("##SmoothEdge", "Edges", &SMEdge, { 0, ButtonSize }, ImDrawFlags_RoundCornersLeft))
			{
				EPrefs->SmoothGroup = ESmoothGroup::Edges;
			}
			ImGui::SameLine();
			if (XRay::ImGui::ToolbarButton("##SmoothNormals", "Normals", &SMNormals, {0, ButtonSize}, ImDrawFlags_RoundCornersNone))
			{
				EPrefs->SmoothGroup = ESmoothGroup::Normals;
			}
			ImGui::SameLine();
			if (XRay::ImGui::ToolbarButton("##SmoothOther", "Legacy", &SMOther, {0, ButtonSize}, ImDrawFlags_RoundCornersRight))
			{
				EPrefs->SmoothGroup = ESmoothGroup::Other;
			}
			ImGui::PopStyleVar();
			//ImGui::Checkbox("Auto Smooth", &EPrefs->IsEdgeSmooth);
			//ImGui::SameLine(0, 10);

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

	if (ATools->CurrentObject() != nullptr)
	{
		if (!ATools->CurrentObject()->m_objectFlags.test(CEditableObject::eoDynamic))
		{
			if (ImGui::Button("Make dynamic"))
			{
				ATools->CurrentObject()->CreateBone("idle");

				for (EditMeshIt mesh_it = ATools->CurrentObject()->FirstMesh(); mesh_it != ATools->CurrentObject()->LastMesh(); mesh_it++)
				{
					CEditableMesh* pMesh = *mesh_it;
					pMesh->AssignMesh("idle");
				}

				ATools->RealUpdateProperties();
			}
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
