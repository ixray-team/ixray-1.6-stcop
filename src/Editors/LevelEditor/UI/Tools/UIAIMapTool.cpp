#include "stdafx.h"
#include "IconsFontAwesome6.h"

UIAIMapTool::UIAIMapTool()
{
	m_Mode = mdAppend;
	m_AutoLink = true;
	m_IgnoreConstraints = false;
	m_IgnoreMaterialsListSelected = 0;
	m_ChooseIgnoreMaterials = false;
}

UIAIMapTool::~UIAIMapTool()
{
}

void UIAIMapTool::Draw()
{
	const float TableRowHeight = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
	const float ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{

		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::Button("Generate Full", { SizeX, 0 })) tool->GenerateMap(false);
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Generate Selected", { SizeX, 0 })) tool->GenerateMap(true);
			if (XRay::ImGui::Button("Clear AI Map", { SizeX, 0 })) 
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to clear AI Map?") == mrYes) 
				{
					tool->Clear();
					Scene->UndoSave();
				}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Remove invalid nodes", { SizeX, 0 }))
				tool->CleanupInvalidNodes();

			XRay::ImGui::Separator();

			if (XRay::ImGui::Button("Smooth Selected", { SizeX, 0 }))	tool->SmoothNodes();
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Reset Selected", { SizeX, 0 }))	tool->ResetNodes();

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("AI Map Nodes"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			XRay::ImGui::ToggleButton("Ignore Constraints",&m_IgnoreConstraints, { SizeX, 0 });
			ImGui::SameLine(0, ItemSpacingX);
			XRay::ImGui::ToggleButton("Auto Link", &m_AutoLink, { SizeX, 0 });

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Ignore materials"))
		{
			if (XRay::ImGui::BeginTable("##objecttools_refselect", 3, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
				ImGui::TableSetupColumn("Empty", ImGuiTableColumnFlags_WidthStretch);	ImGui::TableSetupColumn("Add", ImGuiTableColumnFlags_WidthFixed);	// ImGui::TableSetupColumn("Clear All", ImGuiTableColumnFlags_WidthFixed);													ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();
				XRay::ImGui::TableNextColumn();
				XRay::ImGui::TableNextColumn();
				if (XRay::ImGui::Button("Add"))
				{
					UIChooseForm::SelectItem(smGameMaterial, 1, 0, 0, 0, 0, 0, 0);
					m_ChooseIgnoreMaterials = true;
				}
				XRay::ImGui::TableNextColumn();
				if (XRay::ImGui::Button("Clear All"))
				{
					m_IgnoreMaterialsListSelected = 0;
					m_IgnoreMaterialsList.clear();
					tool->m_ignored_materials.clear();
				}
				XRay::ImGui::EndTable();
			}
			ImGui::SetNextItemWidth(-0.01f);
			ImGui::ListBox("##mat_list_box", &m_IgnoreMaterialsListSelected, [](void* data, int ind, const char** out)->bool {*out = reinterpret_cast<xr_vector<xr_string>*>(data)->at(ind).c_str();  return true; }, reinterpret_cast<void*>(&this->m_IgnoreMaterialsList), m_IgnoreMaterialsList.size(), 7);

			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Link Commands"))
		{
			bool bAppend = m_Mode == mdAppend;
			bool bRemove = m_Mode == mdRemove;
			bool bInvert = m_Mode == mdInvert;

			if (XRay::ImGui::BeginTable("##objecttools_refselect", 5, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
												ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);
												ImGui::TableSetupColumn("Arrows1", ImGuiTableColumnFlags_WidthFixed);
												ImGui::TableSetupColumn("Arrows2", ImGuiTableColumnFlags_WidthFixed);
												ImGui::TableSetupColumn("Arrows3", ImGuiTableColumnFlags_WidthFixed);
												ImGui::TableSetupColumn("Buttons", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	if (XRay::ImGui::ToggleButton("Add", bAppend, { 60.f, 0 }))				m_Mode = mdAppend;
												XRay::ImGui::TableNextColumn();
												XRay::ImGui::TableNextColumn();	if (XRay::ImGui::Button(ICON_FA_CHEVRON_UP, { TableRowHeight, 0 }))		SideClick(1);
												XRay::ImGui::TableNextColumn();
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button("Select 0-Link", { -0.01f, 0 }))				{ tool->SelectNodesByLink(0); Scene->UndoSave(); }

				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	if (XRay::ImGui::ToggleButton("Delete ", bRemove, { 60.f, 0 }))			m_Mode = mdRemove;
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button(ICON_FA_CHEVRON_LEFT, { TableRowHeight, 0 }))	SideClick(0);
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button(ICON_FA_XMARK, { TableRowHeight, 0 }))			SideClick(4);
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button(ICON_FA_CHEVRON_RIGHT, { TableRowHeight, 0 }))	SideClick(2);
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button("Select 1-Link", { -0.01f, 0 }))				{ tool->SelectNodesByLink(1); Scene->UndoSave(); }

				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	if (XRay::ImGui::ToggleButton("Invert ", bInvert, { 60.f, 0 }))			m_Mode = mdInvert;
												XRay::ImGui::TableNextColumn();
												XRay::ImGui::TableNextColumn();	if (XRay::ImGui::Button(ICON_FA_CHEVRON_DOWN, { TableRowHeight, 0 }))	SideClick(3);
												XRay::ImGui::TableNextColumn();
												XRay::ImGui::TableNextColumn(); if (XRay::ImGui::Button("Select 2-Link", { -0.01f, 0 }))				{ tool->SelectNodesByLink(2); Scene->UndoSave(); }
				XRay::ImGui::EndTable();
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}



static const int idx[5][4] = {
	{0,1,2,3},
	{1,2,3,0},
	{2,3,0,1},
	{3,0,1,2},
	{4,4,4,4},
};

int ConvertV2L(int side)
{
	if (side < 4) {
		const Fvector& HPB = UI->CurrentView().m_Camera.GetHPB();
		float h = angle_normalize(HPB.x) / PI; R_ASSERT((h >= 0.f) && (h <= 2.f));
		if (h > 0.25f && h <= 0.75f)		return idx[3][side];
		else if (h > 0.75f && h <= 1.25f)	return idx[2][side];
		else if (h > 1.25f && h <= 1.75f)	return idx[1][side];
		else return idx[0][side];
	}
	else return side;
}

static const u8 fl[5] = { SAINode::flN1,SAINode::flN2,SAINode::flN3,SAINode::flN4,
							 SAINode::flN1 | SAINode::flN2 | SAINode::flN3 | SAINode::flN4,
	//    					 	 SAINode::flN1|SAINode::flN2,SAINode::flN2|SAINode::flN3,
	//    					 	 SAINode::flN3|SAINode::flN4,SAINode::flN4|SAINode::flN1
};
void UIAIMapTool::SideClick(int tag)
{
	ESceneAIMapTool::EMode mode;
	switch (m_Mode)
	{
	case UIAIMapTool::mdAppend:
		mode = ESceneAIMapTool::mdAppend;
		break;
	case UIAIMapTool::mdRemove:
		mode = ESceneAIMapTool::mdRemove;
		break;
	case UIAIMapTool::mdInvert:
		mode = ESceneAIMapTool::mdInvert;
		break;
	}
	
	tool->MakeLinks(fl[ConvertV2L(tag)], mode, m_IgnoreConstraints);
	Scene->UndoSave();
	UI->RedrawScene();
}

void UIAIMapTool::UpdateIgnoreMaterial()
{
	for (u16 MaterialID : tool->m_ignored_materials)
	{
		SGameMtl* mtl = GameMaterialLibraryEditors->GetMaterialByID(MaterialID);
		m_IgnoreMaterialsList.push_back(*mtl->m_Name);
	}
}

void UIAIMapTool::OnDrawUI()
{
	if (m_ChooseIgnoreMaterials)
	{
		bool result = false;
		xr_string name;
		if (UIChooseForm::GetResult(result, name))
		{
			if (result)
			{
				m_IgnoreMaterialsList.push_back(name);
				SGameMtl* mtl =  GameMaterialLibraryEditors->GetMaterial(name.c_str());
				tool->m_ignored_materials.push_back(mtl->GetID());
			}
			m_ChooseIgnoreMaterials = false;
		}
		UIChooseForm::Update();
	}
}