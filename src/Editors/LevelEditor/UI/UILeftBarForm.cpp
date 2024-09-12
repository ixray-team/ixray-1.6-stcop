#include "stdafx.h"

UILeftBarForm::UILeftBarForm()
{
	bUseSnapList = true;
	bUseObjectsTool = true;
	bDrawSnapListObjects = static_cast<CLevelPreferences*>(EPrefs)->OpenSnapList;
	m_SnapListMode = false;
	m_SnapItem_Current = 0;
}

UILeftBarForm::~UILeftBarForm()
{
}

void UILeftBarForm::Draw()
{
	if (ImGui::Begin(g_pStringTable->translate("ed_st_edit_mode").c_str(), 0))
	{
		static ObjClassID Tools[OBJCLASS_COUNT + 1] = {
													OBJCLASS_SCENEOBJECT,
													OBJCLASS_LIGHT,
													OBJCLASS_SOUND_SRC,
													OBJCLASS_SOUND_ENV,OBJCLASS_GLOW,
													OBJCLASS_SHAPE,
													OBJCLASS_SPAWNPOINT,
													OBJCLASS_WAY,
													OBJCLASS_SECTOR,
													OBJCLASS_PORTAL,
													OBJCLASS_GROUP,
													OBJCLASS_PS,
													OBJCLASS_DO,
													OBJCLASS_AIMAP,
													OBJCLASS_WM,
													OBJCLASS_FOG_VOL,
													OBJCLASS_PUDDLES,
													OBJCLASS_force_dword
		};

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 1));
		ImGui::Columns(2);
		ImGui::Separator();
		for (u32 i = 0; Tools[i] != OBJCLASS_force_dword; i++)
		{
			u32 id = 0;
			if (i % 2)
				id = ((OBJCLASS_COUNT + 1) / 2) + (i / 2);
			else
				id = (i / 2);
			ESceneToolBase* tool = Scene->GetTool(Tools[id]);
			bool visble = tool->IsVisible();
			ImGui::PushID(tool->ClassName());
			if (ImGui::Checkbox("##value", &visble)) { tool->m_EditFlags.set(ESceneToolBase::flVisible, visble); UI->RedrawScene(); }; ImGui::SameLine();

			if (ImGui::RadioButton(tool->ClassDesc(), LTools->GetTarget() == Tools[id]))
			{
				ExecCommand(COMMAND_CHANGE_TARGET, Tools[id]);
			}
			ImGui::PopID();
			ImGui::NextColumn();
		}
		ImGui::Columns(1);
		ImGui::Separator();
		ImGui::PopStyleVar(2);
	}
	ImGui::End();

	if (LTools->GetToolForm())
	{
		if (bUseObjectsTool)
		{
			if (ImGui::Begin(g_pStringTable->translate("ed_st_obj_tools").c_str(), &bUseObjectsTool))
			{
				if (LTools->GetToolForm())
					LTools->GetToolForm()->Draw();
			}
			ImGui::End();

			UIObjectTool* pTool = smart_cast<UIObjectTool*>(LTools->GetToolForm());
			if (pTool)
			{
				pTool->DrawObjectsList();
			}
		}
	}

	if (!bUseSnapList)
		return;

	if (ImGui::Begin(g_pStringTable->translate("ed_st_snap_list").c_str(), &bUseSnapList))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 1));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 4));
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(4, 0));
		
		if (ImGui::Checkbox(g_pStringTable->translate("ed_st_show_snap_list").c_str(), &bDrawSnapListObjects))
			static_cast<CLevelPreferences*>(EPrefs)->OpenSnapList = bDrawSnapListObjects;
		
		ImGui::Separator();
		{
			ImGui::BulletText(g_pStringTable->translate("ed_st_commands").c_str(), ImGuiDir_Left);
			if (ImGui::BeginPopupContextItem(g_pStringTable->translate("ed_st_commands").c_str(), 1))
			{
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_make_list_from_sel").c_str()))
				{
					ExecCommand(COMMAND_SET_SNAP_OBJECTS);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_sel_obj_from_list").c_str()))
				{
					ExecCommand(COMMAND_SELECT_SNAP_OBJECTS);
				}
				ImGui::Separator();
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_add_sel_to_list").c_str()))
				{
					ExecCommand(COMMAND_ADD_SEL_SNAP_OBJECTS);
				}
				if (ImGui::MenuItem(g_pStringTable->translate("ed_st_remove_sel_from_list").c_str()))
				{
					ExecCommand(COMMAND_DEL_SEL_SNAP_OBJECTS);
				}
				ImGui::EndPopup();
			}
			ImGui::OpenPopupOnItemClick(g_pStringTable->translate("ed_st_commands").c_str(), 0);
		}

		ImGui::Separator();
		ImGui::Checkbox(g_pStringTable->translate("ed_st_snap_list_mode").c_str(), &m_SnapListMode); ImGui::SameLine(0, 10);
		if (ImGui::Button("X"))
		{
			if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, g_pStringTable->translate("ed_st_clear_snap_dialog").c_str()) == mrYes)
				ExecCommand(COMMAND_CLEAR_SNAP_OBJECTS);
		}
		ImGui::PopStyleVar(2);
		ObjectList* lst = Scene->GetSnapList(true);
		
		float ListBoxHeight = ImGui::GetWindowSize().y - ImGui::GetCursorPosY() - 10;
		ListBoxHeight /= 15.3f;

		ImGui::SetNextItemWidth(-1);
		ImGui::ListBox
		(
			"##snap_list_box", 
			&m_SnapItem_Current, 
			[](void* data, int ind, const char** out)->bool 
			{
				auto item = reinterpret_cast<ObjectList*>(data)->begin(); std::advance(item, ind);
				*out = (*item)->GetName(); 
				return true; 
			}, 
			reinterpret_cast<void*>(lst), 
			lst->size(), 
			ListBoxHeight
		);

		ImGui::PopStyleVar(2);
	}
	ImGui::End();
}