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
	if (ImGui::Begin("Edit Mode", 0))
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
			if (ImGui::Begin("Object Tools", &bUseObjectsTool))
			{
				if (LTools->GetToolForm())
					LTools->GetToolForm()->Draw();
			}
			ImGui::End();

			if (UIObjectTool* pTool = smart_cast<UIObjectTool*>(LTools->GetToolForm()))
			{
				pTool->DrawObjectsList();
			}
			else if (UISpawnTool* pTool = smart_cast<UISpawnTool*>(LTools->GetToolForm()))
			{
				pTool->DrawObjectsList();
			}
			else if (UIParticlesTool* pTool = smart_cast<UIParticlesTool*>(LTools->GetToolForm()))
			{
				pTool->DrawObjectsList();
			}
		}
	}

	if (!bUseSnapList)
		return;

	if (ImGui::Begin("Snap List", &bUseSnapList))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 1));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 4));
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(4, 0));
		
		if (ImGui::Checkbox("Enable/Show Snap List", &bDrawSnapListObjects))
			static_cast<CLevelPreferences*>(EPrefs)->OpenSnapList = bDrawSnapListObjects;
		
		ImGui::Separator();
		{
			ImGui::BulletText("Commands", ImGuiDir_Left);
			if (ImGui::BeginPopupContextItem("Commands", 1))
			{
				if (ImGui::MenuItem("Make List From Selected"))
				{
					ExecCommand(COMMAND_SET_SNAP_OBJECTS);
				}
				if (ImGui::MenuItem("Select Object From List"))
				{
					ExecCommand(COMMAND_SELECT_SNAP_OBJECTS);
				}
				ImGui::Separator();
				if (ImGui::MenuItem("Add Selected To List"))
				{
					ExecCommand(COMMAND_ADD_SEL_SNAP_OBJECTS);
				}
				if (ImGui::MenuItem("Remove Selected From List"))
				{
					ExecCommand(COMMAND_DEL_SEL_SNAP_OBJECTS);
				}
				ImGui::EndPopup();
			}
			ImGui::OpenPopupOnItemClick("Commands", 0);
		}

		ImGui::Separator();
		ImGui::Checkbox("+/- Mode", &m_SnapListMode); ImGui::SameLine(0, 10);
		if (ImGui::Button("X"))
		{
			if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to clear snap objects?") == mrYes)
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