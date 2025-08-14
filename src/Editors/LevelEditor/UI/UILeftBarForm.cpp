#include "stdafx.h"
#include "IconsFontAwesome6.h"

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

// Вспомогательная функция для рендеринга кнопки инструмента
void RenderToolButton(ESceneToolBase * tool, ObjClassID tool_id)
{
	if (!tool) return;

	bool visible = tool->IsVisible();
	ImGui::PushID(tool->ClassName());
	ImGui::BeginDisabled(!tool->IsEnabled());

	// Кнопка видимости
	xr_string icon = visible ? ICON_FA_EYE"##" : ICON_FA_EYE_SLASH"##";
	icon += tool->ClassName();

	auto col = ImGui::GetStyle().Colors[ImGuiCol_CheckMark];
	if (!visible) col.w = 0.5f;

	ImGui::PushStyleColor(ImGuiCol_Text, col);
	if (ImGui::Button(icon.c_str(), { 20, 15 }))
	{
		visible = !visible;
		tool->m_EditFlags.set(ESceneToolBase::flVisible, visible);
		UI->RedrawScene();
	}
	ImGui::PopStyleColor();

	ImGui::SameLine();

	// Кнопка выбора инструмента
	auto col_tool = ImGui::GetStyle().Colors[ImGuiCol_Button];
	if (LTools->GetTarget() == tool_id)
		col_tool = ImGui::GetStyle().Colors[ImGuiCol_ButtonActive];

	ImGui::PushStyleColor(ImGuiCol_Button, col_tool);
	if (ImGui::Button(tool->ClassDesc(), ImVec2(-1, 15)))
	{
		ExecCommand(COMMAND_CHANGE_TARGET, tool_id);
	}
	ImGui::PopStyleColor();

	ImGui::EndDisabled();
	ImGui::PopID();
}
void UILeftBarForm::Draw()
{
	ImGuiStyle& Style = ImGui::GetStyle();

	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 4));
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(0, 0));

	if (ImGui::Begin("Edit Mode", 0, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse))
	{
		static ObjClassID Tools[OBJCLASS_COUNT + 1] = {
			OBJCLASS_SCENEOBJECT,
			OBJCLASS_LIGHT,
			OBJCLASS_SOUND_SRC,
			OBJCLASS_SOUND_ENV, OBJCLASS_GLOW,
			OBJCLASS_SHAPE,
			OBJCLASS_SPAWNPOINT,
			OBJCLASS_WAY,
			OBJCLASS_TERRAIN, // End left
			OBJCLASS_SECTOR,
			OBJCLASS_PORTAL,
			OBJCLASS_GROUP,
			OBJCLASS_PS,
			OBJCLASS_DO,
			OBJCLASS_AIMAP,
			OBJCLASS_WM,
			OBJCLASS_FOG_VOL,
			OBJCLASS_PUDDLES, // End right
			OBJCLASS_force_dword
		};

		if (ImGui::BeginTable("EditModeTable", 2, ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_NoPadOuterX | ImGuiTableFlags_NoPadInnerX))
		{
			const float column_width = ImGui::GetContentRegionAvail().x * 0.5f;
			ImGui::TableSetupColumn("Left", ImGuiTableColumnFlags_WidthFixed, column_width);
			ImGui::TableSetupColumn("Right", ImGuiTableColumnFlags_WidthFixed, column_width);

			for (u32 i = 0; Tools[i] != OBJCLASS_force_dword; i++)
			{
				const u32 id = (i % 2) ? ((OBJCLASS_COUNT + 1) / 2 + (i / 2)) : (i / 2);
				ESceneToolBase* tool = Scene->GetTool(Tools[id]);

				if (i % 2 == 0)
					ImGui::TableNextRow(ImGuiTableRowFlags_None, 0.0f);

				ImGui::TableSetColumnIndex(i % 2);

				ImGui::PushID(tool->ClassName());
				ImGui::BeginDisabled(!tool->IsEnabled());

				const bool IsVisible = tool->IsVisible();
				const xr_string Icon = IsVisible ? ICON_FA_EYE"##" : ICON_FA_EYE_SLASH"##";
				ImVec4 IconColor = ImGui::GetStyle().Colors[ImGuiCol_CheckMark];
				if (!IsVisible) IconColor.w = 0.5f;

				ImGui::PushStyleColor(ImGuiCol_Text, IconColor);
				if (ImGui::Button(Icon.c_str(), ImVec2(20, 15)))
				{
					tool->m_EditFlags.set(ESceneToolBase::flVisible, !IsVisible);
					UI->RedrawScene();
				}
				ImGui::PopStyleColor();

				ImGui::SameLine(0, 0);

				const bool IsActive = (LTools->GetTarget() == Tools[id]);
				ImVec4 BtnColor = IsActive ? Style.Colors[ImGuiCol_ButtonActive] : Style.Colors[ImGuiCol_Button];

				ImGui::PushStyleColor(ImGuiCol_Button, BtnColor);
				if (ImGui::Button(tool->ClassDesc(), ImVec2(-1, 15)))
				{
					ExecCommand(COMMAND_CHANGE_TARGET, Tools[id]);
				}
				ImGui::PopStyleColor();

				ImGui::EndDisabled();
				ImGui::PopID();
			}

			ImGui::EndTable();
		}

	}
	ImGui::End();
	ImGui::PopStyleVar(4);

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