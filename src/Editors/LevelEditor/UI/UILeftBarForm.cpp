#include "stdafx.h"
#include "IconsFontAwesome6.h"

UILeftBarForm::UILeftBarForm()
{
	bUseSnapList = true;
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

	ImVec2	WindowPadding	= ImGui::GetStyle().WindowPadding;
	ImVec2	ItemSpacing		= ImGui::GetStyle().ItemSpacing;
	float	PannelPadding	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::PanelPadding);
	float	TablePadding	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableBorder);
	float	ButtonSize		= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, WindowPadding + ImVec2(PannelPadding, PannelPadding));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(1.f, 1.f));

	if (ImGui::Begin("Edit Mode", 0, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse))
	{
		static ESceneItemsGuids Tools[OBJCLASS_COUNT + 1] = {
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
		
		if (ImGui::BeginTable("EditModeTable", 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_NoBordersInBody ))
		{
			ImGui::TableSetupColumn("Left", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("Right", ImGuiTableColumnFlags_WidthStretch);
			ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, { 0.f, 0.f });

			for (u32 i = 0; Tools[i] != OBJCLASS_force_dword; i++)
			{
				const u32 id = (i % 2) ? ((OBJCLASS_COUNT + 1) / 2 + (i / 2)) : (i / 2);
				ESceneToolBase* tool = Scene->GetTool(Tools[id]);

				if (i % 2 == 0)
					ImGui::TableNextRow(ImGuiTableRowFlags_None, 0.0f);

				ImGui::TableSetColumnIndex(i % 2);
				// --- Edit Mode Button
				{
					float FlagSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::IndicatorWidth);
					float ButtonH = ImGui::GetFontSize() + FlagSize / 2.f;
					float ShowH = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::CheckboxSize);


					ImGui::PushID(tool->ClassName());
					ImGui::BeginDisabled(!tool->IsEnabled());

					auto cur = ImGui::GetCursorPos();
					bool IsVisible = tool->IsVisible();
					bool IsActive = (LTools->GetTarget() == Tools[id]);

					// --- background button ---
					ImGui::SetNextItemAllowOverlap();
					if (XRay::ImGui::ButtonBackground(tool->ClassDesc(), &IsActive, { -0.01f, ButtonH }))
					{
						ExecCommand(COMMAND_CHANGE_TARGET, IsActive ? OBJCLASS_DUMMY : Tools[id]);
					}

					// --- Inactive Flag Stripe ---
					if (!IsActive)
					{
						const	float	StripeWidth	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::IndicatorWidth);
						const	float	Rounding	= ::ImGui::GetStyle().FrameRounding;
								ImVec2	Min			= ::ImGui::GetItemRectMin();
								ImVec2	Max			= ::ImGui::GetItemRectMax();
						ImDrawList* dl = ImGui::GetWindowDrawList();
						dl->AddRectFilled(
							Min, { Min.x + StripeWidth, Min.y + ButtonH },
							XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::Accent),
							Rounding, ImDrawFlags_RoundCornersLeft);
					}

					// --- Show/hide button ---
					const	xr_string	Icon		= IsVisible ? ICON_FA_EYE : ICON_FA_EYE_SLASH;
					const	xr_string	IconText	= Icon;
					const	ImColor		IconColor	= IsVisible
						? XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::Accent)
						: XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ContentIconTint)
						;
					const	ImVec2		ShowPos		= { cur.x + FlagSize * 1.5f, cur.y + (ButtonH - ShowH) / 2.f };
					ImGui::SetCursorPos(ShowPos);
					ImGui::PushStyleColor(ImGuiCol_Button, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableTint).Value);
					ImGui::PushStyleColor(ImGuiCol_ButtonHovered, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableHover).Value);
					ImGui::PushStyleColor(ImGuiCol_ButtonActive, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableActive).Value);
					ImGui::PushStyleColor(ImGuiCol_Text, IconColor.Value);
					ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.f);
					ImGui::SetWindowFontScale(0.7f);
					if (ImGui::Button(IconText.c_str(), { ShowH, ShowH }))
					{
						tool->m_EditFlags.set(ESceneToolBase::flVisible, !IsVisible);
						UI->RedrawScene();
					}
					ImGui::SetWindowFontScale(1.0f);
					ImGui::PopStyleColor(4);
					ImGui::PopStyleVar();

					// --- Text ---
					ImGui::SameLine();
					ImVec2	TextSize = ImGui::CalcTextSize(tool->ClassDesc());
					ImGui::SetCursorPos({ ShowPos.x + ShowH + 4.f, cur.y + (ButtonH - TextSize.y) * 0.5f });
					ImGui::Text(tool->ClassDesc());

					ImGui::EndDisabled();
					ImGui::PopID();
				}
			}
			ImGui::PopStyleVar();


			ImGui::EndTable();
		}

	}
	ImGui::End();
	ImGui::PopStyleVar(2);

	DrawObjectTool(WindowPadding, PannelPadding, ItemSpacing);

	if (!bUseSnapList)
	{
		ImGui::PopStyleColor();
		return;
	}

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

		float ListBoxHeight = ImGui::GetContentRegionAvail().y;
		ListBoxHeight /= 17.5f;

		ImGui::SetNextItemWidth(-1);
		ImGui::ListBox
		(
			"##snap_list_box",
			&m_SnapItem_Current,
			[](void* data, int ind, const char** out)->bool
			{
				auto item = reinterpret_cast<ObjectList*>(data)->begin();
				std::advance(item, ind);
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

void UILeftBarForm::DrawObjectTool(ImVec2& WindowPadding, float PannelPadding, ImVec2& ItemSpacing)
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, WindowPadding + ImVec2(PannelPadding, PannelPadding));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(ItemSpacing.y, ItemSpacing.y));
	if (ImGui::Begin("Object Tools", nullptr))
	{
		if (LTools->GetToolForm())
		{
			ImGui::BeginChild("Scroll", { -1.f, -1.f });
				LTools->GetToolForm()->Draw();
			ImGui::EndChild();
		}
	}
	ImGui::End();
	ImGui::PopStyleVar(2); // WindowPadding + ItemSpacing

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
	else
	{
		ImGui::Begin("Edit Group Items");
		ImGui::End();
	}
}