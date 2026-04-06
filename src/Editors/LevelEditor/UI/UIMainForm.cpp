#include "stdafx.h"

#include "../xrECore/Editor/EditorChooseEvents.h"
#include "../xrECore/Editor/UIEditLightAnim.h"

#include "../xrEUI/ImGuizmo.h"

#include "Editor/Utils/Gizmo/IM_Manipulator.h"
#include "Editor/Terrain/HeightmapUtils.h"

#include "IconsFontAwesome6.h"
#include "../xrECore/Editor/imgui_EditorEx.h"

UIMainForm* MainForm = nullptr;

static void ViewportFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->BeginEState(esEditScene);
}

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
	m_Properties = new UILPropertiesForm();
	m_WorldProperties = new UIWorldPropertiesFrom();
	m_Render->SetContextMenuEvent(TOnRenderContextMenu(this, &UIMainForm::DrawContextMenu));
	m_Render->SetToolBarEvent(TOnRenderToolBar(this, &UIMainForm::DrawRenderToolBar));
	m_Render->OnFocusCallback = (xr_delegate<void()>)ViewportFocusCallback;

	// Action
	m_tMenu         = EDevice->Resources->_CreateTexture("ed\\bar\\menu");
	m_tSelect       = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Select");
	m_tAdd          = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Add");
	m_tMove         = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Move");
	m_tScale        = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Scale");
	m_tRotate       = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Rotate");

	// Snap
	m_tGSnap        = EDevice->Resources->_CreateTexture("ed\\icons\\Snap Align to Normal");
	m_tOSnap        = EDevice->Resources->_CreateTexture("ed\\icons\\Snap to Grid");
	m_tMoveToSnap   = EDevice->Resources->_CreateTexture("ed\\icons\\Snap to Object");
	m_tNSnap        = EDevice->Resources->_CreateTexture("ed\\icons\\Snap while Moving");
	m_tVSnap        = EDevice->Resources->_CreateTexture("ed\\icons\\Snap to Vertex");
	m_tASnap        = EDevice->Resources->_CreateTexture("ed\\bar\\asnap"); //????
	m_tMSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\msnap"); //????

	m_tZoom         = EDevice->Resources->_CreateTexture("ed\\icons\\Zoom Extent");
	m_tZoomSel      = EDevice->Resources->_CreateTexture("ed\\icons\\Zoom Extents Selected");

	// Axis
	m_tX            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisX");
	m_tY            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisY");
	m_tZ            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZ");
	m_tZX           = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZX");
	
	m_tGrid         = EDevice->Resources->_CreateTexture("ed\\icons\\Snap Moving");
	m_tScaleGrid    = EDevice->Resources->_CreateTexture("ed\\icons\\Snap Scale");
	m_tAngle        = EDevice->Resources->_CreateTexture("ed\\icons\\Snap Rotate");

	m_tCsLocal      = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Parent CS");
	m_tNuScale      = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Non-Uniform Scale");

	// View
	m_tVFront       = EDevice->Resources->_CreateTexture("ed\\bar\\ViewFront");
	m_tVBack        = EDevice->Resources->_CreateTexture("ed\\bar\\ViewB");
	m_tVLeft        = EDevice->Resources->_CreateTexture("ed\\bar\\ViewLeft");
	m_tVRight       = EDevice->Resources->_CreateTexture("ed\\bar\\ViewRight");
	m_tVTop         = EDevice->Resources->_CreateTexture("ed\\bar\\ViewTop");
	m_tVBottom      = EDevice->Resources->_CreateTexture("ed\\bar\\ViewB");
	m_tVReset       = EDevice->Resources->_CreateTexture("ed\\bar\\ViewReset");

	// Camera
	m_tPlaneMove    = EDevice->Resources->_CreateTexture("ed\\bar\\PlaneMove");
	m_tArcBall      = EDevice->Resources->_CreateTexture("ed\\bar\\ArcBall");
	m_tFreeFly      = EDevice->Resources->_CreateTexture("ed\\bar\\FreeFly");

	TransformLocalOrWorld = EDevice->Resources->_CreateTexture("ed\\icons\\Tool Local");
	TransformLocalOrWorld2 = EDevice->Resources->_CreateTexture("ed\\icons\\Tool World");

	LoadWindowsStates();
}

void UIMainForm::ResetEnd()
{
}

void UIMainForm::LoadWindowsStates()
{
	CLevelPreferences* LPrefs = static_cast<CLevelPreferences*>(EPrefs);

	if (LPrefs->OpenObjectList)
	{
		UIObjectList::Show();
	}
	else
	{
		UIObjectList::Close();
	}

	if (LPrefs->OpenProperties)
	{
		m_Properties->Open();
	}
	else
	{
		m_Properties->Close();
	}

	if (LPrefs->OpenWorldProperties)
	{
		m_WorldProperties->Open();
	}
	else
	{
		m_WorldProperties->Close();
	}

	//if (LPrefs->OpenLightAnim)
	//{
	//	UIEditLightAnim::Show();
	//}
}

UIMainForm::~UIMainForm()
{
	CLevelPreferences* LPrefs = static_cast<CLevelPreferences*>(EPrefs);

	LPrefs->OpenProperties = !m_Properties->IsClosed();
	LPrefs->OpenWorldProperties = !m_WorldProperties->IsClosed();
	LPrefs->OpenObjectList = UIObjectList::IsOpen();
	LPrefs->OpenLightAnim = UIEditLightAnim::IsOpen();

	ClearChooseEvents();
	xr_delete(m_WorldProperties);
	xr_delete(m_Properties);
	xr_delete(m_LeftBar);
	xr_delete(m_MainMenu);
	xr_delete(m_Render);
	xr_delete(m_TopBar);

	// Action
	m_tMenu.destroy();
	m_tSelect.destroy();
	m_tAdd.destroy();
	m_tMove.destroy();
	m_tScale.destroy();
	m_tRotate.destroy();

	// Snap
	m_tGSnap.destroy();
	m_tOSnap.destroy();
	m_tMoveToSnap.destroy();
	m_tNSnap.destroy();
	m_tVSnap.destroy();
	m_tASnap.destroy();
	m_tMSnap.destroy();

	// Axis
	m_tX.destroy();
	m_tY.destroy();
	m_tZ.destroy();
	m_tZX.destroy();

	m_tZoom.destroy();
	m_tZoomSel.destroy();
	m_tGrid.destroy();
	m_tScaleGrid.destroy();
	m_tAngle.destroy();

	m_tCsLocal.destroy();
	m_tNuScale.destroy();

	// View
	m_tVFront.destroy();
	m_tVBack.destroy();
	m_tVLeft.destroy();
	m_tVRight.destroy();
	m_tVTop.destroy();
	m_tVBottom.destroy();
	m_tVReset.destroy();

	// Camera
	m_tPlaneMove.destroy();
	m_tArcBall.destroy();
	m_tFreeFly.destroy();
	TransformLocalOrWorld.destroy();
	TransformLocalOrWorld2.destroy();

	Console->Execute("cfg_save");
	ExecCommand(COMMAND_DESTROY, (u32)0, (u32)0);
}

shared_str UIMainForm::GetCommandShortcat(int CommandID) const
{
	ECommandVec& CommandVec = GetEditorCommands();

	if (CommandVec[CommandID] == nullptr)
		return {};

	ESubCommandVec& SubCommandVec = CommandVec[CommandID]->sub_commands;

	if (SubCommandVec.empty())
		return {};

	const xr_shortcut& Cat = SubCommandVec[0]->shortcut;

	xr_string txt;
	if (Cat.key == 0)
	{
		return {};
	}

	if (Cat.ext.test(xr_shortcut::flCtrl))
	{
		txt.append("Ctrl+");
	}
	if (Cat.ext.test(xr_shortcut::flShift))
	{
		txt.append("Shift+");
	}
	if (Cat.ext.test(xr_shortcut::flAlt))
	{
		txt.append("Alt+");
	}

	txt += SDL_GetScancodeName((SDL_Scancode)Cat.key);


	return txt.c_str();
}

void UIMainForm::DrawMenuItem(const char* label, int command, int param, int flag)
{
	if (ImGui::MenuItem(label, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainForm::DrawMenuItemI(const char* label, const char* icon, int command, int param, int flag)
{
	if (ImGui::MenuItemI(label, icon, *GetCommandShortcat(command)))
	{
		ExecCommand(command, param, flag);
	}
}

void UIMainForm::Draw()
{
	bOpen = true;
	m_MainMenu->Draw();
	m_TopBar->Draw();
	m_LeftBar->Draw();
	m_Properties->Draw();
	m_WorldProperties->Draw();

	m_Render->Draw();
}

bool UIMainForm::Frame()
{
	return UI && UI->Idle();
}

void UIMainForm::DrawContextMenu()
{
	if (ImGui::BeginMenu("Create"))
	{
		if (ImGui::BeginMenu("Shape"))
		{
			DrawMenuItem("Box", COMMAND_CREATE_SHAPE_BOX);
			DrawMenuItem("Sphere", COMMAND_CREATE_SHAPE_SPHERE);

			ImGui::EndMenu();
		}
		ImGui::EndMenu();
	}
	if (ImGui::BeginMenuI("Visiblity", ICON_FA_EYE))
	{
		DrawMenuItemI("Hide Selected", ICON_FA_EYE_SLASH, COMMAND_HIDE_SEL, FALSE);
		DrawMenuItemI("Hide Unselected", ICON_FA_EYE_SLASH, COMMAND_HIDE_UNSEL);
		DrawMenuItemI("Hide All", ICON_FA_EYE_SLASH, COMMAND_HIDE_ALL, FALSE);
		ImGui::Separator();

		DrawMenuItemI("Unhide All", ICON_FA_EYE, COMMAND_HIDE_ALL, TRUE);

		ImGui::EndMenu();
	}

	if (ImGui::BeginMenuI("Locking", ICON_FA_LOCK))
	{
		ESceneToolBase* SceneTool = Scene->GetTool(LTools->CurrentClassID());
		ESceneToolBase::ETestResult TestOut = SceneTool->TestSelectedObjectsFlag(CCustomObject::flRT_Locked);
		bool CanLock = TestOut != ESceneToolBase::ETestResult::All;
		bool CanUnlock = TestOut != ESceneToolBase::ETestResult::None;

		ImGui::BeginDisabled(!CanLock);
		DrawMenuItemI("Lock selection", ICON_FA_LOCK, COMMAND_LOCK_SEL, TRUE);
		DrawMenuItemI("Lock unselected", ICON_FA_LOCK, COMMAND_LOCK_UNSEL, TRUE);
		DrawMenuItemI("Lock all", ICON_FA_LOCK, COMMAND_LOCK_ALL, TRUE);
		ImGui::EndDisabled();

		ImGui::Separator();

		ImGui::BeginDisabled(!CanUnlock);
		DrawMenuItemI("Unlock selection", ICON_FA_LOCK_OPEN, COMMAND_LOCK_SEL, FALSE);
		DrawMenuItemI("Unlock unselected", ICON_FA_LOCK_OPEN, COMMAND_LOCK_UNSEL, FALSE);
		DrawMenuItemI("Unlock all", ICON_FA_LOCK_OPEN, COMMAND_LOCK_ALL, FALSE);
		ImGui::EndDisabled();

		ImGui::EndMenu();
	}

	if (ImGui::BeginMenuI("Edit", ICON_FA_PEN_TO_SQUARE))
	{
		DrawMenuItemI("Copy", ICON_FA_COPY, COMMAND_COPY);
		DrawMenuItemI("Paste", ICON_FA_PASTE, COMMAND_PASTE);
		DrawMenuItemI("Duplicate", ICON_FA_CLONE, COMMAND_DUPLICATE);

		ImGui::Separator();

		DrawMenuItemI("Cut", ICON_FA_SCISSORS, COMMAND_CUT);

		ImGui::Separator();

		DrawMenuItemI("Delete", ICON_FA_TRASH, COMMAND_DELETE_SELECTION);

		ImGui::EndMenu();
	}
	ImGui::Separator();

	DrawMenuItemI("Properties", ICON_FA_GEAR, COMMAND_SHOW_PROPERTIES);

	if (ImGui::MenuItem("Make Heightmap"))
	{
		ESceneObjectTool* mt = (ESceneObjectTool*)Scene->GetTool(OBJCLASS_SCENEOBJECT);
		if (CSceneObject* Obj = (CSceneObject*)mt->LastSelected())
		{
			auto MeshObjects = Obj->Meshes();

			for (auto Mesh : *MeshObjects)
			{
				XRay::Editor::HeightmapUtils::GenerateHeightmapByMesh(Obj->GetReference(), *Mesh->Name());
			}
		}
	}
}

void UIMainForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	const	float	ButtonSize		= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const	float	ToolbarPadding	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);
	ImGui::BeginChild("##RenderFormToolbar", { 0, ButtonSize + ToolbarPadding * 2 }, 0, ImGuiWindowFlags_NoScrollbar);
	ImGui::PopStyleColor();

	// Параметры таблицы, которые может настроить пользователь
	static ImVec2 cellPadding = ImVec2(ToolbarPadding, ToolbarPadding); // Отступы внутри ячеек
	static ImVec2 minColumnWidth = ImVec2(GUIManager->ScaleByDpi(100), 0); // Минимальная ширина колонок (0 = авто)
	static bool stretchColumns = true; // Растягивать ли колонки

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::SameLine(0, ToolbarPadding);

	auto DrawActionButton = [&](const char* id, auto& texture, ETAction action, const char* tooltip, ImDrawFlags flags)
	{
		bool selected = LTools->GetAction() == action;
		texture->Load();
		if (XRay::ImGui::ToolbarIconButton(id, texture->get_SRView()->GetRawSRV(), &selected, flags))
		{
			LTools->SetAction(action);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", tooltip);
		}
	};

	auto DrawSettingsButton = [&](const char* id, auto& texture, ETFlags setting, const char* tooltip, ImDrawFlags flags)
	{
		bool selected = Tools->GetSettings(setting);
		texture->Load();
		if (XRay::ImGui::ToolbarIconButton(id, texture->get_SRView()->GetRawSRV(), &selected, flags))
		{
			ExecCommand(COMMAND_SET_SETTINGS, setting, !Tools->GetSettings(setting));
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", tooltip);
		}
	};

	auto DrawSnapCombo = [&](const char* label, float& value, const float* values, int count, const char* tooltip, bool isAngle = false)
	{
		string_path temp;
		xr_sprintf(temp, "%.2f", isAngle ? rad2deg(value) : value);

		ImGui::SameLine();
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 4.2f);

		if (ImGui::BeginCombo(label, temp, ImGuiComboFlags_None))
		{
			for (int i = 0; i < count; i++)
			{
				if (ImGui::Selectable(isAngle ? std::to_string((int)values[i]).c_str() : std::to_string(values[i]).c_str(), false))
				{
					value = isAngle ? deg2rad(values[i]) : values[i];
				}

				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				}
			}
			ImGui::EndCombo();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s", tooltip);
		}
	};

	// Настройки флагов таблицы
	ImGuiTableFlags tableFlags = ImGuiTableFlags_Resizable | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_ContextMenuInBody;

	// Применяем отступы в ячейках
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, cellPadding);

	const int MaxTableIndex = EPrefs->ShowAxisButtons + EPrefs->ShowOldCameraButtons + 5;
	if (ImGui::BeginTable("##ToolbarGroups", MaxTableIndex + 1, tableFlags))
	{
		if (minColumnWidth.x > 0)
		{
			for (int i = 0; i < MaxTableIndex; i++)
			{
				ImGui::TableSetupColumn(std::format("##col_{}", i).c_str(), ImGuiTableColumnFlags_WidthFixed, minColumnWidth.x);
			}
		}

		// Строка 1: DrawMenuSettings
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::BeginGroup();
		DrawMenuSettings(); // Добавляем DrawMenuSettings в отдельную ячейку
		ImGui::EndGroup();

		// Группа Action кнопок
		ImGui::TableSetColumnIndex(1);
		ImGui::BeginGroup();
		DrawActionButton("##DrawRenderToolBar574", m_tSelect, etaSelect, "Select", ImDrawFlags_RoundCornersLeft);
		ImGui::SameLine();
		DrawActionButton("##DrawRenderToolBar568", m_tAdd, etaAdd, "Add", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawActionButton("##DrawRenderToolBar594", m_tMove, etaMove, "Move", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawActionButton("##DrawRenderToolBar646", m_tRotate, etaRotate, "Rotate", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawActionButton("##DrawRenderToolBar620", m_tScale, etaScale, "Scale", ImDrawFlags_RoundCornersRight);
		ImGui::SameLine();
		ImGui::EndGroup();

		ImGui::TableSetColumnIndex(2);
		ImGui::BeginGroup();
		DrawSettingsButton("##DrawRenderToolBar1173", m_tCsLocal, etfCSParent, "Parent Constraint Toggle", ImDrawFlags_RoundCornersLeft);
		ImGui::SameLine();
		DrawSettingsButton("##DrawRenderToolBar1200", m_tNuScale, etfNUScale, "Scaling by Axes only", ImDrawFlags_RoundCornersNone);
		
		ImGui::SameLine(0);
		const ETAction action = LTools->GetAction();
		ImGui::BeginDisabled(action == etaScale || action == etaSelect || action == etaAdd);

		bool UseLocal = imManipulator.MatrixMode;
		ref_texture& CurrentCoordsView = UseLocal ? TransformLocalOrWorld2 : TransformLocalOrWorld;
		if (CurrentCoordsView->get_SRView() == nullptr)
		{
			CurrentCoordsView = EDevice->texture_null;
		}

		if (XRay::ImGui::ToolbarIconButton("##LocalOrWorldTransform", CurrentCoordsView->get_SRView()->GetRawSRV(), &UseLocal, ImDrawFlags_RoundCornersRight))
		{
			imManipulator.MatrixMode = !UseLocal;
		}

		ImGui::EndDisabled();
		ImGui::EndGroup();

		// Группа привязок
		ImGui::TableSetColumnIndex(3);
		ImGui::BeginGroup();
		DrawSettingsButton("##DrawRenderToolBar687", m_tOSnap, etfOSnap, "Object Snap Toggle", ImDrawFlags_RoundCornersLeft);
		ImGui::SameLine();
		DrawSettingsButton("##DrawRenderToolBar713", m_tMoveToSnap, etfMTSnap, "Moving Snap To Object Toggle", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawSettingsButton("##DrawRenderToolBar785", m_tNSnap, etfNormalAlign, "Normal Alignment", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawSettingsButton("##DrawRenderToolBar811", m_tGSnap, etfGSnap, "Grid Snap Toggle", ImDrawFlags_RoundCornersNone);
		ImGui::SameLine();
		DrawSettingsButton("##DrawRenderToolBar791", m_tVSnap, etfVSnap, "Vertex Snap Toggle", ImDrawFlags_RoundCornersRight);
		ImGui::EndGroup();

		// Группа фокусировки
		ImGui::TableSetColumnIndex(4);
		ImGui::BeginGroup();
		m_tZoom->Load();
		if (XRay::ImGui::ToolbarIconButton("##DrawRenderToolBar816", m_tZoom->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersLeft))
		{
			ExecCommand(COMMAND_ZOOM_EXTENTS, FALSE);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Focus the whole scene");
		}

		ImGui::SameLine();
		m_tZoomSel->Load();
		if (XRay::ImGui::ToolbarIconButton("##DrawRenderToolBar830", m_tZoomSel->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersRight))
		{
			ExecCommand(COMMAND_ZOOM_EXTENTS, TRUE);
		}

		if (ImGui::IsItemHovered())
			ImGui::SetTooltip("Focus on the selected object");

		ImGui::EndGroup();

		ImGui::TableSetColumnIndex(5);
		// Группа Move snap
		ImGui::BeginGroup();
		DrawSettingsButton("##DrawRenderToolBar859", m_tGrid, etfMSnap, "Fixed object movement", ImDrawFlags_RoundCornersLeft);

		const float moveValues[] = { 0.01f, 0.05f, 0.1f, 0.5f, 1.f, 2.f, 5.f, 10.f, 25.f, 50.f, 100.f, 250.f, 500.f };
		DrawSnapCombo("##move", Tools->m_MoveSnap, moveValues, 13, "The choice of a fixed distance of movement of the object");
		ImGui::EndGroup();

		ImGui::SameLine(0, 4);
		// Группа Scale snap
		ImGui::BeginGroup();
		DrawSettingsButton("##DrawRenderToolBar972", m_tScaleGrid, etfScaleFixed, "Fixed Object Scaling", ImDrawFlags_RoundCornersLeft);

		const float scaleValues[] = { 0.01f, 0.05f, 0.1f, 0.25f, 0.5f, 1.f, 2.f, 5.f, 10.f, 50.f, 200.f, 1000.f };
		DrawSnapCombo("##scale", Tools->m_ScaleFixed, scaleValues, 12, "Setting a Fixed Object Scaling");
		ImGui::EndGroup();
		ImGui::SameLine(0, 4);
		// Группа Rotate snap
		ImGui::BeginGroup();
		DrawSettingsButton("##DrawRenderToolBar1085", m_tAngle, etfASnap, "Fixed object rotation angle", ImDrawFlags_RoundCornersLeft);

		const float angleValues[] = { 1.f, 5.f, 10.f, 15.f, 45.f, 90.f, 180.f };
		DrawSnapCombo("##rotate", Tools->m_RotateSnapAngle, angleValues, 7, "Set a fixed rotation angle of the object (in degrees)", true);
		ImGui::EndGroup();

		int Idx = 5;

		if (EPrefs->ShowAxisButtons)
		{
			ImGui::TableSetColumnIndex(++Idx);
			RenderAxisButtons();
		}

		if (EPrefs->ShowOldCameraButtons)
		{
			ImGui::TableSetColumnIndex(++Idx);
			RenderOldCameraButtons();
		}

		ImGui::EndTable();
	}
	ImGui::PopStyleVar(2);
	ImGui::EndChild();

	if (UI->ViewID == 0)
	{
		imManipulator.Render(Pos.x, Pos.y, Size.x, Size.y);
	}
}

void UIMainForm::DrawMenuSettings()
{
	auto DrawFlagMenuItem = [&](const char* label, u32 flag, const char* icon = nullptr)
	{
		bool selected = psDeviceFlags.test(flag);
		if (icon ? ImGui::MenuItemI(label, icon, "", &selected) : ImGui::MenuItem(label, "", &selected))
		{
			psDeviceFlags.set(flag, selected);
			UI->RedrawScene();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
	};

	// Меню
	{
		ImGui::BeginGroup();
		m_tMenu->Load();

		if (ImGui::BeginPopupContextItem("MenuScene"))
		{
			DrawFlagMenuItem("Draw Safe Rect", rsDrawSafeRect);
			DrawFlagMenuItem("Draw Grid", rsDrawGrid, ICON_FA_TABLE_CELLS);

			// Coordinate Axes подменю
			if (ImGui::BeginMenu("Coordinate Axes"))
			{
				bool disabled = psDeviceFlags.test(rsDisableAxisCube);

				if (ImGui::MenuItem("None", "", &disabled))
					psDeviceFlags.set(rsDisableAxisCube, disabled);

				ImGui::BeginDisabled(disabled);

				bool isAxis = psDeviceFlags.test(rsDrawAxis);
				bool selectedAxis = isAxis;
				bool selectedCube = !isAxis;

				if (ImGui::MenuItem("Axis", "", &selectedAxis))
					psDeviceFlags.set(rsDrawAxis, true);
				if (ImGui::MenuItem("Cube", "", &selectedCube))
					psDeviceFlags.set(rsDrawAxis, false);

				ImGui::EndDisabled();
				ImGui::EndMenu();
			}

			ImGui::Separator();
			DrawFlagMenuItem("Fog", rsFog, ICON_FA_CLOUD);

			// Environment подменю
			if (ImGui::BeginMenuI("Environment", ICON_FA_CLOUD_SUN))
			{
				DrawMenuItem("Properties", COMMAND_WEATHER_PROPERTIES);

				bool selected = !psDeviceFlags.test(rsEnvironment);
				if (ImGui::MenuItem("None", "", &selected))
				{
					psDeviceFlags.set(rsEnvironment, false);
					UI->RedrawScene();
				}

				ImGui::Separator();
				auto& weatherCycles = g_pGamePersistent->Environment().WeatherCycles;
				for (auto& cycle : weatherCycles)
				{
					selected = psDeviceFlags.test(rsEnvironment) && cycle.first == g_pGamePersistent->Environment().CurrentCycleName;

					if (ImGui::MenuItem(cycle.first.c_str(), "", &selected))
					{
						psDeviceFlags.set(rsEnvironment, true);
						g_pGamePersistent->Environment().SetWeather(cycle.first.c_str(), true);
						UI->RedrawScene();
					}

					if (ImGui::IsItemHovered())
					{
						ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					}
				}
				ImGui::EndMenu();
			}

			ImGui::Separator();

			// Render подменю
			if (ImGui::BeginMenu("Render"))
			{
				// Quality подменю
				if (ImGui::BeginMenu("Quality"))
				{
					const char* qualities[] = { "25%", "50%", "100%", "200%" };
					const float values[] = { 0.25f, 0.5f, 1.0f, 2.0f };

					for (int i = 0; i < 4; i++)
					{
						bool selected = (i == 0 && EDevice->m_ScreenQuality < 0.3f) ||
							(i == 1 && EDevice->m_ScreenQuality >= 0.3f && EDevice->m_ScreenQuality < 0.6f) ||
							(i == 2 && EDevice->m_ScreenQuality >= 0.6f && EDevice->m_ScreenQuality < 1.1f) ||
							(i == 3 && EDevice->m_ScreenQuality >= 1.1f && EDevice->m_ScreenQuality < 2.1f);

						if (ImGui::MenuItem(qualities[i], "", &selected))
						{
							UI->SetRenderQuality(values[i]);
							UI->RedrawScene();
						}
					}
					ImGui::EndMenu();
				}

				// Fill Mode подменю
				if (ImGui::BeginMenu("Fill Mode"))
				{
					const char* modes[] = { "Point", "Wireframe", "Solid" };
					D3DFILLMODE d3dModes[] = { D3DFILL_POINT, D3DFILL_WIREFRAME, D3DFILL_SOLID };

					for (int i = 0; i < 3; i++)
					{
						bool selected = EDevice->dwFillMode == d3dModes[i];
						if (ImGui::MenuItem(modes[i], "", &selected))
						{
							EDevice->dwFillMode = d3dModes[i];
							UI->RedrawScene();
						}
					}
					ImGui::EndMenu();
				}

				DrawFlagMenuItem("Edged Faces", rsEdgedFaces);
				ImGui::EndMenu();
			}

			ImGui::Separator();
			DrawFlagMenuItem("Mute Sounds", rsMuteSounds, ICON_FA_VOLUME_XMARK);
			DrawFlagMenuItem("Real Time", rsRenderRealTime, ICON_FA_HOURGLASS_HALF);
			ImGui::Separator();
			DrawFlagMenuItem("Stats", rsStatistic);

			ImGui::EndPopup();
		}

		//if (ImGui::ImageButton("##DrawRenderToolBar548", m_tMenu->get_SRView()->GetRawSRV(), ImVec2(16, ImGui::GetFontSize())))
		if (XRay::ImGui::ToolbarIconButton("##DrawRenderToolBar548", m_tMenu->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersLeft))
		{
			ImGui::OpenPopup("MenuScene");
		}

		ImGui::SameLine();
		const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
		XRay::ImGui::ToolbarButton("##HintButton", ICON_FA_LIGHTBULB, &MainForm->GetRenderForm()->UseHint, { ButtonSize, ButtonSize }, ImDrawFlags_RoundCornersRight);
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Hint");
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("Menu");
		}

		ImGui::EndGroup();
	}
}

void UIMainForm::RenderOldCameraButtons()
{
	ImGui::BeginGroup();
	if (XRay::ImGui::ToolbarIconButton("##ViewFront", m_tVFront->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersLeft))
	{
		UI->CurrentView().m_Camera.ViewFront();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Front View");
	}
	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##ViewBack", m_tVBack->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersNone))
	{
		UI->CurrentView().m_Camera.ViewBack();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Back View");
	}
	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##ViewLeft", m_tVLeft->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersNone))
	{
		UI->CurrentView().m_Camera.ViewLeft();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Left View");
	}
	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##ViewRight", m_tVRight->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersNone))
	{
		UI->CurrentView().m_Camera.ViewRight();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Right View");
	}
	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##ViewBottom", m_tVBottom->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersNone))
	{
		UI->CurrentView().m_Camera.ViewBottom();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Bottom View");
	}
	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##ViewTop", m_tVTop->get_SRView()->GetRawSRV(), nullptr, ImDrawFlags_RoundCornersRight))
	{
		UI->CurrentView().m_Camera.ViewTop();
		UI->RedrawScene();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Top View");
	}
	ImGui::SameLine();

	// Сбросить Вид.
	//{
	//	m_tVReset->Load();
	//	{
	//		if (ImGui::ImageButton("##DrawRenderToolBar1343", m_tVReset->get_SRView()->GetRawSRV(), ImVec2(16, ImGui::GetFontSize())))
	//		{
	//			UI->CurrentView().m_Camera.ViewReset();
	//			UI->RedrawScene();
	//		}
	//	}
	//	if (ImGui::IsItemHovered())
	//	{
	//		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	//		ImGui::SetTooltip("Reset View");
	//	}
	//}
	ImGui::EndGroup();

	ImGui::SameLine(0, 4);

	ImGui::BeginGroup();
	ECameraStyle Camera = UI->CurrentView().m_Camera.GetStyle();

	bool CamPlane = Camera == csPlaneMove;
	bool CamArcBall = Camera == cs3DArcBall;
	bool CamFly = Camera == csFreeFly;

	if (XRay::ImGui::ToolbarIconButton("##CamPlane", m_tPlaneMove->get_SRView()->GetRawSRV(), &CamPlane, ImDrawFlags_RoundCornersLeft))
	{
		UI->CurrentView().m_Camera.SetStyle(csPlaneMove);
		UI->RedrawScene();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Free camera mode");
	}

	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##CamArcBall", m_tArcBall->get_SRView()->GetRawSRV(), &CamArcBall, ImDrawFlags_RoundCornersNone))
	{
		UI->CurrentView().m_Camera.SetStyle(cs3DArcBall);
		UI->RedrawScene();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Snap the camera to the center of coordinates|scene");
	}

	ImGui::SameLine();

	if (XRay::ImGui::ToolbarIconButton("##CamFreeFly", m_tFreeFly->get_SRView()->GetRawSRV(), &CamFly, ImDrawFlags_RoundCornersRight))
	{
		UI->CurrentView().m_Camera.SetStyle(csFreeFly);
		UI->RedrawScene();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Automatic camera flyover of the scene");
	}
	ImGui::EndGroup();
}

void UIMainForm::RenderAxisButtons()
{
	ImGui::BeginGroup();

	ETAxis Axis = LTools->GetAxis();

	m_tX->Load();
	m_tY->Load();
	m_tZ->Load();
	m_tZX->Load();

	bool AxisX = Axis == etAxisX;
	bool AxisY = Axis == etAxisY;
	bool AxisZ = Axis == etAxisZ;
	bool AxisZX = Axis == etAxisZX;

	if (XRay::ImGui::ToolbarIconButton("##AxisX", m_tX->get_SRView()->GetRawSRV(), &AxisX, ImDrawFlags_RoundCornersLeft))
	{
		ExecCommand(COMMAND_CHANGE_AXIS, etAxisX, !LTools->GetSettings(etAxisX));
	}

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarIconButton("##AxisY", m_tY->get_SRView()->GetRawSRV(), &AxisY, ImDrawFlags_RoundCornersNone))
	{
		ExecCommand(COMMAND_CHANGE_AXIS, etAxisY, !LTools->GetSettings(etAxisY));
	}

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarIconButton("##AxisZ", m_tZ->get_SRView()->GetRawSRV(), &AxisY, ImDrawFlags_RoundCornersNone))
	{
		ExecCommand(COMMAND_CHANGE_AXIS, etAxisZ, !LTools->GetSettings(etAxisZ));
	}

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarIconButton("##AxisZX", m_tZX->get_SRView()->GetRawSRV(), &AxisY, ImDrawFlags_RoundCornersRight))
	{
		ExecCommand(COMMAND_CHANGE_AXIS, etAxisZX, !LTools->GetSettings(etAxisZX));
	}

	ImGui::EndGroup();
	ImGui::NewLine();
}
