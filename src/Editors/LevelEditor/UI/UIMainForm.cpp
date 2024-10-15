#include "stdafx.h"
#include "../XrECore/Editor/EditorChooseEvents.h"
#include "../xrEUI/ImGuizmo.h"
#include "Editor/Utils/Gizmo/IM_Manipulator.h"

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
	m_Properties = new UILPropertiesFrom();
	m_WorldProperties = new UIWorldPropertiesFrom();
	m_Render->SetContextMenuEvent(TOnRenderContextMenu(this, &UIMainForm::DrawContextMenu));
	m_Render->SetToolBarEvent(TOnRenderToolBar(this, &UIMainForm::DrawRenderToolBar));
	m_Render->OnFocusCallback = ViewportFocusCallback;
	if (dynamic_cast<CLevelPreferences*>(EPrefs)->OpenObjectList)
	{
		UIObjectList::Show();
	}
	if (!dynamic_cast<CLevelPreferences*>(EPrefs)->OpenProperties)
	{
		m_Properties->Close();
	}
	if (!dynamic_cast<CLevelPreferences*>(EPrefs)->OpenWorldProperties)
	{
		m_WorldProperties->Close();
	}

	// Action
	m_tMenu         = EDevice->Resources->_CreateTexture("ed\\bar\\menu");
	m_tSelect       = EDevice->Resources->_CreateTexture("ed\\bar\\select");
	m_tAdd          = EDevice->Resources->_CreateTexture("ed\\bar\\add");
	m_tMove         = EDevice->Resources->_CreateTexture("ed\\bar\\move");
	m_tScale        = EDevice->Resources->_CreateTexture("ed\\bar\\scale");
	m_tRotate       = EDevice->Resources->_CreateTexture("ed\\bar\\rotate");

	// Snap
	m_tGSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\gsnap");
	m_tOSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\osnap");
	m_tMoveToSnap   = EDevice->Resources->_CreateTexture("ed\\bar\\movetosnap");
	m_tNSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\nsnap");
	m_tVSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\vsnap");
	m_tASnap        = EDevice->Resources->_CreateTexture("ed\\bar\\asnap");
	m_tMSnap        = EDevice->Resources->_CreateTexture("ed\\bar\\msnap");

	m_tZoom         = EDevice->Resources->_CreateTexture("ed\\bar\\zoom");
	m_tZoomSel      = EDevice->Resources->_CreateTexture("ed\\bar\\zoomsel");

	// Axis
	m_tX            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisX");
	m_tY            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisY");
	m_tZ            = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZ");
	m_tZX           = EDevice->Resources->_CreateTexture("ed\\bar\\AxisZX");
	
	m_tGrid         = EDevice->Resources->_CreateTexture("ed\\bar\\grid");
	m_tScaleGrid    = EDevice->Resources->_CreateTexture("ed\\bar\\scale_grid");
	m_tAngle        = EDevice->Resources->_CreateTexture("ed\\bar\\angle");

	m_tCsLocal      = EDevice->Resources->_CreateTexture("ed\\bar\\cslocal");
	m_tNuScale      = EDevice->Resources->_CreateTexture("ed\\bar\\nuscale");

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
}

UIMainForm::~UIMainForm()
{
	dynamic_cast<CLevelPreferences*>(EPrefs)->OpenProperties = !m_Properties->IsClosed();
	dynamic_cast<CLevelPreferences*>(EPrefs)->OpenWorldProperties = !m_WorldProperties->IsClosed();
	dynamic_cast<CLevelPreferences*>(EPrefs)->OpenObjectList = UIObjectList::IsOpen();
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

	ExecCommand(COMMAND_DESTROY, (u32)0, (u32)0);
}

void UIMainForm::Draw()
{
	bOpen = true;
	m_MainMenu->Draw();
	m_TopBar->Draw();
	m_LeftBar->Draw();
	m_Properties->Draw();
	m_WorldProperties->Draw();
	//static bool Demo = true;
   // ImGui::ShowDemoWindow(&Demo);
	m_Render->Draw();
}

bool UIMainForm::Frame()
{
	if(UI)  return UI->Idle();
	return false;
}

void UIMainForm::DrawContextMenu()
{
	if (ImGui::BeginMenu("Visiblity"))
	{
		if (ImGui::MenuItem("Hide Selected"))
		{
			ExecCommand(COMMAND_HIDE_SEL, FALSE);
		}
		if (ImGui::MenuItem("Hide Unselected"))
		{
			ExecCommand(COMMAND_HIDE_UNSEL);
		}
		if (ImGui::MenuItem("Hide All"))
		{
			ExecCommand(COMMAND_HIDE_ALL, FALSE);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Unhide All"))
		{
			ExecCommand(COMMAND_HIDE_ALL, TRUE);
		}
		ImGui::EndMenu();
	}
	if(ImGui::BeginMenu("Locking"))
	{
		if(ImGui::MenuItem("Lock selection"))
		{
            ExecCommand(COMMAND_LOCK_SEL, TRUE);
		}
        if(ImGui::MenuItem("Lock unselected"))
		{
            ExecCommand(COMMAND_LOCK_UNSEL, TRUE);
		}
        if(ImGui::MenuItem("Lock all"))
		{
			ExecCommand(COMMAND_LOCK_ALL, TRUE);
		}
        ImGui::Separator();
		if(ImGui::MenuItem("Unlock selection"))
		{
			ExecCommand(COMMAND_LOCK_SEL, FALSE);
		}
		if(ImGui::MenuItem("Unlock unselected"))
		{
			ExecCommand(COMMAND_LOCK_UNSEL, FALSE);
		}
		if(ImGui::MenuItem("Unlock all"))
		{
			ExecCommand(COMMAND_LOCK_ALL, FALSE);
		}
		ImGui::EndMenu();
	}

	if (ImGui::BeginMenu("Edit"))
	{
		if (ImGui::MenuItem("Copy"))
		{
			ExecCommand(COMMAND_COPY);
		}
		if (ImGui::MenuItem("Paste"))
		{
			ExecCommand(COMMAND_PASTE);
		}
		if (ImGui::MenuItem("Duplicate"))
		{
			ExecCommand(COMMAND_DUPLICATE);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Cut"))
		{
			ExecCommand(COMMAND_CUT);
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Delete"))
		{
			ExecCommand(COMMAND_DELETE_SELECTION);
		}
		ImGui::EndMenu();
	}
	ImGui::Separator();
	if (ImGui::MenuItem("Properties"))
	{
		ExecCommand(COMMAND_SHOW_PROPERTIES);
	}
}

void UIMainForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	// Меню
	{
		ImGui::BeginGroup();
		m_tMenu->Load();
		{
			if (ImGui::BeginPopupContextItem("MenuScene"))
			{
				{
					bool selected = psDeviceFlags.test(rsDrawSafeRect);
					if (ImGui::MenuItem("Draw Safe Rect", "", &selected))
					{
						psDeviceFlags.set(rsDrawSafeRect, selected);
						UI->RedrawScene();
					}
				}
				{
					bool selected = psDeviceFlags.test(rsDrawGrid);
					if (ImGui::MenuItem("Draw Grid", "", &selected))
					{
						psDeviceFlags.set(rsDrawGrid, selected);
						UI->RedrawScene();
					}
				}
				{
					if (ImGui::BeginMenu("Coordinate Axes"))
					{
						bool disabled = psDeviceFlags.test(rsDisableAxisCube);

						if (ImGui::MenuItem("None", "", &disabled))
						{
							psDeviceFlags.set(rsDisableAxisCube, disabled);
						}

						ImGui::BeginDisabled(disabled);

						bool selected_a = false;
						bool selected_c = false;

						(!psDeviceFlags.test(rsDrawAxis) ? selected_c : selected_a) = true;

						if (ImGui::MenuItem("Axis", "", &selected_a))
						{
							psDeviceFlags.set(rsDrawAxis, true);
						}
						if (ImGui::MenuItem("Cube", "", &selected_c))
						{
							psDeviceFlags.set(rsDrawAxis, false);
						}

						ImGui::EndDisabled();
						ImGui::EndMenu();
					}
				}
				ImGui::Separator();
				{
					bool selected = psDeviceFlags.test(rsFog);
					if (ImGui::MenuItem("Fog", "", &selected))
					{
						psDeviceFlags.set(rsFog, selected);
						UI->RedrawScene();
					}
				}
				{
					if (ImGui::BeginMenu("Environment"))
					{
						bool selected = !psDeviceFlags.test(rsEnvironment);
						if (ImGui::MenuItem("None", "", &selected))
						{
							psDeviceFlags.set(rsEnvironment, false);
							UI->RedrawScene();
						}
						ImGui::Separator();
						for (auto& i : g_pGamePersistent->Environment().WeatherCycles)
						{
							selected = psDeviceFlags.test(rsEnvironment) && i.first == g_pGamePersistent->Environment().CurrentCycleName;
							if (ImGui::MenuItem(i.first.c_str(), "", &selected))
							{
								psDeviceFlags.set(rsEnvironment, true);
								g_pGamePersistent->Environment().SetWeather(i.first.c_str(), true);
								UI->RedrawScene();
							}
						}
						ImGui::EndMenu();
					}
				}
				ImGui::Separator();
				if (ImGui::BeginMenu("Render"))
				{
					if (ImGui::BeginMenu("Quality"))
					{
						static bool selected[4] = { false,false,true,false };
						if (ImGui::MenuItem("25%", "", &selected[0]))
						{
							selected[1] = selected[2] = selected[3] = false;
							UI->SetRenderQuality(1 / 4.f);
							UI->RedrawScene();
						}
						if (ImGui::MenuItem("50%", "", &selected[1]))
						{
							selected[0] = selected[2] = selected[3] = false;
							UI->SetRenderQuality(1 / 2.f);
							UI->RedrawScene();
						}
						if (ImGui::MenuItem("100%", "", &selected[2]))
						{
							selected[1] = selected[0] = selected[3] = false;
							UI->SetRenderQuality(1.f);
							UI->RedrawScene();
						}
						if (ImGui::MenuItem("200%", "", &selected[3]))
						{
							selected[1] = selected[2] = selected[0] = false;
							UI->SetRenderQuality(2.f);
							UI->RedrawScene();
						}
						ImGui::EndMenu();
					}
					if (ImGui::BeginMenu("Fill Mode"))
					{
						bool selected[3] = { EDevice->dwFillMode == D3DFILL_POINT,EDevice->dwFillMode == D3DFILL_WIREFRAME,EDevice->dwFillMode == D3DFILL_SOLID };
						if (ImGui::MenuItem("Point", "", &selected[0]))
						{
							EDevice->dwFillMode = D3DFILL_POINT;
							UI->RedrawScene();
						}
						if (ImGui::MenuItem("Wireframe", "", &selected[1]))
						{
							EDevice->dwFillMode = D3DFILL_WIREFRAME;
							UI->RedrawScene();
						}
						if (ImGui::MenuItem("Solid", "", &selected[2]))
						{
							EDevice->dwFillMode = D3DFILL_SOLID;
							UI->RedrawScene();
						}
						ImGui::EndMenu();
					}
					{
						bool selected = psDeviceFlags.test(rsEdgedFaces);
						if (ImGui::MenuItem("Edged Faces", "", &selected))
						{
							psDeviceFlags.set(rsEdgedFaces, selected);
							UI->RedrawScene();
						}
					}
					{
						bool selected = psDeviceFlags.test(rsLighting);
						if (ImGui::MenuItem("Lighting", "", &selected))
						{
							psDeviceFlags.set(rsLighting, selected);
							UI->RedrawScene();
						}
					}
					ImGui::EndMenu();
				}
				ImGui::Separator();
				{
					bool selected = psDeviceFlags.test(rsMuteSounds);
					if (ImGui::MenuItem("Mute Sounds", "", &selected))
					{
						psDeviceFlags.set(rsMuteSounds, selected);
					}
				}
				{
					bool selected = psDeviceFlags.test(rsRenderRealTime);
					if (ImGui::MenuItem("Real Time", "", &selected))
					{
						psDeviceFlags.set(rsRenderRealTime, selected);
					}
				}
				ImGui::Separator();
				{
					bool selected = psDeviceFlags.test(rsStatistic);
					if (ImGui::MenuItem("Stats", "", &selected)) { psDeviceFlags.set(rsStatistic, selected);  UI->RedrawScene(); }

				}
				ImGui::EndPopup();
			}
			if (ImGui::ImageButton(m_tMenu->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				ImGui::OpenPopup("MenuScene");
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Menu");
			}
		}
		ImGui::EndGroup();
	}
	ImGui::SameLine(0, ImGui::GetFontSize() * 1.5);
	// Action
	{
		ETAction Action = LTools->GetAction();
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
			m_tSelect->Load();
			if (ImGui::ImageButton(m_tSelect->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				LTools->SetAction(etaSelect);
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
		// Add
		{
			bool bPushColor = false;
			if (Action == etaAdd)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tAdd->Load();
			if (ImGui::ImageButton(m_tAdd->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				LTools->SetAction(etaAdd);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Add");
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
			m_tMove->Load();
			if (ImGui::ImageButton(m_tMove->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				LTools->SetAction(etaMove);
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
			m_tScale->Load();
			if (ImGui::ImageButton(m_tScale->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				LTools->SetAction(etaScale);
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
			m_tRotate->Load();
			if (ImGui::ImageButton(m_tRotate->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				LTools->SetAction(etaRotate);
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
	ImGui::SameLine(0, ImGui::GetFontSize() * 1.5);
	// Привязки
	{
		ImGui::BeginGroup();
		// Привязка к объектам
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfOSnap))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tOSnap->Load();
			if (ImGui::ImageButton(m_tOSnap->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfOSnap, !Tools->GetSettings(etfOSnap));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Object Snap Toggle");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Переключатель перемещения привязки к объекту
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfMTSnap))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tMoveToSnap->Load();
			if (ImGui::ImageButton(m_tMoveToSnap->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfMTSnap, !Tools->GetSettings(etfMTSnap));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Moving Snap To Object Toggle");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Привязка к Нормалям
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfNormalAlign))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tNSnap->Load();
			if (ImGui::ImageButton(m_tNSnap->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfNormalAlign, !Tools->GetSettings(etfNormalAlign));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Normal Alignment");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Привязка к сетке
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfGSnap))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tGSnap->Load();
			if (ImGui::ImageButton(m_tGSnap->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfGSnap, !Tools->GetSettings(etfGSnap));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Grid Snap Toggle");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::SameLine();
		// Привязка к вершинам
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfVSnap))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tVSnap->Load();
			if (ImGui::ImageButton(m_tVSnap->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfVSnap, !Tools->GetSettings(etfVSnap));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Vertex Snap Toggle");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::EndGroup();
	}
	ImGui::SameLine(0, ImGui::GetFontSize() * 1.5);
	// --------------------------------------------------------------------------------------------
	// Фокусировка
	{
		ImGui::BeginGroup();
		// Оптимальный вид - вся сцена
		{
			m_tZoom->Load();
			if (ImGui::ImageButton(m_tZoom->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_ZOOM_EXTENTS, FALSE);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Focus the whole scene");
			}
		}
		ImGui::SameLine();
		// Сфокусироваться на выбранном объекте
		{
			m_tZoomSel->Load();
			if (ImGui::ImageButton(m_tZoomSel->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_ZOOM_EXTENTS, TRUE);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Focus on the selected object");
			}
		}
		ImGui::EndGroup();
	}
	ImGui::SameLine(0, ImGui::GetFontSize() * 1.5);
	// --------------------------------------------------------------------------------------------
	// Фиксации манипуляторов
	{
		string_path Temp;
		ImGui::BeginGroup();
		// Move
		{
			{
				bool bPushColor = false;
				if (Tools->GetSettings(etfMSnap))
				{
					bPushColor = true;
					ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
					ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				}
				m_tGrid->Load();
				if (ImGui::ImageButton(m_tGrid->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					ExecCommand(COMMAND_SET_SETTINGS, etfMSnap, !Tools->GetSettings(etfMSnap));
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Fixed object movement");
				}
				if (bPushColor)
				{
					ImGui::PopStyleColor();
					ImGui::PopStyleColor();
				}
			}
			ImGui::SameLine();
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 3.5);
			xr_sprintf(Temp, "%.2f", Tools->m_MoveSnap);
			if (ImGui::BeginCombo("##move", Temp, ImGuiComboFlags_None))
			{
				if (ImGui::Selectable("0.01", false))
				{
					Tools->m_MoveSnap = 0.01f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.05", false))
				{
					Tools->m_MoveSnap = 0.05f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.1", false))
				{
					Tools->m_MoveSnap = 0.1f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.5", false))
				{
					Tools->m_MoveSnap = 0.5f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("1", false))
				{
					Tools->m_MoveSnap = 1.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("2", false))
				{
					Tools->m_MoveSnap = 2.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("5", false))
				{
					Tools->m_MoveSnap = 10.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("25", false))
				{
					Tools->m_MoveSnap = 25.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("50", false))
				{
					Tools->m_MoveSnap = 50.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("100", false))
				{
					Tools->m_MoveSnap = 100.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("250", false))
				{
					Tools->m_MoveSnap = 250.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("500", false))
				{
					Tools->m_MoveSnap = 500.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::EndCombo();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("The choice of a fixed distance of movement of the object");
			}
		}
		ImGui::SameLine(0, ImGui::GetFontSize());
		// --------------------------------------------------------------------------------------------
		// Scale
		{
			{
				bool bPushColor = false;
				if (Tools->GetSettings(etfScaleFixed))
				{
					bPushColor = true;
					ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
					ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				}
				m_tScaleGrid->Load();
				if (ImGui::ImageButton(m_tScaleGrid->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					ExecCommand(COMMAND_SET_SETTINGS, etfScaleFixed, !Tools->GetSettings(etfScaleFixed));
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Fixed Object Scaling");
				}
				if (bPushColor)
				{
					ImGui::PopStyleColor();
					ImGui::PopStyleColor();
				}
			}
			ImGui::SameLine();
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 3);
			xr_sprintf(Temp, "%.2f", Tools->m_ScaleFixed);
			if (ImGui::BeginCombo("##scale", Temp, ImGuiComboFlags_None))
			{
				if (ImGui::Selectable("0.01", false))
				{
					Tools->m_ScaleFixed = 0.01f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.05", false))
				{
					Tools->m_ScaleFixed = 0.05f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.1", false))
				{
					Tools->m_ScaleFixed = 0.1f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.25", false))
				{
					Tools->m_ScaleFixed = 0.25f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("0.5", false))
				{
					Tools->m_ScaleFixed = 0.5f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("1", false))
				{
					Tools->m_ScaleFixed = 1.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("2", false))
				{
					Tools->m_ScaleFixed = 2.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("5", false))
				{
					Tools->m_ScaleFixed = 5.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("10", false))
				{
					Tools->m_ScaleFixed = 10.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("50", false))
				{
					Tools->m_ScaleFixed = 50.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("200", false))
				{
					Tools->m_ScaleFixed = 200.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("1000", false))
				{
					Tools->m_ScaleFixed = 1000.f;
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::EndCombo();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Setting a Fixed Object Scaling");
			}
		}
		ImGui::SameLine(0, ImGui::GetFontSize());
		// --------------------------------------------------------------------------------------------
		// ROTATE
		{
			{
				bool bPushColor = false;
				if (Tools->GetSettings(etfASnap))
				{
					bPushColor = true;
					ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
					ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				}
				m_tAngle->Load();
				if (ImGui::ImageButton(m_tAngle->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					ExecCommand(COMMAND_SET_SETTINGS, etfASnap, !Tools->GetSettings(etfASnap));
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SetTooltip("Fixed object rotation angle");
				}
				if (bPushColor)
				{
					ImGui::PopStyleColor();
					ImGui::PopStyleColor();
				}
			}
			ImGui::SameLine();
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 3);
			xr_sprintf(Temp, "%.f", rad2deg(Tools->m_RotateSnapAngle));
			if (ImGui::BeginCombo("##rotate", Temp, ImGuiComboFlags_None))
			{
				if (ImGui::Selectable("1", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(1.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("5", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(5.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("10", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(10.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("15", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(15.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("45", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(45.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("90", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(90.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				if (ImGui::Selectable("180", false))
				{
					Tools->m_RotateSnapAngle = deg2rad(180.f);
				}
				if (ImGui::IsItemHovered())
					ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::EndCombo();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Set a fixed rotation angle of the object (in degrees)");
			}
		}
		ImGui::EndGroup();
	}
	// --------------------------------------------------------------------------------------------
	ImGui::NewLine();
	// --------------------------------------------------------------------------------------------
	// прочее...
	{
		ImGui::BeginGroup();
		// Parent CS Toggle
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfCSParent))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tCsLocal->Load();
			if (ImGui::ImageButton(m_tCsLocal->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfCSParent, !Tools->GetSettings(etfCSParent));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Parent CS Toggle");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::Spacing();
		// --------------------------------------------------------------------------------------------
		// Маштабирование по осям
		{
			bool bPushColor = false;
			if (Tools->GetSettings(etfNUScale))
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tNuScale->Load();
			if (ImGui::ImageButton(m_tNuScale->pSurface, ImVec2(16, ImGui::GetFontSize()), ImVec2(0, 0), ImVec2(0.5f, 1.f)))
			{
				ExecCommand(COMMAND_SET_SETTINGS, etfNUScale, !Tools->GetSettings(etfNUScale));
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Scaling by Axes only");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::EndGroup();
	}
	ImGui::NewLine();
	// --------------------------------------------------------------------------------------------
	// Выбор Осей.
	if (EPrefs->ShowAxisButtons)
		RenderAxisButtons();

	// --------------------------------------------------------------------------------------------
	// View
	if (EPrefs->ShowOldCameraButtons)
		RenderOldCameraButtons();

	// Gizmo
	if (UI->ViewID == 0)
		imManipulator.Render(Pos.x, Pos.y, Size.x, Size.y);
}

void UIMainForm::RenderOldCameraButtons()
{
	{
		ImGui::BeginGroup();
		// Вид спереди.
		{
			m_tVFront->Load();
			{
				if (ImGui::ImageButton(m_tVFront->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewFront();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Front View");
			}
		}
		ImGui::Spacing();
		// Вид сзади.
		{
			m_tVBack->Load();
			{
				if (ImGui::ImageButton(m_tVBack->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewBack();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Back View");
			}
		}
		ImGui::Spacing();
		// Вид слева.
		{
			m_tVLeft->Load();
			{
				if (ImGui::ImageButton(m_tVLeft->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewLeft();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Left View");
			}
		}
		ImGui::Spacing();
		// Вид справа.
		{
			m_tVRight->Load();
			{
				if (ImGui::ImageButton(m_tVRight->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewRight();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Right View");
			}
		}
		ImGui::Spacing();
		// Вид сверху.
		{
			m_tVTop->Load();
			{
				if (ImGui::ImageButton(m_tVTop->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewTop();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Top View");
			}
		}
		ImGui::Spacing();
		// Вид снизу.
		{
			m_tVBottom->Load();
			{
				if (ImGui::ImageButton(m_tVBottom->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewBottom();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Bottom View");
			}
		}
		ImGui::Spacing();
		// Сбросить Вид.
		{
			m_tVReset->Load();
			{
				if (ImGui::ImageButton(m_tVReset->pSurface, ImVec2(16, ImGui::GetFontSize())))
				{
					UI->CurrentView().m_Camera.ViewReset();
					UI->RedrawScene();
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Reset View");
			}
		}
		ImGui::EndGroup();
	}
	ImGui::NewLine();
	// --------------------------------------------------------------------------------------------
	// Camera
	{
		ImGui::BeginGroup();
		ECameraStyle Camera = UI->CurrentView().m_Camera.GetStyle();
		// Свободный режим камеры
		{
			bool bPushColor = false;
			if (Camera == csPlaneMove)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tPlaneMove->Load();
			if (ImGui::ImageButton(m_tPlaneMove->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				UI->CurrentView().m_Camera.SetStyle(csPlaneMove);
				UI->RedrawScene();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Free camera mode");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::Spacing();
		// Привязка камеры к центру координат|сцены
		{
			bool bPushColor = false;
			if (Camera == cs3DArcBall)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tArcBall->Load();
			if (ImGui::ImageButton(m_tArcBall->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				UI->CurrentView().m_Camera.SetStyle(cs3DArcBall);
				UI->RedrawScene();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Snap the camera to the center of coordinates|scene");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::Spacing();
		// Автооблёт сцены камерой
		{
			bool bPushColor = false;
			if (Camera == csFreeFly)
			{
				bPushColor = true;
				ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
				ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_CheckMark));
			}
			m_tFreeFly->Load();
			if (ImGui::ImageButton(m_tFreeFly->pSurface, ImVec2(16, ImGui::GetFontSize())))
			{
				UI->CurrentView().m_Camera.SetStyle(csFreeFly);
				UI->RedrawScene();
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
				ImGui::SetTooltip("Automatic camera flyover of the scene");
			}
			if (bPushColor)
			{
				ImGui::PopStyleColor();
				ImGui::PopStyleColor();
			}
		}
		ImGui::EndGroup();
	}
}

void UIMainForm::RenderAxisButtons()
{
	ImGui::BeginGroup();
	// --------------------------------------------------------------------------------------------
	ETAxis Axis = LTools->GetAxis();
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
		if (ImGui::ImageButton(m_tX->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisX, !LTools->GetSettings(etAxisX));
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
		if (ImGui::ImageButton(m_tY->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisY, !LTools->GetSettings(etAxisY));
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
		if (ImGui::ImageButton(m_tZ->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisZ, !LTools->GetSettings(etAxisZ));
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
		if (ImGui::ImageButton(m_tZX->pSurface, ImVec2(16, ImGui::GetFontSize())))
		{
			ExecCommand(COMMAND_CHANGE_AXIS, etAxisZX, !LTools->GetSettings(etAxisZX));
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
