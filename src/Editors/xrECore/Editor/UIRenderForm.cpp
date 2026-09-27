#include "stdafx.h"
#include "UIRenderForm.h"
#include "ui_main.h"
#include "../xrEUI/ImGuizmo.h"
#include <imgui_internal.h>

static int GlobalViewportIndex = 0;

namespace ImGui
{
	XREUI_API ImFont* LightFont;
	XREUI_API ImFont* RegularFont;
	XREUI_API ImFont* MediumFont;
	XREUI_API ImFont* BoldFont;
}

UIRenderForm::UIRenderForm()
{
	m_mouse_down = false;
	m_mouse_move = false;
	m_shiftstate_down = false;

	ViewportID = GlobalViewportIndex++;
	sprintf(ViewportName, "%s##%d", "Render", ViewportID);

	if (ViewportID != 0)
	{
		EContext.UI->CreateViewport(ViewportID, this);
	}
	else
	{
		TUI::Viewport& MainView = EContext.UI->CurrentView();
		MainView.ViewGlobalIDX = ViewportID;
		MainView.ViewportForm = this;
	}
}

UIRenderForm::~UIRenderForm()
{
	EContext.UI->DestroyViewport(ViewportID);
}

void UIRenderForm::DrawStatistics()
{
	const float ToolbarHeight = 0
		+ XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding) * 2.f
		+ XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float Gap = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::DockingGap);

	if (!psDeviceFlags.is(rsStatistic))
	{
		return;
	}

	auto Print = [](const char* param, const char* value_fmt, ...)
	{
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::Text("%s:", param);
		ImGui::TableSetColumnIndex(1);
		va_list args;
		va_start(args, value_fmt);
		ImGui::TextV(value_fmt, args);
		va_end(args);
	};

	ImGui::SetCursorPos(ImVec2(ToolbarHeight * 0.5f, ToolbarHeight * 1.5f));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(Gap, Gap));

	if (!ImGui::BeginTable("stats", 2))
	{
		return;
	}
	ImGui::TableSetupColumn("AAA", ImGuiTableColumnFlags_WidthFixed);

	CEStats* s = static_cast<CEStats*>(EDevice->Statistic);

	//color(0xFFFFFFFF);
	Print("FPS/RFPS", "%3.1f/%3.1f", (s->fFPS), s->fRFPS);
	ImGui::NewLine();
	//color(0xDDDDDDDD);
	Print("TPS", "%2.2f M", s->fTPS);

	Print("VERT", "%d",		s->lastDPS_verts);
	Print("POLY", "%d",		s->lastDPS_polys);
	Print("DIP/DP", "%d",	s->lastDPS_calls);

	if (ViewportID == 0 && EPrefs->bMoreStats)
	{
		Print("SH/T/M/C", "%d/%d/%d/%d", s->dwShader_Codes, s->dwShader_Textures, s->dwShader_Matrices, s->dwShader_Constants);
		Print("Skeletons", "%2.2fms, %d", s->Animation.result, s->Animation.count);
		Print("Skinning", "%2.2fms", s->RenderDUMP_SKIN.result);
		ImGui::NewLine();
		Print("Input", "%2.2fms", s->Input.result);
		Print("clRAY", "%2.2fms, %d", s->clRAY.result, s->clRAY.count);
		Print("clBOX", "%2.2fms, %d", s->clBOX.result, s->clBOX.count);
		Print("clFRUSTUM", "%2.2fms, %d", s->clFRUSTUM.result, s->clFRUSTUM.count);
		ImGui::NewLine();
		Print("RT", "%2.2fms, %d", s->RenderDUMP_RT.result, s->RenderDUMP_RT.count);
		Print("DT_Vis", "%2.2fms", s->RenderDUMP_DT_VIS.result);
		Print(" DT_Render", "%2.2fms", s->RenderDUMP_DT_Render.result);
		Print(" DT_Cache", "%2.2fms", s->RenderDUMP_DT_Cache.result);
	}
    if (psDeviceFlags.test(rsEnvironment))
    {
        ImGui::NewLine();
        // color(0xFFC8DCAF);
        Print("GAME TIME", "%02d:%02d:%02d", s->hours, s->minutes, s->seconds);
    }

    ImGui::NewLine();
	Print("Camera Pos", "%2.2f, %2.2f, %2.2f", EContext.UI->CurrentView().m_Camera.GetPosition().x, EContext.UI->CurrentView().m_Camera.GetPosition().y, EContext.UI->CurrentView().m_Camera.GetPosition().z);

	ImGui::EndTable();
	ImGui::PopStyleVar();
}
void UIRenderForm::Draw()
{
	ImGuiWindowClass WndClass;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	WndClass.DockNodeFlagsOverrideSet = ImGuiDockNodeFlags_HiddenTabBar | ImGuiDockNodeFlags_NoDockingOverMe | ImGuiDockNodeFlags_NoDockingOverOther;
	ImGui::SetNextWindowClass(&WndClass);

	if (!ImGui::Begin(ViewportName, nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse))
	{
		ImGui::End();
		ImGui::PopStyleVar();
		return;
	}

	ImGui::PopStyleVar();
	DrawVP();
	ImGui::End();
}

void UIRenderForm::DrawVP()
{
	const float ToolbarHeight = 0
		+ XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding) * 2.f
		+ XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);

	float ScreenDPI = GUIManager->GetScaleDpi();

	if (EContext.UI->Views[ViewportID].ViewGlobalIDX != ViewportID)
	{
		return;
	}

	if (ImGui::IsWindowFocused() || EContext.UI->ViewID == ViewportID)
	{
		if ((EContext.UI->IsPlayInEditor() && ViewportID == 0) || !EContext.UI->IsPlayInEditor())
		{
			EContext.UI->ViewID = ViewportID;

			if (OnFocusCallback)
			{
				OnFocusCallback();
			}
		}
	}

	if ((EContext.UI->IsPlayInEditor() && ViewportID == 0) || EContext.UI->ViewID == ViewportID)
	{
		GRHI->CopySurface(EContext.UI->Views[ViewportID].RTFreez->pRT, EContext.UI->RT->pRT);
	}

	m_render_pos.right = ImGui::GetWindowSize().x;
	m_render_pos.left = ImGui::GetWindowPos().x;

	m_render_pos.bottom = ImGui::GetWindowSize().y;
	m_render_pos.top = ImGui::GetWindowPos().y;

	bool CursorInZone = true;
	if (EContext.UI && EContext.UI->Views[ViewportID].RTFreez->pSurface)
	{
		int ShiftState = ssNone;

		if (ViewportID == EContext.UI->ViewID)
		{
			auto ViewHandle = ImGui::GetWindowViewport();
			EContext.UI->Views[ViewportID].WndHandle = SDL_GetWindowFromID((SDL_WindowID)(size_t)ViewHandle->PlatformHandle);

			if (ImGui::GetIO().KeyShift)ShiftState |= ssShift;
			if (ImGui::GetIO().KeyCtrl)	ShiftState |= ssCtrl;
			if (ImGui::GetIO().KeyAlt)	ShiftState |= ssAlt;

			if (ImGui::IsMouseDown(ImGuiMouseButton_Left))ShiftState |= ssLeft;
			if (ImGui::IsMouseDown(ImGuiMouseButton_Right))ShiftState |= ssRight;
		}

		//VERIFY(!(ShiftState & ssLeft && ShiftState & ssRight));
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 canvas_pos = ImGui::GetCursorScreenPos();
		ImVec2 canvas_size = ImGui::GetContentRegionAvail();
		ImVec2 mouse_pos = ImGui::GetIO().MousePos;
		if (mouse_pos.x < canvas_pos.x)
		{
			CursorInZone = false;
			mouse_pos.x = canvas_pos.x;
		}
		if (mouse_pos.y < canvas_pos.y)
		{
			CursorInZone = false;
			mouse_pos.y = canvas_pos.y;
		}

		if (mouse_pos.x > canvas_pos.x + canvas_size.x)
		{
			CursorInZone = false;
			mouse_pos.x = canvas_pos.x + canvas_size.x;
		}
		if (mouse_pos.y > canvas_pos.y + canvas_size.y)
		{
			CursorInZone = false;
			mouse_pos.y = canvas_pos.y + canvas_size.y;
		}

		bool curent_shiftstate_down = EContext.UI->CurrentView().m_Camera.IsMoving();


		if (canvas_size.x < 32.0f * ScreenDPI) canvas_size.x = 32.0f * ScreenDPI;
		if (canvas_size.y < 32.0f * ScreenDPI) canvas_size.y = 32.0f * ScreenDPI;
		EContext.UI->Views[ViewportID].RTSize.set(canvas_size.x, canvas_size.y);

		ImGui::SetCursorScreenPos(canvas_pos);
		draw_list->AddImage(EContext.UI->Views[ViewportID].RTFreez->pTexture->get_SRView()->GetRawSRV(), canvas_pos, ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y));

		if (ViewportID != EContext.UI->ViewID && ImGui::IsWindowFocused())
		{
			return;
		}

		if (m_OnToolBar)
			m_OnToolBar(canvas_pos, canvas_size);

		if (ViewportID == EContext.UI->ViewID && !EContext.UI->IsPlayInEditor())
		{
			//Statistic
			DrawStatistics();

			if (!psDeviceFlags.test(rsDrawAxis) && !psDeviceFlags.test(rsDisableAxisCube))
			{
				ImGuizmo::SetRect(canvas_pos.x, canvas_pos.y, canvas_size.x, canvas_size.y);
				ImGuizmo::SetDrawlist();
				ImGuizmo::AllowAxisFlip(true);

				float calcSide = (canvas_size.x > canvas_size.y) ? canvas_size.y : canvas_size.x;

				ImVec2 size{ calcSide * 0.15f, calcSide * 0.15f };
				ImVec2 pos{ canvas_pos.x + canvas_size.x - size.x, canvas_pos.y + ToolbarHeight };

				//Device.mView for only read
				Fmatrix TempViewMatrix = Device.mView;
				ImGuizmo::ViewManipulate((float*)&TempViewMatrix, 10, pos, size, ImColor());

				if (ImGuizmo::IsUsingViewManipulate())
				{
					CUI_Camera& Camera = EContext.UI->CurrentView().m_Camera;

					Fvector OldDir = Camera.GetDirection();
					Fvector LookAt;
					LookAt.mad(Camera.GetPosition(), OldDir, 1.0f);

					Fmatrix InvView;
					InvView.invert(TempViewMatrix);

					Fvector Hpb;
					InvView.getHPB(Hpb);

					Fvector NewDir = InvView.k;
					NewDir.normalize();

					float Dist = LookAt.distance_to(Camera.GetPosition());

					Fvector NewPos;
					NewPos.mad(LookAt, NewDir, -Dist);

					Camera.Set(Hpb, NewPos);

					Device.mView.set(TempViewMatrix);
				}
			}
		}

		ImGui::SetCursorScreenPos(canvas_pos);

		if (!ImGuizmo::IsUsing())
			ImGui::InvisibleButton("canvas", canvas_size);

		if (ImGui::IsItemFocused())
		{
			if ((ImGui::IsMouseDown(ImGuiMouseButton_Left) || ImGui::IsMouseDown(ImGuiMouseButton_Right)) && !m_mouse_down && CursorInZone)
			{
				EContext.UI->MousePress(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_down = true;
			}

			else  if ((ImGui::IsMouseReleased(ImGuiMouseButton_Left) || ImGui::IsMouseReleased(ImGuiMouseButton_Right)) && m_mouse_down)
			{
				if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !ImGui::IsMouseDown(ImGuiMouseButton_Right))
				{
					EContext.UI->MouseRelease(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
					m_mouse_down = false;
					m_mouse_move = false;
					m_shiftstate_down = false;
				}
			}
			else if (m_mouse_down)
			{
				EContext.UI->MouseMove(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_move = true;
				m_shiftstate_down = m_shiftstate_down || (ShiftState & (ssShift | ssCtrl | ssAlt));
			}

			if (ImGui::IsMouseReleased(ImGuiMouseButton_Left) && OnClickCallback)
			{
				OnClickCallback();
			}
		}
		else  if (m_mouse_down)
		{
			if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !ImGui::IsMouseDown(ImGuiMouseButton_Right))
			{
				EContext.UI->MouseRelease(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_down = false;
				m_mouse_move = false;
				m_shiftstate_down = false;
			}
		}
		m_mouse_position.set(mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);


		if (!m_OnContextMenu.empty() && !curent_shiftstate_down && !EContext.UI->IsPlayInEditor())
		{
			if (ImGui::BeginPopupContextItem("Menu"))
			{
				m_OnContextMenu();
				ImGui::EndPopup();
			}
			else
			{
				EContext.UI->m_ContextRDir = EContext.UI->m_CurrentRDir;
				EContext.UI->m_ContextRStart = EContext.UI->m_CurrentRStart;
			}
		}

		HandleDragDrop(canvas_pos);
	}

	// MainViewport
	if (ViewportID == 0)
	{
		if (CursorInZone && UseHint)
		{
			EContext.UI->ShowHint();
		}
	}
}

void UIRenderForm::HandleDragDrop(const ImVec2& canvas_pos)
{
	const ImGuiPayload* payload = ImGui::GetDragDropPayload();

	if (payload && ImGui::IsMouseDragging(ImGuiMouseButton_Left) && GUIManager->DnDType == EDragDropType::Viewport)
	{
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 p_min = ImGui::GetItemRectMin();
		ImVec2 p_max = ImGui::GetItemRectMax();
		draw_list->AddRectFilled(p_min, p_max, IM_COL32(50, 50, 70, 100));
		draw_list->AddRect(p_min, p_max, IM_COL32(100, 180, 255, 255));
	}

	if (ViewportID != 0 || !ImGui::BeginDragDropTarget())
		return;

	auto ImData = ImGui::AcceptDragDropPayload("TEST");

	if (ImData == nullptr)
	{
		ImGui::EndDragDropTarget();
		return;
	}

	struct DragDropData
	{
		xr_string FileName;
	} Data = *(DragDropData*)ImData->Data;


	if (Data.FileName.ends_with(".object"))
	{
		DragFunctor(Data.FileName, 2);
	}
	else if (Data.FileName.ends_with(".group"))
	{
		DragFunctor(Data.FileName, 0);
	}
	else if (Data.FileName.ends_with(".r16"))
	{
		DragFunctor(Data.FileName, 17);
	}
	else {
		DragFunctor(Data.FileName, 6);
	}

	ImGui::EndDragDropTarget();
}