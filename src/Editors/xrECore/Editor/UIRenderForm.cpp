#include "stdafx.h"
#include "UIRenderForm.h"
#include "ui_main.h"
#include "../xrEUI/ImGuizmo.h"

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
		UI->CreateViewport(ViewportID);
	}
}

UIRenderForm::~UIRenderForm()
{
}

void UIRenderForm::DrawStatistics()
{
	if (!psDeviceFlags.is(rsStatistic))
		return;

	auto print = [](const char* param, const char* value_fmt, ...)
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

	ImGui::SetCursorPos(ImVec2(48, 48));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(4.0f, .5f));

	if (!ImGui::BeginTable("stats", 2))
	{
		return;
	}
	ImGui::TableSetupColumn("AAA", ImGuiTableColumnFlags_WidthFixed);

	CEStats* s = static_cast<CEStats*>(EDevice->Statistic);

	//color(0xFFFFFFFF);
	print("FPS/RFPS", "%3.1f/%3.1f", (s->fFPS), s->fRFPS);
	ImGui::NewLine();
	//color(0xDDDDDDDD);
	print("TPS", "%2.2f M", s->fTPS);

	print("VERT", "%d",		s->lastDPS_verts);
	print("POLY", "%d",		s->lastDPS_polys);
	print("DIP/DP", "%d",	s->lastDPS_calls);

	if (ViewportID == 0)
	{
		print("SH/T/M/C", "%d/%d/%d/%d", s->dwShader_Codes, s->dwShader_Textures, s->dwShader_Matrices, s->dwShader_Constants);
		print("Skeletons", "%2.2fms, %d", s->Animation.result, s->Animation.count);
		print("Skinning", "%2.2fms", s->RenderDUMP_SKIN.result);
		ImGui::NewLine();
		print("Input", "%2.2fms", s->Input.result);
		print("clRAY", "%2.2fms, %d", s->clRAY.result, s->clRAY.count);
		print("clBOX", "%2.2fms, %d", s->clBOX.result, s->clBOX.count);
		print("clFRUSTUM", "%2.2fms, %d", s->clFRUSTUM.result, s->clFRUSTUM.count);
		ImGui::NewLine();
		print("RT", "%2.2fms, %d", s->RenderDUMP_RT.result, s->RenderDUMP_RT.count);
		print("DT_Vis", "%2.2fms", s->RenderDUMP_DT_VIS.result);
		print(" DT_Render", "%2.2fms", s->RenderDUMP_DT_Render.result);
		print(" DT_Cache", "%2.2fms", s->RenderDUMP_DT_Cache.result);
		ImGui::NewLine();
		print("TEST 0", "%2.2fms, %d", s->TEST0.result, s->TEST0.count);
		print("TEST 1", "%2.2fms, %d", s->TEST1.result, s->TEST1.count);
		print("TEST 2", "%2.2fms, %d", s->TEST2.result, s->TEST2.count);
		print("TEST 3", "%2.2fms, %d", s->TEST3.result, s->TEST3.count);
	}

	ImGui::EndTable();
	ImGui::PopStyleVar();
}
void UIRenderForm::Draw()
{
	if (!ImGui::Begin(ViewportName, nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse))
	{
		ImGui::End();
		return;
	}

	if (ImGui::IsWindowFocused() || UI->ViewID == ViewportID)
	{
		if ((UI->IsPlayInEditor() && ViewportID == 0) || !UI->IsPlayInEditor())
		{
			UI->ViewID = ViewportID;

			if (OnFocusCallback)
			{
				OnFocusCallback();
			}
		}
	}

	if ((UI->IsPlayInEditor() && ViewportID == 0) || UI->ViewID == ViewportID)
	{
		RDevice->StretchRect(UI->RT->pRT, 0, UI->Views[ViewportID].RTFreez->pRT, 0, D3DTEXF_NONE);
	}

	m_render_pos.right = ImGui::GetWindowSize().x;
	m_render_pos.left = ImGui::GetWindowPos().x;

	m_render_pos.bottom = ImGui::GetWindowSize().y;
	m_render_pos.top = ImGui::GetWindowPos().y;

	if (UI && UI->Views[ViewportID].RTFreez->pSurface)
	{
		int ShiftState = ssNone;

		if (ViewportID == UI->ViewID)
		{
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
		bool cursor_in_zone = true;
		if (mouse_pos.x < canvas_pos.x)
		{
			cursor_in_zone = false;
			mouse_pos.x = canvas_pos.x;
		}
		if (mouse_pos.y < canvas_pos.y)
		{
			cursor_in_zone = false;
			mouse_pos.y = canvas_pos.y;
		}

		if (mouse_pos.x > canvas_pos.x + canvas_size.x)
		{
			cursor_in_zone = false;
			mouse_pos.x = canvas_pos.x + canvas_size.x;
		}
		if (mouse_pos.y > canvas_pos.y + canvas_size.y)
		{
			cursor_in_zone = false;
			mouse_pos.y = canvas_pos.y + canvas_size.y;
		}

		bool curent_shiftstate_down = m_shiftstate_down;


		if (canvas_size.x < 32.0f) canvas_size.x = 32.0f;
		if (canvas_size.y < 32.0f) canvas_size.y = 32.0f;
		UI->Views[ViewportID].RTSize.set(canvas_size.x, canvas_size.y);

		ImGui::SetCursorScreenPos(canvas_pos);
		draw_list->AddImage(UI->Views[ViewportID].RTFreez->pSurface, canvas_pos, ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y));

		if (ViewportID != UI->ViewID && ImGui::IsWindowFocused())
		{
			ImGui::End();
			return;
		}

		if(m_OnToolBar)
			m_OnToolBar(canvas_pos, canvas_size);

		if (ViewportID == UI->ViewID && !UI->IsPlayInEditor())
		{
			//Statistic
			DrawStatistics();

			if (!psDeviceFlags.test(rsDrawAxis) && !psDeviceFlags.test(rsDisableAxisCube))
			{
				ImGuizmo::SetRect(canvas_pos.x, canvas_pos.y, canvas_size.x, canvas_size.y);
				ImGuizmo::SetDrawlist();
				ImGuizmo::AllowAxisFlip(true);

				float calcSide = (canvas_size.x > canvas_size.y) ? canvas_size.y : canvas_size.x;

				ImVec2 size{ calcSide*0.15f, calcSide * 0.15f };
				ImVec2 pos{ canvas_pos.x + canvas_size.x - size.x, canvas_pos.y };

				//Device.mView for only read
				ImGuizmo::ViewManipulate((float*)&Device.mView, 10, pos, size, ImColor());
			}
		}

		ImGui::SetCursorScreenPos(canvas_pos);

		if (!ImGuizmo::IsUsing())
			ImGui::InvisibleButton("canvas", canvas_size);

		if (ImGui::IsItemFocused())
		{

			if ((ImGui::IsMouseDown(ImGuiMouseButton_Left) || ImGui::IsMouseDown(ImGuiMouseButton_Right)) && !m_mouse_down&& cursor_in_zone)
			{
				UI->MousePress(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_down = true;
			}

			else  if ((ImGui::IsMouseReleased(ImGuiMouseButton_Left) || ImGui::IsMouseReleased(ImGuiMouseButton_Right) )&& m_mouse_down)
			{
				if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) &&! ImGui::IsMouseDown(ImGuiMouseButton_Right))
				{
					UI->MouseRelease(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
					m_mouse_down = false;
					m_mouse_move = false;
					m_shiftstate_down = false;
				}
			}
			else if (m_mouse_down)
			{
				UI->MouseMove(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_move = true;
				m_shiftstate_down = m_shiftstate_down||( ShiftState & (ssShift | ssCtrl | ssAlt));
			}
		}
		else  if (m_mouse_down)
		{
			if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !ImGui::IsMouseDown(ImGuiMouseButton_Right))
			{
				UI->MouseRelease(TShiftState(ShiftState), mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);
				m_mouse_down = false;
				m_mouse_move = false;
				m_shiftstate_down = false;
			}
		}
		m_mouse_position.set(mouse_pos.x - canvas_pos.x, mouse_pos.y - canvas_pos.y);


		if (!m_OnContextMenu.empty()&& !curent_shiftstate_down && !UI->IsPlayInEditor())
		{
			if (ImGui::BeginPopupContextItem("Menu"))
			{
				m_OnContextMenu();
				ImGui::EndPopup();
			}
		}

		HandleDragDrop(canvas_pos);
	}
	ImGui::End();
}

void UIRenderForm::HandleDragDrop(const ImVec2& canvas_pos)
{
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
	else {
		DragFunctor(Data.FileName, 6);
	}

	ImGui::EndDragDropTarget();
}