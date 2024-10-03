#include "stdafx.h"
#include "UIRenderForm.h"
#include "ui_main.h"
#include "../xrEUI/ImGuizmo.h"

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
}

UIRenderForm::~UIRenderForm()
{

}

void UIRenderForm::Draw()
{
	ImGui::Begin("Render", nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);

	m_render_pos.right = ImGui::GetWindowSize().x;
	m_render_pos.left = ImGui::GetWindowPos().x;

	m_render_pos.bottom = ImGui::GetWindowSize().y;
	m_render_pos.top = ImGui::GetWindowPos().y;

	if (UI && UI->RT->pSurface)
	{
		int ShiftState = ssNone;

		if (ImGui::GetIO().KeyShift)ShiftState |= ssShift;
		if (ImGui::GetIO().KeyCtrl)ShiftState |= ssCtrl;
		if (ImGui::GetIO().KeyAlt)ShiftState |= ssAlt;


		if (ImGui::IsMouseDown(ImGuiMouseButton_Left))ShiftState |= ssLeft;
		if (ImGui::IsMouseDown(ImGuiMouseButton_Right))ShiftState |= ssRight;
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
		UI->RTSize.set(canvas_size.x, canvas_size.y);

		ImGui::SetCursorScreenPos(canvas_pos);
		draw_list->AddImage(UI->RT->pSurface, canvas_pos, ImVec2(canvas_pos.x + canvas_size.x, canvas_pos.y + canvas_size.y));

		if(m_OnToolBar)
            m_OnToolBar(canvas_pos, canvas_size);

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

		if (ImGui::BeginDragDropTarget())
		{
			auto ImData = ImGui::AcceptDragDropPayload("TEST");

			if (ImData != nullptr)
			{
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
				else
				{
					DragFunctor(Data.FileName, 6);
				}
			}
			ImGui::EndDragDropTarget();
		}
	}
	ImGui::End();
}
