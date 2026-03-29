#include "stdafx.h"
#include <imgui.h>
#include <imgui_internal.h>
#include "imgui_EditorEx.h"
#include "IconsFontAwesome7.h"
#include "ModernUI.h"

bool BeginMenuBar(float off)
{
	ImGuiWindow* window = ImGui::GetCurrentWindow();
	if (window->SkipItems)
		return false;
	IM_ASSERT(!window->DC.MenuBarAppending);
	ImGui::BeginGroup();
	const float border_top = ImMax(IM_ROUND(window->WindowBorderSize * 0.5f), 0.0f);
	const float border_half = IM_ROUND(window->WindowBorderSize * 0.5f);
	ImRect bar_rect = window->MenuBarRect();
	ImRect clip_rect(
		ImFloor(bar_rect.Min.x + border_half),
		ImFloor(bar_rect.Min.y + border_top),
		ImFloor(ImMax(bar_rect.Min.x, bar_rect.Max.x - ImMax(window->WindowRounding, border_half))),
		ImFloor(bar_rect.Max.y + off));
	clip_rect.ClipWith(window->OuterRectClipped);
	ImGui::PushClipRect(clip_rect.Min, clip_rect.Max, false);

	window->DC.LayoutType = ImGuiLayoutType_Horizontal;
	window->DC.NavLayerCurrent = ImGuiNavLayer_Main;
	window->DC.MenuBarAppending = true;
	return true;
}

void EndMenuBar()
{
	ImGuiWindow* window = ImGui::GetCurrentWindow();
	if (window->SkipItems)
		return;

	ImGuiContext& g = *GImGui;

	IM_MSVC_WARNING_SUPPRESS(6011); // Static Analysis false positive "warning C6011: Dereferencing NULL pointer 'window'"
	IM_ASSERT(window->DC.MenuBarAppending);

	// Nav: When a move request within one of our child menu failed, capture the request to navigate among our siblings.
	if (ImGui::NavMoveRequestButNoResultYet() && (g.NavMoveDir == ImGuiDir_Left || g.NavMoveDir == ImGuiDir_Right) && (g.NavWindow->Flags & ImGuiWindowFlags_ChildMenu))
	{
		// Try to find out if the request is for one of our child menu
		ImGuiWindow* nav_earliest_child = g.NavWindow;
		while (nav_earliest_child->ParentWindow && (nav_earliest_child->ParentWindow->Flags & ImGuiWindowFlags_ChildMenu))
			nav_earliest_child = nav_earliest_child->ParentWindow;
		if (nav_earliest_child->ParentWindow == window && nav_earliest_child->DC.ParentLayoutType == ImGuiLayoutType_Horizontal && (g.NavMoveFlags & ImGuiNavMoveFlags_Forwarded) == 0)
		{
			// To do so we claim focus back, restore NavId and then process the movement request for yet another frame.
			// This involve a one-frame delay which isn't very problematic in this situation. We could remove it by scoring in advance for multiple window (probably not worth bothering)
			const ImGuiNavLayer layer = ImGuiNavLayer_Main;
			IM_ASSERT(window->DC.NavLayersActiveMaskNext & (1 << layer)); // Sanity check (FIXME: Seems unnecessary)
			ImGui::FocusWindow(window);
			ImGui::SetNavID(window->NavLastIds[layer], layer, 0, window->NavRectRel[layer]);
			// FIXME-NAV: How to deal with this when not using g.IO.ConfigNavCursorVisibleAuto?
			if (g.NavCursorVisible)
			{
				g.NavCursorVisible = false; // Hide nav cursor for the current frame so we don't see the intermediary selection. Will be set again
				g.NavCursorHideFrames = 2;
			}
			g.NavHighlightItemUnderNav = g.NavMousePosDirty = true;
			ImGui::NavMoveRequestForward(g.NavMoveDir, g.NavMoveClipDir, g.NavMoveFlags, g.NavMoveScrollFlags); // Repeat
		}
	}

	ImGui::PopClipRect();
	ImGui::EndGroup();
	window->DC.LayoutType = ImGuiLayoutType_Vertical;
	window->DC.MenuBarAppending = false;
}

#define dbg_draw_tmenu 0

ECORE_API bool IXBeginMainMenuBar()
{
	float LogoSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::IconSize) * 2;
	float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);

	float UIMainMenuSize = UI->GetMenuBarHeight();
	ImGuiViewport* viewport = ImGui::GetMainViewport();

	ImGuiStyle& style = ImGui::GetStyle();

	const float text_line_h = ImGui::GetTextLineHeight();
	const float font_size = ImGui::GetFontSize();

	ImVec2 LogoButtonSize = ImVec2(UIMainMenuSize, UIMainMenuSize);


	ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y));
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, UIMainMenuSize));

	ImGuiWindowFlags window_flags = 0
		| ImGuiWindowFlags_NoDocking
		| ImGuiWindowFlags_NoTitleBar
		| ImGuiWindowFlags_NoResize
		| ImGuiWindowFlags_NoMove
		| ImGuiWindowFlags_NoScrollbar
		| ImGuiWindowFlags_NoScrollWithMouse
		| ImGuiWindowFlags_NoBringToFrontOnFocus
		;

	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);


	ImGui::PushStyleColor(ImGuiCol_WindowBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::BackgroundTint).Value);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.f, 0.f, 0.f, 0.f));
	ImGui::PushStyleColor(ImGuiCol_Border, { 0.f,0.f,0.f,0.f });
	ImGui::PushStyleColor(ImGuiCol_BorderShadow, { 0.f,0.f,0.f,0.f });

	if (!ImGui::Begin("##ChezzeTopMenu", NULL, window_flags))
	{
		ImGui::PopStyleVar(3);
		ImGui::PopStyleColor(4);
		return false;
	}

	
	ImGui::SetCursorPos({0,0});

#if dbg_draw_tmenu
	ImGui::PushStyleColor(ImGuiCol_ChildBg, { 255.f,0.f,0.f,0.2f });
#endif

	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(8.0f, 4.0f)); // : L/R=8, T/B=4
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(8.0f, 10.0f));  // : L/R=0, T/B=8
	const float frame_padding_y = style.FramePadding.y;



	ImVec2 b_content_size = ImGui::GetContentRegionAvail();
	b_content_size.y += style.FramePadding.y;
	b_content_size.x += style.FramePadding.x;
	ImGui::BeginChild("##MENUBAR", b_content_size);

	auto WindowPadding = style.WindowPadding;

	//TopBar logo
	{
		ImVec2 t_pose = { (LogoButtonSize.x - LogoSize) / 2, (LogoButtonSize.y - LogoSize) / 2};
		ImGui::SetCursorPos(t_pose);
		ImGui::Image(UI->m_HeaderLogo->get_SRView()->GetRawSRV(), { LogoSize, LogoSize });
		ImGui::SameLine();
	}

	auto o_cur = ImGui::GetCursorPos();

	const float result = UIMainMenuSize - font_size - frame_padding_y * 2.f;
	const float offset_y = result * 0.5f - text_line_h + frame_padding_y * 2.0f;

	{
		ImGui::SetCursorPos({ o_cur.x, result });
		ImVec2 padding = ImVec2(XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonPaddingW), XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonPaddingH));

		if (!UI->GeneralTabs.empty() && ImGui::BeginTabBar("#TopBarView"))
		{
			for (const auto& [Name, Callback] : UI->GeneralTabs)
			{
				bool ChangedColor = false;
				if (Callback != nullptr && Callback())
				{
					ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 20, 20, 255));
					ChangedColor = true;
				}

				if (ImGui::BeginTabItem(*Name, nullptr, ImGuiTabItemFlags_SetSelected))
				{
					ImGui::EndTabItem();
				}

				if (ChangedColor)
				{
					ImGui::PopStyleColor();
				}
			}

			ImGui::EndTabBar();
		}
	}
	ImGui::SetCursorPos(o_cur);

	if (!BeginMenuBar(offset_y+ImGui::GetTextLineHeight() + style.FramePadding.y * 2.0f))
	{
		ImGui::PopStyleVar(3);
		ImGui::PopStyleColor(4);
		ImGui::End();
		return false;
	}
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, style.FramePadding);

	if (offset_y > 0.0f)
		ImGui::SetCursorPos({ o_cur.x + style.FramePadding.y, offset_y });

	return true;
}

ECORE_API void IXEndMainMenuBar()
{
	ImGui::PopStyleVar(1);
#if dbg_draw_tmenu
	ImGui::PopStyleColor(1);
#endif
	EndMenuBar();
	ImGui::EndChild();
	ImGui::PopStyleVar(2);
	
	ImGuiStyle& style = ImGui::GetStyle();

	float UIMainMenuSize = UI->GetMenuBarHeight();
	float button_h = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	float button_w = button_h * 2.f;
	float IconSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::IconSize) / 2.f;

	bool MaxBut = false;
	bool MoveWin = false;
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	SDL_Event Event;

	ImVec2 dragZoneSize = ImVec2(ImGui::GetContentRegionAvail().x+ style.WindowPadding.x /*- button_w*3*/, ImGui::GetContentRegionAvail().x);
	ImGui::SetCursorPosY(0.f);
	
	auto h_id = ImGui::GetHoveredID();
	bool iih = ImGui::IsItemHovered();

	if (iih && h_id == 0 &&
		ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		MaxBut = true;
	/*
	if (EDevice->isZoomed && h_id == 0    && iih && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		MaxBut = true;
		MoveWin = true;
	}
	else */
		if (!EDevice->isZoomed && h_id == 0 && iih && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		MoveWin = true;

#if dbg_draw_tmenu
	ImGui::Begin("dbg_hwr");
	ImGui::Text("h_id = %d", h_id);
	ImGui::Text("iih = %d", iih);
	ImGui::End();
#endif

	{
		ImVec2 ControlButtonSize = ImVec2(button_w, button_h);
		ImVec2 ImageSize = ImVec2(IconSize, IconSize);

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2((ControlButtonSize.x - ImageSize.x) / 2, (ControlButtonSize.y - ImageSize.y) / 2));

		ImGui::SetCursorPos({ ImGui::GetContentRegionMax().x - button_w * 3 + style.WindowPadding.x, 0 });

		ImGui::BeginChild("##ControlButtons", { button_w * 3,button_h });

		if (ImGui::ImageButton("##IXEndMainMenuBar01", UI->m_WinMin->get_SRView()->GetRawSRV(), ImageSize))
			SendMessageW(EDevice->GetHWND(), WM_SYSCOMMAND, SC_MINIMIZE, 0);

		ImGui::SameLine();

		if (ImGui::ImageButton("##IXEndMainMenuBar02", (EDevice->isZoomed ? UI->m_WinRes->get_SRView()->GetRawSRV() : UI->m_WinMax->get_SRView()->GetRawSRV()), ImageSize))
			MaxBut = true;

		ImGui::SameLine();

		ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImVec4(81.f/255.f,36.f/255.f,40.f/255,1.f));
		ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImVec4(71.f/255.f,24.f/255.f,28.f/255,1.f));

		if (ImGui::ImageButton("##IXEndMainMenuBar03", UI->m_WinClose->get_SRView()->GetRawSRV(), ImageSize))
			SendMessageW(EDevice->GetHWND(), WM_CLOSE, 0, 0);

		ImGui::PopStyleColor(2);

		ImGui::EndChild();

		ImGui::PopStyleVar();

		if (MaxBut)
		{
			if (EDevice->isZoomed)
			{
				EDevice->ResoreWindow(MoveWin);
			}
			else
			{
				EDevice->MaximizedWindow();
			}
		}

		if (MoveWin)
		{
			ReleaseCapture();
			SendMessageW(EDevice->GetHWND(), 0xA1, 0x2, 0);
		}

		ImGui::PopStyleVar();

	}
	ImGui::PopStyleVar(3);
	ImGui::PopStyleColor(4);
	ImGui::End();
}
