#include "stdafx.h"
#include "UIMapWnd.h"
#include "UIMap.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/UIHelper.h"
#include "UITaskWnd.h"
#include "PdaConstants.h"
#include "../../xrEngine/string_table.h"

namespace
{
bool WindowNameEquals(const char* windowName, const char* candidate)
{
	return windowName && candidate && xr_strcmp(windowName, candidate) == 0;
}

constexpr u8 kNavExtraIndexStart = 9;
constexpr u8 kNavExtraIndexMaxExclusive = 32;
constexpr const char* kNavButtonPathFormat = "btn_nav_parent:btn_nav_%d";

CUI3tButton* FindNavButtonByWindowName(CUI3tButton* const* buttons, u32 count, const char* windowName)
{
	for (u32 i = 0; i < count; ++i)
	{
		CUI3tButton* btn = buttons[i];
		if (btn && WindowNameEquals(btn->WindowName().c_str(), windowName))
		{
			return btn;
		}
	}
	return nullptr;
}

bool IsNavPanButtonHeld(CUI3tButton* btn)
{
	return btn && btn->CursorOverWindow() && btn->GetButtonState() == CUIButton::BUTTON_PUSHED;
}

} // namespace

void CUIMapWnd::InitPersonalSpotRmbMode(CUIXml& xml, const char* buttonPath, CUI3tButton* btn)
{
	if (!btn || !buttonPath || !buttonPath[0])
	{
		return;
	}

	const shared_str windowName = btn->WindowName();
	if (!WindowNameEquals(windowName.c_str(), PdaNavButton::PersonalSpot))
	{
		return;
	}

	if (xml.ReadAttribInt(buttonPath, 0, PdaNavButton::PersonalSpotRmbAttrib, 0) != 1)
	{
		return;
	}

	m_personalSpotRmbMode = true;

	const char* hintRmb = xml.ReadAttrib(buttonPath, 0, PdaNavButton::PersonalSpotRmbHintAttrib, nullptr);
	if (hintRmb && hintRmb[0])
	{
		btn->m_hint_text = g_pStringTable->translate(hintRmb);
	}
}

void CUIMapWnd::RegisterNavButtonByName(CUI3tButton* btn)
{
	if (!btn)
	{
		return;
	}

	const shared_str windowName = btn->WindowName();
	const char* n = windowName.c_str();
	if (!n || !n[0])
	{
		return;
	}

	if (WindowNameEquals(n, PdaNavButton::Legend))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnLegend_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::ZoomIn))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomMore_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::Center) || WindowNameEquals(n, "btn_nav_actor"))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnActor_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::ZoomOut))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomLess_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::ZoomReset))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomReset_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::PersonalSpot))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnPersonalSpot_Push));
	}
	else if (WindowNameEquals(n, PdaNavButton::TaskFocus))
	{
		m_btn_nav_task_focus = btn;
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnNavTaskFocus_Push));
	}
	else if (WindowNameEquals(n, "global_map_btn"))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomReset_Push));
	}
	else if (WindowNameEquals(n, "actor_btn"))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnActor_Push));
	}
	else if (WindowNameEquals(n, "zoom_in_btn"))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomMore_Push));
	}
	else if (WindowNameEquals(n, "zoom_out_btn"))
	{
		AddCallback(btn, BUTTON_DOWN, CUIWndCallback::void_function(this, &CUIMapWnd::OnBtnZoomLess_Push));
	}
}

void CUIMapWnd::init_xml_nav(CUIXml& xml, const char* start_from)
{
	if (xml.NavigateToNode("btn_nav_parent"))
	{
		m_btn_nav_parent = UIHelper::CreateStatic(xml, "btn_nav_parent", this);

		VERIFY(hint_wnd);

		auto registerNavButtonByPathLambda = [this, &xml](const char* buttonPath) -> CUI3tButton*
		{
			if (!xml.NavigateToNode(buttonPath))
			{
				return nullptr;
			}

			CUI3tButton* navButton = UIHelper::Create3tButton(xml, buttonPath, m_btn_nav_parent);
			Register(navButton);
			RegisterNavButtonByName(navButton);
			InitPersonalSpotRmbMode(xml, buttonPath, navButton);
			return navButton;
		};

		string64 buf;
		for (u8 i = 0; i < max_btn_nav; ++i)
		{
			xr_sprintf(buf, kNavButtonPathFormat, i);
			m_btn_nav[i] = registerNavButtonByPathLambda(buf);
		}

		for (u8 i = kNavExtraIndexStart; i < kNavExtraIndexMaxExclusive; ++i)
		{
			xr_sprintf(buf, kNavButtonPathFormat, i);
			if (!registerNavButtonByPathLambda(buf))
			{
				break;
			}
		}
	}
	else
	{
		string512 pth;
		xr_strconcat(pth, start_from, ":main_wnd:map_header_frame_line:tool_bar");

		string512 temp;
		auto registerToolbarButtonLambda = [this, &xml, &pth, &temp](u8 navIndex, const char* buttonName)
		{
			m_btn_nav[navIndex] = UIHelper::Create3tButton(xml, xr_strconcat(temp, pth, buttonName), UIMainMapHeader);
			Register(m_btn_nav[navIndex]);
			RegisterNavButtonByName(m_btn_nav[navIndex]);
		};

		registerToolbarButtonLambda(btn_zoom_reset, ":global_map_btn");
		registerToolbarButtonLambda(btn_actor, ":actor_btn");
		registerToolbarButtonLambda(btn_zoom_more, ":zoom_in_btn");
		registerToolbarButtonLambda(btn_zoom_less, ":zoom_out_btn");
	}
}

void CUIMapWnd::UpdateNav()
{
	if (m_btn_nav_parent)
	{
		m_btn_nav_parent->Show(!pInput->GetControllerMode());
	}
	if (Device.dwTimeGlobal - m_nav_timing < 10)
	{
		return;
	}
	m_nav_timing = Device.dwTimeGlobal;

	CUI3tButton* btnUp = FindNavButtonByWindowName(m_btn_nav, max_btn_nav, PdaNavButton::Up);
	CUI3tButton* btnLeft = FindNavButtonByWindowName(m_btn_nav, max_btn_nav, PdaNavButton::Left);
	CUI3tButton* btnRight = FindNavButtonByWindowName(m_btn_nav, max_btn_nav, PdaNavButton::Right);
	CUI3tButton* btnDown = FindNavButtonByWindowName(m_btn_nav, max_btn_nav, PdaNavButton::Down);

	if (IsNavPanButtonHeld(btnUp))
	{
		MoveMap(Fvector2().set(0.0f, m_map_move_step));
	}
	else if (IsNavPanButtonHeld(btnLeft))
	{
		MoveMap(Fvector2().set(m_map_move_step, 0.0f));
	}
	else if (IsNavPanButtonHeld(btnRight))
	{
		MoveMap(Fvector2().set(-m_map_move_step, 0.0f));
	}
	else if (IsNavPanButtonHeld(btnDown))
	{
		MoveMap(Fvector2().set(0.0f, -m_map_move_step));
	}
}

void CUIMapWnd::OnBtnLegend_Push(CUIWindow*, void*)
{
	CUITaskWnd* parent_wnd = smart_cast<CUITaskWnd*>(m_pParentWnd);
	if (parent_wnd)
	{
		parent_wnd->Switch_ShowMapLegend();
	}
}

void CUIMapWnd::OnBtnZoomMore_Push(CUIWindow*, void*)
{
	ViewZoomIn();
}

void CUIMapWnd::OnBtnActor_Push(CUIWindow*, void*)
{
	ViewActor();
}

void CUIMapWnd::OnBtnZoomLess_Push(CUIWindow*, void*)
{
	ViewZoomOut();
}

void CUIMapWnd::OnBtnZoomReset_Push(CUIWindow*, void*)
{
	ViewGlobalMap();
}

void CUIMapWnd::OnBtnPersonalSpot_Push(CUIWindow*, void*)
{
	SetPersonalSpotPlacement(!m_personalSpotPlacement);
}

void CUIMapWnd::OnBtnNavTaskFocus_Push(CUIWindow*, void*)
{
	CUITaskWnd* parentWnd = smart_cast<CUITaskWnd*>(m_pParentWnd);
	if (parentWnd)
	{
		parentWnd->FocusPrimaryTaskOnMap();
	}
}

void CUIMapWnd::UpdateNavTaskFocusVisibility(CGameTask* primaryTask)
{
	if (!m_btn_nav_task_focus)
	{
		return;
	}

	if (!primaryTask || !primaryTask->HasActiveMapTarget())
	{
		m_btn_nav_task_focus->Show(false);
	}
	else
	{
		m_btn_nav_task_focus->Show(true);
	}
}

