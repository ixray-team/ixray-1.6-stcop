#include "stdafx.h"
#include "UIWindow.h"
#include "UIFrameWindow.h"
#include "UIFrameLineWnd.h"
#include "UIDialogWnd.h"
#include "UIDialogHolder.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "UITextureMaster.h"
#include "UIScrollView.h"
#include "../UICursor.h"
#include <luabind/luabind.hpp>
#include <luabind/adopt_policy.hpp>
#include "UIHint.h"

CFontManager& mngr()
{
	return UI().Font();
}

// hud font
CGameFont* GetFontSmall()
{
	return mngr().pFontStat;
}

CGameFont* GetFontMedium()
{
	return mngr().pFontMedium;
}
CGameFont* GetFontDI()
{
	return mngr().pFontDI;
}

//шрифты для интерфейса
CGameFont* ui_font_arial_14()
{
	static shared_str FontName = "ui_font_arial_14";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_arial_21()
{
	static shared_str FontName = "ui_font_arial_21";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_graffiti19_russian()
{
	static shared_str FontName = "ui_font_graffiti19_russian";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_graffiti22_russian()
{
	static shared_str FontName = "ui_font_graffiti22_russian";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_graffiti32_russian()
{
	static shared_str FontName = "ui_font_graffiti32_russian";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_graffiti50_russian()
{
	static shared_str FontName = "ui_font_graffiti50_russian";
	return UI().Font().GetFont(FontName);
}

CGameFont* ui_font_letterica16_russian()
{
	return UI().Font().pFontSystem16;
}

CGameFont* ui_font_letterica18_russian()
{
	return UI().Font().pFontSystem;
}

CGameFont* ui_font_letter_25()
{
	static shared_str FontName = "ui_font_letter_25";
	return UI().Font().GetFont(FontName);
}


int GetARGB(u16 a, u16 r, u16 g, u16 b)
{return color_argb(a,r,g,b);}

const Fvector2 get_wnd_pos(CUIWindow* w) {
	return w->GetWndPos();
}

Fvector2 GetCursorPosition_script()
{
	return GetUICursor().GetCursorPosition();
}

void SetCursorPosition_script(Fvector2& pos)
{
	GetUICursor().SetUICursorPosition(pos);
}
using namespace luabind;
#pragma optimize("s",on)
void CUIWindow::script_register(lua_State *L)
{
	module(L)
	[
		def("GetARGB",					&GetARGB),
		def("GetFontSmall",				&GetFontSmall),
		def("GetFontMedium",			&GetFontMedium),
		def("GetFontDI",				&GetFontDI),
		def("GetFontArial14",				&ui_font_arial_14			),
		def("GetFontArial21",				&ui_font_arial_21			),
		def("GetFontGraffiti19Russian",		&ui_font_graffiti19_russian	),
		def("GetFontGraffiti22Russian",		&ui_font_graffiti22_russian	),
		def("GetFontGraffiti32Russian",		&ui_font_graffiti32_russian	),
		def("GetFontGraffiti50Russian",		&ui_font_graffiti50_russian	),
		def("GetFontLetterica16Russian",	&ui_font_letterica16_russian),
		def("GetFontLetterica18Russian",	&ui_font_letterica18_russian),
		def("GetFontLetterica25",			&ui_font_letter_25),
		def("GetCursorPosition",			&GetCursorPosition_script),
		def("SetCursorPosition",			&SetCursorPosition_script),
		def("FitInRect",					&fit_in_rect),

		class_<CUIWindow>("CUIWindow")
		.def(							constructor<>())
		.def("AttachChild",				&CUIWindow::AttachChild, adopt<2>())
		.def("DetachChild",				&CUIWindow::DetachChild)
		.def("SetAutoDelete",			&CUIWindow::SetAutoDelete)
		.def("IsAutoDelete",			&CUIWindow::IsAutoDelete)
			
		.def("IsCursorOverWindow",		&CUIWindow::CursorOverWindow)
		.def("FocusReceiveTime",		&CUIWindow::FocusReceiveTime)
		.def("GetAbsoluteRect",			&CUIWindow::GetAbsoluteRect)

		.def("SetWndRect",				(void (CUIWindow::*)(Frect))	&CUIWindow::SetWndRect_script)
		.def("SetWndPos",				(void (CUIWindow::*)(Fvector2)) &CUIWindow::SetWndPos_script)
		.def("SetWndSize",				(void (CUIWindow::*)(Fvector2)) &CUIWindow::SetWndSize_script)
		.def("GetWndPos",				&get_wnd_pos)
		.def("GetWidth",				&CUIWindow::GetWidth)
		.def("GetHeight",				&CUIWindow::GetHeight)

		.def("Enable",					&CUIWindow::Enable)
		.def("IsEnabled",				&CUIWindow::IsEnabled)
		.def("Show",					&CUIWindow::Show)
		.def("IsShown",					&CUIWindow::IsShown)

		.def("WindowName",				&CUIWindow::WindowName_script)
		.def("SetWindowName",			&CUIWindow::SetWindowName)
		.def("SetPPMode",				&CUIWindow::SetPPMode)
		.def("ResetPPMode",				&CUIWindow::ResetPPMode)
	];

	module(L)
	[
		class_<CDialogHolder>("CDialogHolder")
		.def(constructor<>())
		.def("TopInputReceiver", 		&CDialogHolder::TopInputReceiver)
		.def("SetMainInputReceiver",	&CDialogHolder::SetMainInputReceiver)
		.def("AddDialogToRender",		&CDialogHolder::AddDialogToRender)
		.def("RemoveDialogToRender",	&CDialogHolder::RemoveDialogToRender),

		class_<CUIDialogWnd, CUIWindow>("CUIDialogWnd")
		.def(constructor<>())
		.def("ShowDialog",				&CUIDialogWnd::ShowDialog)
		.def("HideDialog",				&CUIDialogWnd::HideDialog)
		.def("GetHolder",				&CUIDialogWnd::GetHolder),

		class_<CUIFrameWindow, CUIWindow>("CUIFrameWindow")
		.def(							constructor<>())
		.def("SetWidth",				&CUIFrameWindow::SetWidth)
		.def("SetHeight",				&CUIFrameWindow::SetHeight)
		.def("SetColor",				&CUIFrameWindow::SetTextureColor),

		class_<CUIFrameLineWnd, CUIWindow>("CUIFrameLineWnd")
		.def(							constructor<>())
		.def("SetWidth",				&CUIFrameLineWnd::SetWidth)
		.def("SetHeight",				&CUIFrameLineWnd::SetHeight)
		.def("SetColor",				&CUIFrameLineWnd::SetTextureColor),

		class_<UIHint, CUIWindow>("UIHint")
		.def(							constructor<>())
		.def("SetWidth",				&UIHint::SetWidth)
		.def("SetHeight",				&UIHint::SetHeight)
		.def("SetHintText",				&UIHint::set_text)
		.def("GetHintText",				&UIHint::get_text),
		
		class_<CUIScrollView, CUIWindow>("CUIScrollView")
		.def(							constructor<>())
		.def("AddWindow",				&CUIScrollView::AddWindow)
		.def("RemoveWindow",			&CUIScrollView::RemoveWindow)
		.def("Clear",					&CUIScrollView::Clear)
		.def("ScrollToBegin",			&CUIScrollView::ScrollToBegin)
		.def("ScrollToEnd",				&CUIScrollView::ScrollToEnd)
		.def("GetMinScrollPos",			&CUIScrollView::GetMinScrollPos)
		.def("GetMaxScrollPos",			&CUIScrollView::GetMaxScrollPos)
		.def("GetCurrentScrollPos",		&CUIScrollView::GetCurrentScrollPos)
		.def("SetFixedScrollBar", 		&CUIScrollView::SetFixedScrollBar)																
		.def("SetScrollPos",			&CUIScrollView::SetScrollPos),

		class_<enum_exporter<EUIMessages> >("ui_events")
			.enum_("events")
			[
	// CUIWindow
				value("WINDOW_LBUTTON_DOWN",			int(WINDOW_LBUTTON_DOWN)),
				value("WINDOW_RBUTTON_DOWN",			int(WINDOW_RBUTTON_DOWN)),
				value("WINDOW_LBUTTON_UP",				int(WINDOW_LBUTTON_UP)),
				value("WINDOW_RBUTTON_UP",				int(WINDOW_RBUTTON_UP)),
				value("WINDOW_MOUSE_MOVE",				int(WINDOW_MOUSE_MOVE)),
				value("WINDOW_LBUTTON_DB_CLICK",		int(WINDOW_LBUTTON_DB_CLICK)),
				value("WINDOW_KEY_PRESSED",				int(WINDOW_KEY_PRESSED)),
				value("WINDOW_KEY_RELEASED",			int(WINDOW_KEY_RELEASED)),
				value("WINDOW_KEYBOARD_CAPTURE_LOST",	int(WINDOW_KEYBOARD_CAPTURE_LOST)),


	// CUIButton
				value("BUTTON_CLICKED",					int(BUTTON_CLICKED)),
				value("BUTTON_DOWN",					int(BUTTON_DOWN)),
				
	// CUITabControl
				value("TAB_CHANGED",					int(TAB_CHANGED)),

	// CUICheckButton
				value("CHECK_BUTTON_SET",				int(CHECK_BUTTON_SET)),
				value("CHECK_BUTTON_RESET",				int(CHECK_BUTTON_RESET)),
				
	// CUIRadioButton
				value("RADIOBUTTON_SET",				int(RADIOBUTTON_SET)),

	// CUIScrollBox
				value("SCROLLBOX_MOVE",					int(SCROLLBOX_MOVE)),
				
	// CUIScrollBar
				value("SCROLLBAR_VSCROLL",				int(SCROLLBAR_VSCROLL)),
				value("SCROLLBAR_HSCROLL",				int(SCROLLBAR_HSCROLL)),

	// CUIListWnd
				value("LIST_ITEM_CLICKED",				int(LIST_ITEM_CLICKED)),
				value("LIST_ITEM_SELECT",				int(LIST_ITEM_SELECT)),
	
	// UIPropertiesBox
				value("PROPERTY_CLICKED",				int(PROPERTY_CLICKED)),

	// CUIMessageBox
				value("MESSAGE_BOX_OK_CLICKED",			int(MESSAGE_BOX_OK_CLICKED)),
				value("MESSAGE_BOX_YES_CLICKED",		int(MESSAGE_BOX_YES_CLICKED)),
				value("MESSAGE_BOX_NO_CLICKED",			int(MESSAGE_BOX_NO_CLICKED)),
				value("MESSAGE_BOX_CANCEL_CLICKED",		int(MESSAGE_BOX_CANCEL_CLICKED)),
				value("MESSAGE_BOX_COPY_CLICKED",		int(MESSAGE_BOX_COPY_CLICKED)),
				value("MESSAGE_BOX_QUIT_GAME_CLICKED",	int(MESSAGE_BOX_QUIT_GAME_CLICKED)),
				value("MESSAGE_BOX_QUIT_WIN_CLICKED",	int(MESSAGE_BOX_QUIT_WIN_CLICKED)),

				value("EDIT_TEXT_COMMIT",				int(EDIT_TEXT_COMMIT)),
	// CMainMenu
				value("MAIN_MENU_RELOADED",				int(MAIN_MENU_RELOADED)),
	// CUITrackBar
				value("TRACK_VALUE_CHANGED",			int(TRACK_VALUE_CHANGED))
			]
	];
}
