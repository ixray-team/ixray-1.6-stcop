#include "pch_script.h"
#include "UIWindow.h"
#include "UIFrameWindow.h"
#include "UIFrameLineWnd.h"
#include "UIDialogWnd.h"
#include "../UIDialogHolder.h"
#include "../GamePersistent.h"
#include "UILabel.h"
#include "UIMMShniaga.h"
#include "UITextureMaster.h"
#include "UIScrollView.h"

CFontManager& mngr(){
	return UI().Font();
}

// hud font
CGameFont* GetFontSmall()
{return mngr().pFontStat;}

CGameFont* GetFontMedium()
{return mngr().pFontMedium;}
CGameFont* GetFontDI()
{return mngr().pFontDI;}
//רנטפעû הכÿ טםעונפויסא
CGameFont* GetFontGraffiti19Russian()
{return mngr().pFontGraffiti19Russian;}
CGameFont* GetFontGraffiti22Russian()
{return mngr().pFontGraffiti22Russian;}
CGameFont* GetFontLetterica16Russian()
{return mngr().pFontLetterica16Russian;}
CGameFont* GetFontLetterica18Russian()
{return mngr().pFontLetterica18Russian;}
CGameFont* GetFontGraffiti32Russian()
{return mngr().pFontGraffiti32Russian;}
CGameFont* GetFontGraffiti50Russian()
{return mngr().pFontGraffiti50Russian;}
CGameFont* GetFontLetterica25()
{return mngr().pFontLetterica25;}


int GetARGB(u16 a, u16 r, u16 g, u16 b)
{return color_argb(a,r,g,b);}

const Fvector2* get_wnd_pos(CUIWindow* w)
{
	return &w->GetWndPos();
}

Frect	get_texture_rect(LPCSTR icon_name)
{
	return CUITextureMaster::GetTextureRect(icon_name);
}

LPCSTR	get_texture_name(LPCSTR icon_name)
{
	return CUITextureMaster::GetTextureFileName(icon_name);
}

TEX_INFO	get_texture_info(LPCSTR name, LPCSTR def_name)
{
	return CUITextureMaster::FindItem(name, def_name);
}

void attach_child_script(CUIWindow* self, CUIWindow* child)
{
	self->AttachChild(child);
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
		def("GetFontGraffiti19Russian",	&GetFontGraffiti19Russian),
		def("GetFontGraffiti22Russian",	&GetFontGraffiti22Russian),
		def("GetFontLetterica16Russian",&GetFontLetterica16Russian),
		def("GetFontLetterica18Russian",&GetFontLetterica18Russian),
		def("GetFontGraffiti32Russian",	&GetFontGraffiti32Russian),
		def("GetFontGraffiti50Russian",	&GetFontGraffiti50Russian),
		def("GetFontLetterica25",		&GetFontLetterica25),

		class_<TEX_INFO>("TEX_INFO")
		.def("get_file_name",	 			&TEX_INFO::get_file_name)
		.def("get_rect",					&TEX_INFO::get_rect),

		def("GetTextureName",			&get_texture_name),
		def("GetTextureRect",			&get_texture_rect),
		def("GetTextureInfo",			&get_texture_info),

		class_<CUIWindow>("CUIWindow")
		.def(							constructor<>())
		.def("AttachChild",				&attach_child_script, adopt(_2))
		.def("DetachChild",				&CUIWindow::DetachChild)
		.def("SetAutoDelete",			&CUIWindow::SetAutoDelete)
		.def("IsAutoDelete",			&CUIWindow::IsAutoDelete)

		.def("Init",					(void (CUIWindow::*)(float,float,float,float))   &CUIWindow::Init_script) //skyloader: for current la scripts

		.def("SetWndRect",					(void (CUIWindow::*)(float,float,float,float))   &CUIWindow::SetWndRect_script)
		.def("SetWndRect",					(void (CUIWindow::*)(Frect))   &CUIWindow::SetWndRect_script)
		.def("SetWndPos",				(void (CUIWindow::*)(float,float)) &CUIWindow::SetWndPos_script)
		.def("SetWndSize",				(void (CUIWindow::*)(float,float)) &CUIWindow::SetWndSize_script)
		.def("GetWndPos",				&get_wnd_pos)
		.def("GetWidth",				&CUIWindow::GetWidth)
		.def("SetWidth",				(void (CUIWindow::*)(float)) &CUIWindow::SetWidth)
		.def("GetHeight",				&CUIWindow::GetHeight)
		.def("SetHeight",				(void (CUIWindow::*)(float)) &CUIWindow::SetHeight)
#ifdef DEBUG
		.def("SetDebugColor",			&CUIWindow::SetDebugColor)
#endif


		.def("Enable",					&CUIWindow::Enable)
		.def("IsEnabled",				&CUIWindow::IsEnabled)
		.def("Show",					&CUIWindow::Show)
		.def("IsShown",					&CUIWindow::IsShown)
		.def("SetFont",					&CUIWindow::SetFont)
		.def("GetFont",					&CUIWindow::GetFont)

		.def("WindowName",				&CUIWindow::WindowName_script)
		.def("SetWindowName",			&CUIWindow::SetWindowName)
		.def("SetPPMode",				&CUIWindow::SetPPMode)
		.def("ResetPPMode",				&CUIWindow::ResetPPMode)

		.def("BringAllToTop",				&CUIWindow::BringAllToTop)
		.def("BringToTop",				&CUIWindow::BringToTop)

		.def("ShowChildren",				&CUIWindow::ShowChildren)

		.def("GetAbsoluteRect",				&CUIWindow::GetAbsoluteRect)
		.def("GetAbsolutePos",				&CUIWindow::GetAbsolutePos)

		.def("SetKeyboardCapture",				&CUIWindow::SetKeyboardCapture)
		.def("SetCapture",				&CUIWindow::SetCapture)
		.def("GetMouseCapturer",				&CUIWindow::GetMouseCapturer)

		.def("GetTop",				&CUIWindow::GetTop)
		.def("SetParent",				&CUIWindow::SetParent)
		.def("GetParent",				&CUIWindow::GetParent)
		.def("GetCustomDraw",				&CUIWindow::GetCustomDraw)
		.def("SetCustomDraw",				&CUIWindow::SetCustomDraw)
		.def("SafeRemoveChild",				&CUIWindow::SafeRemoveChild)
		.def("CursorOverWindow",				&CUIWindow::CursorOverWindow),

		class_<CDialogHolder>("CDialogHolder")
		.def("MainInputReceiver",		&CDialogHolder::TopInputReceiver)
		.def("start_stop_menu",			&CDialogHolder::StartStopMenu)
		.def("AddDialogToRender",		&CDialogHolder::AddDialogToRender)
		.def("RemoveDialogToRender",	&CDialogHolder::RemoveDialogToRender),

		class_<CUIDialogWnd, CUIWindow>("CUIDialogWnd")
		.def("ShowDialog",				&CUIDialogWnd::ShowDialog)
		.def("HideDialog",				&CUIDialogWnd::HideDialog)
		.def("GetHolder",				&CUIDialogWnd::GetHolder)
		.def("SetHolder",				&CUIDialogWnd::SetHolder)
		.def("NeedCenterCursor",		&CUIDialogWnd::SetCenterCursor)
		.def("WorkInPause",			&CUIDialogWnd::SetWorkInPause),

		class_<CUIFrameWindow, CUIWindow>("CUIFrameWindow")
		.def(							constructor<>())
		.def("SetWidth",				&CUIFrameWindow::SetWidth)
		.def("SetHeight",				&CUIFrameWindow::SetHeight)
		.def("SetColor",				&CUIFrameWindow::SetTextureColor)
		.def("GetTitleStatic",			&CUIFrameWindow::GetTitleStatic)
		.def("Init",					(void(CUIFrameWindow::*)(LPCSTR,float,float,float,float))&CUIFrameWindow::Init)
		.def("SetStretchTexture",	&CUIFrameWindow::SetStretchTexture)
		.def("GetStretchTexture",	&CUIFrameWindow::GetStretchTexture)
		.def("GetTextureRect",	&CUIFrameWindow::GetTextureRect)
		.def("SetTextureRect",	&CUIFrameWindow::SetTextureRect)
		.def("SetWndSize",	&CUIFrameWindow::SetWndSize),

		class_<CUIFrameLineWnd, CUIWindow>("CUIFrameLineWnd")
		.def(							constructor<>())
		.def("SetWidth",				&CUIFrameLineWnd::SetWidth)
		.def("SetHeight",				&CUIFrameLineWnd::SetHeight)
		.def("SetOrientation",					&CUIFrameLineWnd::SetHorizontal)
		.def("IsHorizontal",					&CUIFrameLineWnd::IsHorizontal)
		.def("SetColor",						&CUIFrameLineWnd::SetTextureColor)
		.def("GetTitleStatic",					&CUIFrameLineWnd::GetTitleStatic)
		.def("Init",							(void(CUIFrameLineWnd::*)(LPCSTR,float,float,float,float,bool))&CUIFrameLineWnd::Init_script)
		.def("GetTextureHeight",					&CUIFrameLineWnd::GetTextureHeight)
		.def("GetTextureWidth",					&CUIFrameLineWnd::GetTextureWidth),


		class_<CUILabel, CUIFrameLineWnd>("CUILabel")
		.def(					constructor<>())
		.def("SetText",						&CUILabel::SetText)
		.def("GetText",						&CUILabel::GetText)
		.def("SetTextColor",						&CUILabel::SetTextColor)
		.def("GetTextColor",						&CUILabel::GetTextColor)
		.def("SetLightAnim",						&CUILabel::SetLightAnim),

		
		class_<CUITextWnd, CUIWindow>("CUITextWnd")
		.def(						constructor<>())
		.def("AdjustHeightToText",	&CUITextWnd::AdjustHeightToText)
		.def("AdjustWidthToText",	&CUITextWnd::AdjustWidthToText)
		.def("SetText",				&CUITextWnd::SetText)
		.def("SetTextST",			&CUITextWnd::SetTextST)
		.def("GetText",				&CUITextWnd::GetText)
		.def("SetFont",				&CUITextWnd::SetFont)
		.def("GetFont",				&CUITextWnd::GetFont)
		.def("SetTextColor",		&CUITextWnd::SetTextColor)
		.def("GetTextColor",		&CUITextWnd::GetTextColor)
		.def("SetTextComplexMode",	&CUITextWnd::SetTextComplexMode)
		.def("SetTextAlignment",	&CUITextWnd::SetTextAlignment)
		.def("SetVTextAlignment",	&CUITextWnd::SetVTextAlignment)
		.def("SetEllipsis",			&CUITextWnd::SetEllipsis)
		.def("SetTextOffset",		&CUITextWnd::SetTextOffset),

		class_<CUIMMShniaga, CUIWindow>("CUIMMShniaga")
		.def("SetVisibleMagnifier",			&CUIMMShniaga::SetVisibleMagnifier),

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
				value("WINDOW_MOUSE_CAPTURE_LOST ",		int(WINDOW_MOUSE_CAPTURE_LOST )),
				value("WINDOW_KEYBOARD_CAPTURE_LOST",	int(WINDOW_KEYBOARD_CAPTURE_LOST)),

	// CUIStatic
				value("STATIC_FOCUS_RECEIVED",			int(WINDOW_FOCUS_RECEIVED)),
				value("STATIC_FOCUS_LOST",				int(WINDOW_FOCUS_LOST)),

	// CUIButton
				value("BUTTON_CLICKED",					int(BUTTON_CLICKED)),
				value("BUTTON_DOWN",					int(BUTTON_DOWN)),
				
	// CUITabControl
				value("TAB_CHANGED",					int(TAB_CHANGED)),
				value("EDIT_TEXT_COMMIT",				int(EDIT_TEXT_COMMIT)),

	// CUICheckButton
				value("CHECK_BUTTON_SET",				int(CHECK_BUTTON_SET)),
				value("CHECK_BUTTON_RESET",				int(CHECK_BUTTON_RESET)),
				
	// CUIRadioButton
				value("RADIOBUTTON_SET",				int(RADIOBUTTON_SET)),

	// CUIdragDropItem
				value("DRAG_DROP_ITEM_DRAG",			int(DRAG_DROP_ITEM_DRAG)),
				value("DRAG_DROP_ITEM_DROP ",			int(DRAG_DROP_ITEM_DROP )),
				value("DRAG_DROP_ITEM_DB_CLICK",		int(DRAG_DROP_ITEM_DB_CLICK)),
				value("DRAG_DROP_ITEM_RBUTTON_CLICK",	int(DRAG_DROP_ITEM_RBUTTON_CLICK)),

	// CUIScrollBox
				value("SCROLLBOX_MOVE",					int(SCROLLBOX_MOVE)),
				
	// CUIScrollBar
				value("SCROLLBAR_VSCROLL",				int(SCROLLBAR_VSCROLL)),
				value("SCROLLBAR_HSCROLL",				int(SCROLLBAR_HSCROLL)),

	// CUIListWnd
				value("LIST_ITEM_CLICKED",				int(LIST_ITEM_CLICKED)),
				value("LIST_ITEM_DB_CLICKED",				int(LIST_ITEM_DB_CLICKED)),
				value("LIST_ITEM_SELECT",				int(LIST_ITEM_SELECT)),
				value("LIST_ITEM_UNSELECT",				int(LIST_ITEM_UNSELECT)),
				value("LIST_ITEM_FOCUS_RECEIVED",				int(LIST_ITEM_FOCUS_RECEIVED)),

	// CUIInteractiveItem
				value("INTERACTIVE_ITEM_CLICK",			int(INTERACTIVE_ITEM_CLICK)),
	
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

				value("EDIT_TEXT_CHANGED",				int(EDIT_TEXT_CHANGED)),
				value("EDIT_TEXT_COMMIT",				int(EDIT_TEXT_COMMIT)),

	// CUITalkDialogWnd
				value("TALK_DIALOG_TRADE_BUTTON_CLICKED",	int(TALK_DIALOG_TRADE_BUTTON_CLICKED)),
				value("TALK_DIALOG_QUESTION_CLICKED",		int(TALK_DIALOG_QUESTION_CLICKED)),



	// CUIPdaDialogWnd
				value("PDA_DIALOG_WND_BACK_BUTTON_CLICKED",			int(PDA_DIALOG_WND_BACK_BUTTON_CLICKED)),
				value("PDA_DIALOG_WND_MESSAGE_BUTTON_CLICKED",		int(PDA_DIALOG_WND_MESSAGE_BUTTON_CLICKED)),

	// CUIPdaContactsWnd
				value("PDA_CONTACTS_WND_CONTACT_SELECTED",			int(PDA_CONTACTS_WND_CONTACT_SELECTED)),

	// CUITradeWnd
				value("TRADE_WND_CLOSED",							int(TRADE_WND_CLOSED)),

	// CUIOutfitSlot
				value("UNDRESS_OUTFIT",								int(UNDRESS_OUTFIT)),
				value("OUTFIT_RETURNED_BACK",						int(OUTFIT_RETURNED_BACK)),

	// CUIInventroyWnd
				value("INVENTORY_DROP_ACTION",						int(INVENTORY_DROP_ACTION)),
				value("INVENTORY_EAT_ACTION",						int(INVENTORY_EAT_ACTION)),
				value("INVENTORY_TO_BELT_ACTION ",					int(INVENTORY_TO_BELT_ACTION )),
				value("INVENTORY_TO_SLOT_ACTION",					int(INVENTORY_TO_SLOT_ACTION)),
				value("INVENTORY_TO_BAG_ACTION",					int(INVENTORY_TO_BAG_ACTION)),
				value("INVENTORY_ATTACH_ADDON ",					int(INVENTORY_ATTACH_ADDON )),
				value("INVENTORY_DETACH_SCOPE_ADDON",				int(INVENTORY_DETACH_SCOPE_ADDON)),
				value("INVENTORY_DETACH_SILENCER_ADDON",			int(INVENTORY_DETACH_SILENCER_ADDON)),
				value("INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON",	int(INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON)),
	// CMainMenu
				value("MAIN_MENU_RELOADED",				int(MAIN_MENU_RELOADED))
			]
	];
}
