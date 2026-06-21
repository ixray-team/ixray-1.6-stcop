#include "StdAfx.h"
#include "pch_script.h"

#include "script_ui_registrator.h"
#include "MainMenu.h"

#include "UIGameCustom.h"
#include "ui/UIScriptWnd.h"
#include "../xrUI/Widgets/UIButton.h"
#include "../xrUI/Widgets/UIProgressBar.h"
#include "../xrUI/Widgets/UIEditBox.h"
#include "ui/UIMessageBox.h"
#include "../xrUI/Widgets/UIPropertiesBox.h"
#include "../xrUI/Widgets/UITabControl.h"
#include "ui/UIMapList.h"
#include "ui/UIMMShniaga.h"
#include "../xrUI/Widgets/UIComboBox.h"
#include "../xrUI/Widgets/UIOptionsManagerScript.h"
#include "../xrUI/Widgets/UIMultiTextStatic.h"
#include "../xrUI/Widgets/UIListWnd.h"
#include "ui/UIMapInfo.h"
#include "map_manager.h"
#include "ScriptXMLInit.h"
#include "../xrUI/Widgets/UIArrow.h"
#include "../xrEngine/xr_input.h"
#include "../xrUI/Widgets/UIActionRepeaters.h"

using namespace luabind;

CMainMenu*	MainMenu();

int placeholder_func() // to keep game from crashing during menu init
{
	return 0;
}

bool IsControllerMode()
{
	return pInput->GetControllerMode();
}

float get_current_kx()
{
	return UI().get_current_kx();
}

bool is_widescreen()
{
	return UI().is_widescreen();
}

const char* gamepad_prefix()
{
	return pInput->GamepadPrefix();
}

#pragma optimize("s",on)
void UIRegistrator::script_register(lua_State *L)
{
	CUIActionRepeatersOwner::script_register(L);
	CUIActionRepeatersManager::script_register(L);
	CUIWindow::script_register(L);
	CUIMMShniaga::script_register(L);
	CUIStatic::script_register(L);
	CUIButton::script_register(L);
	CUIProgressBar::script_register(L);
	CUIComboBox::script_register(L);
	CUIEditBox::script_register(L);
	CUITabControl::script_register(L);
	CUIMessageBox::script_register(L);
	CUIListWnd::script_register(L);
	CUIListBox::script_register(L);
	CUIMapList::script_register(L);
	CUIDialogWndEx::script_register(L);
	CUIPropertiesBox::script_register(L);
	CUIOptionsManagerScript::script_register(L);
	CUIMapInfo::script_register(L);
	CScriptXmlInit::script_register(L);
	CUIGameCustom::script_register(L);
	CUIArrow::script_register(L);

	module(L)
	[

		class_<CGameFont>("CGameFont")
			.enum_("EAligment")
			[
				value("alLeft",						int(CGameFont::alLeft)),
				value("alRight",					int(CGameFont::alRight)),
				value("alCenter",					int(CGameFont::alCenter))
			],

		class_<CUICaption>("CUICaption")
			.def("addCustomMessage",	&CUICaption::addCustomMessage)
			.def("setCaption",			&CUICaption::setCaption),

		class_<Patch_Dawnload_Progress>("Patch_Dawnload_Progress")
			.def("GetInProgress",	&Patch_Dawnload_Progress::GetInProgress)
			.def("GetStatus",		&Patch_Dawnload_Progress::GetStatus)
			.def("GetFlieName",		&Patch_Dawnload_Progress::GetFlieName)
			.def("GetProgress",		&Patch_Dawnload_Progress::GetProgress),

		class_<CMainMenu>("CMainMenu")
			.def("GetPatchProgress",		&CMainMenu::GetPatchProgress)
			.def("CancelDownload",			&CMainMenu::CancelDownload)
			.def("ValidateCDKey",			&CMainMenu::ValidateCDKey)
			.def("GetGSVer",				&CMainMenu::GetGSVer)
			.def("GetCDKey",				&CMainMenu::GetCDKeyFromRegistry)
			.def("GetPlayerName",			&CMainMenu::GetPlayerName)
			.def("GetDemoInfo",				&CMainMenu::GetDemoInfo)
			.def("GetLoginMngr",			&placeholder_func)
			.def("GetAccountMngr",			&placeholder_func)
			.def("GetProfileStore",			&placeholder_func)
	];

	module(L,"main_menu")
	[
		def("get_main_menu",				&MainMenu),
		def("in_controller_mode",			&IsControllerMode)
	];
	module(L,"ui")
	[
		def("get_current_kx",				&get_current_kx),
		def("is_widescreen",				&is_widescreen),
		def("gamepad_prefix",				&gamepad_prefix)
	];

	CMapManager::script_register(L);
}
