#include "stdafx.h"
#include "pch_script.h"

//UI-controls
#include "UIScriptWnd.h"
#include "../../xrUI/Widgets/UIButton.h"
#include "UIMessageBox.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/Widgets/UIRadioButton.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UIListWnd.h"
#include "../../xrUI/Widgets/UITrackBar.h"
#include "../../xrUI/Widgets/UIComboBox.h"
#include "uiscriptwnd_script.h"

using namespace luabind;

extern export_class script_register_ui_window1(export_class &&);
extern export_class script_register_ui_window2(export_class &&);

#pragma optimize("s",on)
void CUIDialogWndEx::script_register(lua_State *L)
{
	export_class				instance("CUIScriptWnd");

	module(L)
	[
		script_register_ui_window2(
			script_register_ui_window1(
				std::move(instance)
			)
		)
		.def("Load",			&BaseType::Load)
	];
}

export_class script_register_ui_window1(export_class &&instance)
{
	return std::move(instance)
		.def(					constructor<>())

		.def("AddCallback",		(void(BaseType::*)(const char*, s16, const luabind::functor<void>&, const luabind::object&))&BaseType::AddCallback)

		.def("Register",		(void (BaseType::*)(CUIWindow*))&BaseType::Register)
		.def("Register",		(void (BaseType::*)(CUIWindow*,const char*))&BaseType::Register)
		.def("GetStatic",		(CUIStatic * (BaseType::*)(const char*)) & BaseType::GetControl<CUIStatic>)
		.def("GetEditBox",		(CUIEditBox * (BaseType::*)(const char*)) & BaseType::GetControl<CUIEditBox>)
		.def("GetDialogWnd",	(CUIDialogWnd * (BaseType::*)(const char*)) & BaseType::GetControl<CUIDialogWnd>)
		.def("GetFrameWindow",	(CUIFrameWindow * (BaseType::*)(const char*)) & BaseType::GetControl<CUIFrameWindow>)
		.def("GetFrameLineWnd", (CUIFrameLineWnd * (BaseType::*)(const char*)) & BaseType::GetControl<CUIFrameLineWnd>)
		.def("GetProgressBar",	(CUIProgressBar * (BaseType::*)(const char*)) & BaseType::GetControl<CUIProgressBar>)
		.def("GetTabControl",	(CUITabControl * (BaseType::*)(const char*)) & BaseType::GetControl<CUITabControl>)
		.def("GetListWnd",		(CUIListWnd* (BaseType::*)(const char*)) &BaseType::GetControl<CUIListWnd>)
		.def("GetTrackBar",		(CUITrackBar* (BaseType::*)(const char*)) &BaseType::GetControl<CUITrackBar>)
		.def("GetComboBox",		(CUIComboBox* (BaseType::*)(const char*)) &BaseType::GetControl<CUIComboBox>)
		.def("GetCheck",		(CUICheckButton* (BaseType::*)(const char*)) &BaseType::GetControl<CUICheckButton>)
	;
}
