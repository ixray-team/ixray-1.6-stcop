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

		.def("AddCallback",		(void(BaseType::*)(LPCSTR, s16, const luabind::functor<void>&, const luabind::object&))&BaseType::AddCallback)

		.def("Register",		(void (BaseType::*)(CUIWindow*,LPCSTR))&BaseType::Register)

	;
}
