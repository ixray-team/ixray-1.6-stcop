#include "stdafx.h"
#include "pch_script.h"

//UI-controls
//#include "UIListWnd.h"
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

#pragma optimize("s",on)
export_class script_register_ui_window2(export_class &&instance)
{
	return std::move(instance)
		.def("OnKeyboard",		&BaseType::OnKeyboardAction, &WrapType::OnKeyboard_static)
		.def("Update",			&BaseType::Update, &WrapType::Update_static)
		.def("Dispatch",		&BaseType::Dispatch, &WrapType::Dispatch_static)

	;
}
