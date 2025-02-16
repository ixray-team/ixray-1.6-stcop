#include "pch_script.h"
#include "UIButton.h"
#include "UI3tButton.h"
#include "UICheckButton.h"
#include "UIRadioButton.h"
#include "UISpinNum.h"
#include "UISpinText.h"
#include "UITrackBar.h"

using namespace luabind;

#pragma optimize("s",on)
void CUIButton::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUIButton, CUIStatic>("CUIButton")
		.def(							constructor<>())
		.def("SetHighlightColor",			&CUIButton::SetHighlightColor)
		.def("EnableTextHighlighting",			&CUIButton::EnableTextHighlighting)
		.def("SetAccelerator",				&CUIButton::SetAccelerator)
		.def("GetAccelerator",				&CUIButton::GetAccelerator)
		.def("SetPushOffset",			&CUIButton::SetPushOffset)
		.def("GetPushOffset",			&CUIButton::GetPushOffset)
		.def("SetShadowOffset",			&CUIButton::SetShadowOffset),

		class_<CUI3tButton, CUIButton>("CUI3tButton")
		.def(							constructor<>())
		.def("Init",						(void (CUI3tButton::*)(float,float,float,float))  &CUI3tButton::Init_script)
		.def("Init",						(void (CUI3tButton::*)(LPCSTR,float,float,float,float))  &CUI3tButton::Init)
		.def("SetStateTextColor",				&CUI3tButton::SetStateTextColor)
		.def("SetStretchTexture",				&CUI3tButton::SetStretch),


		class_<CUICheckButton, CUI3tButton>("CUICheckButton")
		.def(							constructor<>())
		.def("Init",					&CUICheckButton::Init_script)
		.def("GetCheck",				&CUICheckButton::GetCheck)
		.def("SetCheck",				&CUICheckButton::SetCheck)
		.def("SetDependControl",		&CUICheckButton::SetDependControl)
		.def("InitTexture",					&CUICheckButton::InitTexture2),

		class_<CUICustomSpin, CUIWindow>("CUICustomSpin")
		.def("Init",				&CUICustomSpin::Init_script)
		.def("GetText",				&CUICustomSpin::GetText),

		class_<CUISpinNum, CUICustomSpin>("CUISpinNum")
		.def(							constructor<>())
		.def("Init",					&CUISpinNum::Init_script)
		,

		class_<CUISpinFlt, CUICustomSpin>("CUISpinFlt")
		.def(							constructor<>())
		.def("Init",					&CUISpinFlt::Init_script)

		,

		class_<CUISpinText, CUICustomSpin>("CUISpinText")
		.def(							constructor<>())
		.def("Init",					&CUISpinText::Init_script)
		.def("AddItem",					&CUISpinText::AddItemByScript)
		.def("GetTokenText",			&CUISpinText::GetTokenTextByScript)
		.def("GetCurrentID",			&CUISpinText::GetCurrentIDByScript)
		.def("SetCurrentID",			&CUISpinText::SetCurrentIDByScript),

		class_<CUITrackBar, CUIWindow>("CUITrackBar")
		.def(							constructor<>())
		.def("GetCheck",				&CUITrackBar::GetCheck)
		.def("SetCheck",				&CUITrackBar::SetCheck)
		.def("GetIValue",				&CUITrackBar::GetIValue)
		.def("GetFValue",				&CUITrackBar::GetFValue)
		.def("SetOptIBounds",			&CUITrackBar::SetOptIBounds)
		.def("SetOptFBounds",			&CUITrackBar::SetOptFBounds)
		.def("SetCurrentValue",			&CUITrackBar::SetCurrentOptValue)
	];
}