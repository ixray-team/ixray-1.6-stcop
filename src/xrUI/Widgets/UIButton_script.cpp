#include "stdafx.h"
#include "UIButton.h"
#include "UI3tButton.h"
#include "UICheckButton.h"
#include "UIRadioButton.h"
#include "UISpinNum.h"
#include "UISpinText.h"
#include "UITrackBar.h"
#include "UIArrowStepper.h"

#include <luabind/luabind.hpp>

using namespace luabind;

void AssignProps_script_track(CUITrackBar* track, LPCSTR entry, LPCSTR group) 
{ 
	track->AssignProps(entry, group); 
}

void AssignProps_script_check(CUICheckButton* btn, LPCSTR entry, LPCSTR group)
{
	btn->AssignProps(entry, group);
}

void SetSystemDepends_script_track(CUITrackBar* track, int depend)
{
	track->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);
}

void SetSystemDepends_script_check(CUICheckButton* btn, int depend)
{
	btn->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);
}

void CheckButton_Toggle(CUICheckButton* btn)
{
	btn->SetCheck(!btn->GetCheck());
	btn->SendClickCallback();
}

#pragma optimize("s",on)
void CUIButton::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUIButton, CUIStatic>("CUIButton")
		.def(							constructor<>())
		.def("SetHighlightColor",		&CUIButton::SetHighlightColor)
		.def("EnableTextHighlighting",	&CUIButton::EnableTextHighlighting)
		,

		class_<CUI3tButton, CUIButton>("CUI3tButton")
		.def(							constructor<>())
		.def("OnClick",					&CUI3tButton::OnClick)
		.def("SetHighlighted",			&CUI3tButton::SetHighlighted)
		,


		class_<CUICheckButton, CUI3tButton>("CUICheckButton")
		.def(							constructor<>())
		.def("GetCheck",				&CUICheckButton::GetCheck)
		.def("SetCheck",				&CUICheckButton::SetCheck)
		.def("SetDependControl",		&CUICheckButton::SetDependControl)
		.def("AssignProps",				&AssignProps_script_check)
		.def("SetSystemDepends",		&SetSystemDepends_script_check)
		.def("Toggle",					&CheckButton_Toggle),

		class_<CUICustomSpin, CUIWindow>("CUICustomSpin")
		.def("GetText",				&CUICustomSpin::GetText),

		class_<CUISpinNum, CUICustomSpin>("CUISpinNum")
		.def(							constructor<>()),

		class_<CUISpinFlt, CUICustomSpin>("CUISpinFlt")
		.def(							constructor<>()),

		class_<CUISpinText, CUICustomSpin>("CUISpinText")
		.def(							constructor<>()),

		class_<CUITrackBar, CUIWindow>("CUITrackBar")
		.def(							constructor<>())
		.def("GetCheck",				&CUITrackBar::GetCheck)
		.def("SetCheck",				&CUITrackBar::SetCheck)
		.def("GetIValue",				&CUITrackBar::GetIValue)
		.def("SetIValue",				&CUITrackBar::SetIValue) // FFx0001 ++
		.def("GetFValue",				&CUITrackBar::GetFValue)
		.def("SetFValue",				&CUITrackBar::SetFValue) // FFx0001 ++
		.def("SetOptIBounds",			&CUITrackBar::SetOptIBounds)
		.def("SetOptFBounds",			&CUITrackBar::SetOptFBounds)
		.def("SetCurrentValue",			&CUITrackBar::SetCurrentOptValue)
		.def("CurrentID",				&CUITrackBar::CurrentID)
		.def("SetCurrentID",			&CUITrackBar::SetCurrentID)
		.def("AssignProps",				&AssignProps_script_track)
		.def("SetStep",					&CUITrackBar::SetStep)
		.def("SetMagnitude",			&CUITrackBar::SetMagnitude)
		.def("GetInvert",				&CUITrackBar::GetInvert)
		.def("SetInvert",				&CUITrackBar::SetInvert)
		.def("SetDrawingValue",			&CUITrackBar::SetDrawingValue)
		.def("SetSystemDepends",		&SetSystemDepends_script_track)
		.def("StepLeft",				&CUITrackBar::StepLeft)
		.def("StepRight",				&CUITrackBar::StepRight)
		.def("SetHighlighted",			&CUITrackBar::SetHighlighted),

		class_<CUIArrowStepper, CUIWindow>("CUIArrowStepper")
		.def(							constructor<>())
		.def("GetCheck",				&CUIArrowStepper::GetCheck)
		.def("SetCheck",				&CUIArrowStepper::SetCheck)
		.def("GetIValue",				&CUIArrowStepper::GetIValue)
		.def("GetFValue",				&CUIArrowStepper::GetFValue)
		.def("SetCurrentValue",			&CUIArrowStepper::SetCurrentOptValue)
	];
}