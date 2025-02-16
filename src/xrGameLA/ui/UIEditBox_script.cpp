#include "pch_script.h"
#include "UIEditBox.h"
#include "UIEditBoxEx.h"

using namespace luabind;

#pragma optimize("s",on)
void CUIEditBox::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUICustomEdit, CUIWindow>("CUICustomEdit")
		.def("Init",				(void (CUICustomEdit::*)(float,float,float,float)) &CUICustomEdit::Init_script)
		.def("SetText",				&CUICustomEdit::SetText)
		.def("GetText",				&CUICustomEdit::GetText)
		.def("CaptureFocus",		&CUICustomEdit::CaptureFocus)
		.def("SetNextFocusCapturer",&CUICustomEdit::SetNextFocusCapturer)
		.def("SetDbClickMode",		&CUICustomEdit::SetDbClickMode)
		.def("SetPasswordMode",		&CUICustomEdit::SetPasswordMode)
		.def("ClearText",		&CUICustomEdit::ClearText)
		.def("SetTextColorE",		&CUICustomEdit::SetTextColor)
		.def("SetTextColorD",		&CUICustomEdit::SetTextColorD)
		.def("SetTextPosX",			&CUICustomEdit::SetTextPosX)
		.def("SetTextPosY",			&CUICustomEdit::SetTextPosY),

		class_<CUIEditBox, CUICustomEdit>("CUIEditBox")
		.def(						constructor<>())
		.def("InitTexture",			&CUIEditBox::InitTexture),

		class_<CUIEditBoxEx, CUICustomEdit>("CUIEditBoxEx")
		.def(						constructor<>())
		.def("InitTexture",			&CUIEditBoxEx::InitTexture)
	];
}