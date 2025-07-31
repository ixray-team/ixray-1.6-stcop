#include "stdafx.h"
#include "UIEditBox.h"

#include <luabind/luabind.hpp>

using namespace luabind;

#pragma optimize("s",on)
void CUIEditBox::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUICustomEdit, CUIWindow>("CUICustomEdit")
		.def("SetText",				&CUICustomEdit::SetText)
		.def("GetText",				&CUICustomEdit::GetText)
		.def("CaptureFocus",		&CUICustomEdit::CaptureFocus)
		.def("SetNextFocusCapturer",&CUICustomEdit::SetNextFocusCapturer),		

		class_<CUIEditBox, CUICustomEdit>("CUIEditBox")
		.def(						constructor<>())
		.def("InitTexture",			&CUIEditBox::InitTexture)
        .def("InitTexture",			+[](CUIEditBox* self, pcstr texture) { self->InitTexture(texture); })
	];
}