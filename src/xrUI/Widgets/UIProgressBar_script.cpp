#include "stdafx.h"
#include "UIProgressBar.h"
#include <luabind/luabind.hpp>

using namespace luabind;

#pragma optimize("s",on)
void CUIProgressBar::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUIProgressBar, CUIWindow>("CUIProgressBar")
		.def(						constructor<>())
		.def("SetProgressPos",			&CUIProgressBar::SetProgressPos)
		.def("GetProgressPos",			&CUIProgressBar::GetProgressPos)

		.def("GetRange_min",			&CUIProgressBar::GetRange_min)
		.def("GetRange_max",			&CUIProgressBar::GetRange_max)
		.def("ShowBackground", &CUIProgressBar::ShowBackground)
		.def("SetColor", &CUIProgressBar::SetColor)
		.def("UseColor", &CUIProgressBar::UseColor)
		.def("SetMinColor", &CUIProgressBar::SetMinColor)
		.def("SetMiddleColor", &CUIProgressBar::SetMinColor)
		.def("SetMaxColor", &CUIProgressBar::SetMinColor)
		.def("GetProgressStatic", &CUIProgressBar::GetProgressStatic)
		//.def("GetSnapNoDelay", &CUIProgressBar::GetSnapNoDelay)
		//.def("SetSnapNoDelay", &CUIProgressBar::SetSnapNoDelay)
		.def("SnapProgressPos", &CUIProgressBar::SnapProgressPos)
	];
}