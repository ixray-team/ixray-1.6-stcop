#include "stdafx.h"

#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UITabButton.h"

#include <luabind/luabind.hpp>
#include <luabind/adopt_policy.hpp>
using namespace luabind;

#pragma optimize("s",on)
void CUITabControl::script_register(lua_State *L)
{
	module(L)
	[
		class_<CUITabControl, CUIWindow>("CUITabControl")
		.def(							constructor<>())
		.def("AddItem",					(bool (CUITabControl::*)(CUITabButton*))(&CUITabControl::AddItem), adopt<2>())
		.def("AddItem",					(bool (CUITabControl::*)(LPCSTR, LPCSTR,Fvector2,Fvector2))	&CUITabControl::AddItem)
		.def("RemoveAll",				&CUITabControl::RemoveAll)
		.def("GetActiveId",				&CUITabControl::GetActiveId_script)
		.def("GetTabsCount",			&CUITabControl::GetTabsCount)
		.def("SetActiveTab",			&CUITabControl::SetActiveTab_script)
		.def("GetButtonById",			&CUITabControl::GetButtonById_script),

		class_<CUITabButton, CUIButton>("CUITabButton")
		.def(							constructor<>())		
	];

}