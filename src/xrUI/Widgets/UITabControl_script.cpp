#include "stdafx.h"

#include "UITabControl.h"
#include "UITabButton.h"

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
        .def("RemoveItem",				&CUITabControl::RemoveItemByIndex)
        .def("RemoveItem",				&CUITabControl::RemoveItemById_script)
		.def("RemoveAll",				&CUITabControl::RemoveAll)
		.def("GetActiveId",				&CUITabControl::GetActiveId_script)
		.def("GetActiveIndex",			&CUITabControl::GetActiveIndex)
		.def("GetTabsCount",			&CUITabControl::GetTabsCount)
		.def("SetActiveTab",			&CUITabControl::SetActiveTab_script)
        .def("SetNewActiveTab",			&CUITabControl::SetActiveTabByIndex)
		.def("GetButtonById",			&CUITabControl::GetButtonById_script)
		.def("GetButtonByIndex",		&CUITabControl::GetButtonByIndex),

		class_<CUITabButton, CUIButton>("CUITabButton")
		.def(							constructor<>())		
	];

}