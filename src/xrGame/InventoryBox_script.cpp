#include "stdafx.h"
#include "pch_script.h"
#include "InventoryBox.h"

using namespace luabind;

#pragma optimize("s",on)
void CInventoryBox::script_register(lua_State *L)
{
	module(L)
		[
			class_<CInventoryBox, CGameObject>("CInventoryBox")
			.def(constructor<>())
			.def("can_take", &CInventoryBox::can_take)
			.def("set_can_take", &CInventoryBox::set_can_take)
			.def("set_closed", &CInventoryBox::set_closed)
		];
}