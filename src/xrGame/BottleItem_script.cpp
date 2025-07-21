#include "stdafx.h"
#include "pch_script.h"
#include "BottleItem.h"

using namespace luabind;

#pragma optimize("s",on)
void CBottleItem::script_register(lua_State *L)
{
	module(L)
		[
			class_<CBottleItem, CGameObject>("CBottleItem")
			.def(constructor<>())
			.def("BreakToPieces", &CBottleItem::BreakToPieces)
		];
}