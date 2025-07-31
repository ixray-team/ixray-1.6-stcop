#include "stdafx.h"
#include "pch_script.h"
#include "CustomZone.h"

using namespace luabind;

#pragma optimize("s",on)
void CCustomZone::script_register(lua_State *L)
{
	module(L)
		[
			class_<CCustomZone, CGameObject>("CCustomZone")
			.def(constructor<>())

		];
}