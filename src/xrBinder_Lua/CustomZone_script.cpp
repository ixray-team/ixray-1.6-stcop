#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/CustomZone.h"

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