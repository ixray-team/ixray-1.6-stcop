#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponUSP45.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeaponUSP45::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponUSP45,CGameObject>("CWeaponUSP45")
			.def(constructor<>())
	];
}
