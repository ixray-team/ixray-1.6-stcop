#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponLR300.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeaponLR300::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponLR300,CGameObject>("CWeaponLR300")
			.def(constructor<>())
	];
}
