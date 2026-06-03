#include "stdafx.h"
#include "pch_script.h"
#include "WeaponBinoculars.h"

using namespace luabind;

void CWeaponBinoculars::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponBinoculars,CGameObject>("CWeaponBinoculars")
			.def(constructor<>())
	];
}
