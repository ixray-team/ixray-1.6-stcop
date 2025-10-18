#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponVintorez.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeaponVintorez::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponVintorez,CGameObject>("CWeaponVintorez")
			.def(constructor<>())
	];
}
