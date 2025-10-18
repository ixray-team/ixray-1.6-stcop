#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponSVU.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeaponSVU::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponSVU,CGameObject>("CWeaponSVU")
			.def(constructor<>())
	];
}
