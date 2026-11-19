#include "StdAfx.h"
#include "pch_script.h"
#include "../../Weapon.h"
#include "../../WeaponMagazined.h"
#include "../../WeaponMagazinedWGrenade.h"
#include "../../WeaponPistol.h"

void RegisterWeaponAliases(lua_State* L)
{
	luabind::object weapon_class = luabind::get_globals(L)["CWeaponMagazined"];

	auto MakeAliasLambda = [&weapon_class, L](const char* TypeID)
	{
		lua_pushstring(L, TypeID);
		weapon_class.pushvalue();
		lua_settable(L, LUA_GLOBALSINDEX);
	};

	MakeAliasLambda("CWeaponVintorez");
	MakeAliasLambda("CWeaponVal");
	MakeAliasLambda("CWeaponLR300");
	MakeAliasLambda("CWeaponFN2000");
	MakeAliasLambda("CWeaponCustomPistol");
	MakeAliasLambda("CWeaponSVU");

	weapon_class = luabind::get_globals(L)["CWeaponMagazinedWGrenade"];

	MakeAliasLambda("CWeaponAK74");
	MakeAliasLambda("CWeaponGroza");

	weapon_class = luabind::get_globals(L)["CWeaponPistol"];

	MakeAliasLambda("CWeaponPM");
	MakeAliasLambda("CWeaponHPSA");
	MakeAliasLambda("CWeaponFORT");
	MakeAliasLambda("CWeaponUSP45");
	MakeAliasLambda("CWeaponWalther");
}
