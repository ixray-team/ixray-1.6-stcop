#include "stdafx.h"
#include "pch_script.h"
#include "../../Weapon.h"
#include "../../WeaponMagazined.h"
#include "../../WeaponMagazinedWGrenade.h"
#include "../../WeaponCustomPistol.h"
#include "../../WeaponPistol.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeapon::script_register(lua_State* L)
{
	module(L)
	[
		class_<CWeaponMagazined, CGameObject>("CWeaponMagazined")
			.def(constructor<>())
	];

	module(L)
	[
		class_<CWeaponMagazinedWGrenade, CGameObject>("CWeaponMagazinedWGrenade")
			.def(constructor<>())
	];

	module(L)
	[
		class_<CWeaponCustomPistol, CGameObject>("CWeaponCustomPistol")
			.def(constructor<>())
	];

	module(L)
	[
		class_<CWeaponPistol, CGameObject>("CWeaponPistol")
			.def(constructor<>())
	];

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

	weapon_class = luabind::get_globals(L)["CWeaponMagazinedWGrenade"];

	MakeAliasLambda("CWeaponAK74");
	MakeAliasLambda("CWeaponGroza");

	weapon_class = luabind::get_globals(L)["CWeaponCustomPistol"];

	MakeAliasLambda("CWeaponSVU");
	//MakeAliasLambda("CWeaponSVD");

	weapon_class = luabind::get_globals(L)["CWeaponPistol"];

	MakeAliasLambda("CWeaponPM");
	MakeAliasLambda("CWeaponHPSA");
	MakeAliasLambda("CWeaponFORT");
	MakeAliasLambda("CWeaponUSP45");
	MakeAliasLambda("CWeaponWalther");
}
