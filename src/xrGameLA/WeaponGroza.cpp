#include "pch_script.h"
#include "weapongroza.h"
#include "player_hud.h"

CWeaponGroza::CWeaponGroza() : CWeaponMagazinedWGrenade("GROZA",SOUND_TYPE_WEAPON_SUBMACHINEGUN) 
{}

CWeaponGroza::~CWeaponGroza() 
{
}

using namespace luabind;

#pragma optimize("s",on)
void CWeaponGroza::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponGroza,CGameObject>("CWeaponGroza")
			.def(constructor<>())
	];
}
