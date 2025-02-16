////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Objects_ALife_Items_script.cpp
//	Created 	: 19.09.2002
//  Modified 	: 04.06.2003
//	Author		: Dmitriy Iassenev
//	Description : Server items for ALife simulator, script export
////////////////////////////////////////////////////////////////////////////

#include "pch_script.h"
#include "xrServer_Objects_ALife_Items.h"
#include "xrServer_script_macroses.h"

using namespace luabind;

#pragma optimize("s",on)
void CSE_ALifeInventoryItem::script_register(lua_State *L)
{
	module(L)[
		class_<CSE_ALifeInventoryItem>
			("cse_alife_inventory_item")
//			.def(		constructor<LPCSTR>())
	];
}

void CSE_ALifeItem::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item2(
			CSE_ALifeItem,
			"cse_alife_item",
			CSE_ALifeDynamicObjectVisual,
			CSE_ALifeInventoryItem
		)
	];
}

void CSE_ALifeItemTorch::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemTorch,
			"cse_alife_item_torch",
			CSE_ALifeItem
		)
	];
}

void CSE_ALifeItemAmmo::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemAmmo,
			"cse_alife_item_ammo",
			CSE_ALifeItem
		)
	];
}

void CSE_ALifeItemWeapon::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemWeapon,
			"cse_alife_item_weapon",
			CSE_ALifeItem
		)
		.def					("get_addon_flags",						&CSE_ALifeItemWeapon::get_addon_flags)
		.def					("set_ammo_in_magazine",				&CSE_ALifeItemWeapon::set_ammo_elapsed)
		.def					("get_ammo_in_magazine",				&CSE_ALifeItemWeapon::get_ammo_elapsed)
		.def					("get_ammo_magsize",					&CSE_ALifeItemWeapon::get_ammo_magsize)
		.def					("set_addon_flags",						&CSE_ALifeItemWeapon::set_addon_flags)
		.def					("set_ammo_type",						&CSE_ALifeItemWeapon::set_ammo_type)
		.def					("get_ammo_type",						&CSE_ALifeItemWeapon::get_ammo_type)
		.enum_					("EWeaponAddonStatus")
		[
			value				("eAddonDisabled",						int(CSE_ALifeItemWeapon::eAddonDisabled)),
			value				("eAddonPermanent",						int(CSE_ALifeItemWeapon::eAddonPermanent)),
			value				("eAddonAttachable",					int(CSE_ALifeItemWeapon::eAddonAttachable))
		]
		.enum_					("EWeaponAddonState")
		[
			value				("eWeaponAddonScope",					int(CSE_ALifeItemWeapon::eWeaponAddonScope)),
			value				("eWeaponAddonGrenadeLauncher",			int(CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher)),
			value				("eWeaponAddonSilencer",				int(CSE_ALifeItemWeapon::eWeaponAddonSilencer))
		]
	];
}

void CSE_ALifeItemWeaponShotGun::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemWeaponShotGun,
			"cse_alife_item_weapon_shotgun",
			CSE_ALifeItemWeapon
			)
	];
}

void CSE_ALifeItemDetector::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemDetector,
			"cse_alife_item_detector",
			CSE_ALifeItem
		)
	];
}

void CSE_ALifeItemArtefact::script_register(lua_State *L)
{
	module(L)[
		luabind_class_item1(
			CSE_ALifeItemArtefact,
			"cse_alife_item_artefact",
			CSE_ALifeItem
		)
	];
}
