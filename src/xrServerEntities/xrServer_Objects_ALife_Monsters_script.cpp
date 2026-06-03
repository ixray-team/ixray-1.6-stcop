////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Objects_ALife_Monsters_script.cpp
//	Created 	: 19.09.2002
//  Modified 	: 04.06.2003
//	Author		: Dmitriy Iassenev
//	Description : Server monsters for ALife simulator, script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "xrServer_script_macroses.h"
#include "specific_character.h"

using namespace luabind;

#ifdef XRGAME_EXPORTS
#include "InventoryOwner.h"

const char* profile_name_script(CSE_ALifeTraderAbstract* ta)
{
	return *ta->character_profile();
}

void profile_name_set_script(CSE_ALifeTraderAbstract* ta, const char* str)
{
	ta->set_character_profile(str);
}

const char* character_name_script(CSE_ALifeTraderAbstract* ta)
{
	return ta->m_character_name.c_str();
}

const char* icon_name_script(CSE_ALifeTraderAbstract* ta)
{
	ta->specific_character();
	if (!ta->m_icon_name.size())
	{
		CSpecificCharacter selected_char;
		selected_char.Load(ta->m_SpecificCharacter);
		ta->m_icon_name = selected_char.IconName();
	}
	return *ta->m_icon_name;
}

void set_character_name_script(CSE_ALifeTraderAbstract* ta, const char* str) {
	ta->m_character_name_raw = str;
	ta->m_character_name = TranslateName(ta->m_character_name_raw.c_str());

	if (g_pGameLevel)
	{
		CObject* obj = g_pGameLevel->Objects.net_Find(ta->object_id());
		CInventoryOwner* owner = obj != nullptr ? obj->cast_inventory_owner() : nullptr;
		if (owner)
			owner->ChangeName(str);
	}
}
const char* character_name_str_script(CSE_ALifeTraderAbstract* ta) {
	return ta->m_character_name_raw.c_str();
}
#endif

#pragma optimize("s",on)
void CSE_ALifeTraderAbstract::script_register(lua_State *L)
{
	module(L)[
		class_<CSE_ALifeTraderAbstract>
			("cse_alife_trader_abstract")
//			.def(		constructor<const char*>())
#ifdef XRGAME_EXPORTS
			.def("community",		&CSE_ALifeTraderAbstract::CommunityName)
			.def("profile_name",	&profile_name_script)
			.def("set_profile_name", &profile_name_set_script)
			.def("character_name", &character_name_script)
			.def("set_character_name", &set_character_name_script)
			.def("character_name_str", &character_name_str_script)
			.def("rank",			&CSE_ALifeTraderAbstract::Rank)
			.def("set_rank",		&CSE_ALifeTraderAbstract::SetRank)
			.def("reputation",		&CSE_ALifeTraderAbstract::Reputation)
			.def("character_icon", &icon_name_script)
#endif // XRGAME_EXPORTS
	];
}

void CSE_ALifeTrader::script_register(lua_State *L)
{
	module(L)[
		luabind_class_dynamic_alife2(
			CSE_ALifeTrader,
			"cse_alife_trader",
			CSE_ALifeDynamicObjectVisual,
			CSE_ALifeTraderAbstract
		)
	];
}

void CSE_ALifeAnomalyZone::script_register(lua_State *L)
{
	module(L)[
		luabind_class_dynamic_alife2(
			CSE_ALifeAnomalyZone,
			"cse_custom_zone",
			CSE_ALifeDynamicObject,
			CSE_Shape
		)
	];
}

void CSE_ALifeAnomalousZone::script_register(lua_State *L)
{
	module(L)[
		luabind_class_dynamic_alife1(
			CSE_ALifeAnomalousZone,
			"cse_anomalous_zone",
			CSE_ALifeAnomalyZone
		)
#ifdef XRGAME_EXPORTS
		.def("spawn_artefacts",	&CSE_ALifeAnomalousZone::spawn_artefacts)
#endif
	];
}

void CSE_ALifeMonsterRat::script_register(lua_State *L)
{
	module(L)[
		luabind_class_monster2(
			CSE_ALifeMonsterRat,
			"cse_alife_monster_rat",
			CSE_ALifeMonsterAbstract,
			CSE_ALifeInventoryItem
		)
	];
}