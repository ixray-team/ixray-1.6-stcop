////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object_script3.cpp
//	Created 	: 17.11.2004
//  Modified 	: 17.11.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "Actor.h"
#include "script_game_object.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "cover_evaluators.h"
#include "cover_point.h"
#include "cover_manager.h"
#include "ai/stalker/ai_stalker.h"
#include "stalker_animation_manager.h"
#include "Weapon.h"
#include "Inventory.h"
#include "CustomZone.h"
#include "patrol_path_manager.h"
#include "memory_manager.h"
#include "visual_memory_manager.h"
#include "sound_memory_manager.h"
#include "hit_memory_manager.h"
#include "sight_manager.h"
#include "stalker_movement_manager_smart_cover.h"
#include "movement_manager_space.h"
#include "detail_path_manager_space.h"
#include "level_debug.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "trade_parameters.h"
#include "../xrScripts/exports/script_ini_file.h"
#include "sound_player.h"
#include "space_restriction_manager.h"
#include "eatable_item.h"
#include "Legacy/StalkerPlanner/stalker_planner.h"
#include "level_path_manager.h"
#include "game_path_manager.h"		  
#include "holder_custom.h"
#include "WeaponMagazinedWGrenade.h"
#include "WeaponMagazined.h"
#include "inventory_upgrade_manager.h"
#include "alife_simulator.h"
#include "eatable_item.h"
#include "CustomOutfit.h"

namespace MemorySpace {
	struct CVisibleObject;
	struct CSoundObject;
	struct CHitObject;
};

void CScriptGameObject::IterateFeelTouch(const luabind::functor<bool>& functor)
{
	Feel::Touch* touch = smart_cast<Feel::Touch*>(&object());
	if (touch)
	{
		for (const CObject* Obj : touch->feel_touch)
		{
			if (Obj != nullptr)
			{
				if (functor(Obj->ID()))
				{
					return;
				}
			}
		}
	}
}

int CScriptGameObject::GetAmmoCount(u8 type)
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		if (type < weapon->m_ammoTypes.size())
		{
			return weapon->GetAmmoCount_forType(weapon->m_ammoTypes[type]);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeapon : wrong ammotype!");
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CWeapon!");
	}

	return 0;
}

void CScriptGameObject::SetRemainingUses(u8 value)
{
	if (CEatableItem* eItm = object().cast_eatable_item())
	{
		eItm->SetRemainingUses(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CEatableItem!");
	}
}

u8 CScriptGameObject::GetRemainingUses()
{
	if (CEatableItem* eItm = object().cast_eatable_item())
	{
		return eItm->GetRemainingUses();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CEatableItem!");
	return 0;
}

u8 CScriptGameObject::GetMaxUses()
{
	if (CEatableItem* eItm = object().cast_eatable_item())
	{
		return eItm->GetMaxUses();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CEatableItem!");
	return 0;
}

bool CScriptGameObject::IsAmmo() const
{
	return object().cast_weapon_ammo() != nullptr;
}

void CScriptGameObject::AttachVehicle(CScriptGameObject* veh, bool bForce)
{
	if (CActor* actor = object().cast_actor())
	{
		CHolderCustom* vehicle = smart_cast<CHolderCustom*>(&veh->object());
		if (vehicle != nullptr)
		{
			actor->use_HolderEx(vehicle, bForce);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CHolderCustom!");
		}
	}
}

void CScriptGameObject::DetachVehicle(bool bForce)
{
	if (CActor* actor = object().cast_actor())
	{
		actor->use_HolderEx(nullptr, bForce);
	}
}

CScriptGameObject* CScriptGameObject::GetAttachedVehicle()
{
	CActor* actor = object().cast_actor();
	if (actor == nullptr)
	{
		return 0;
	}

	CHolderCustom* H = actor->Holder();
	if (H == nullptr)
	{
		return 0;
	}

	CGameObject* GO = smart_cast<CGameObject*>(H);
	if (GO == nullptr)
	{
		return 0;
	}

	return GO->lua_game_object();
}

u32 CScriptGameObject::PlayHudMotion(LPCSTR M, bool bMixIn, u32 state)
{
	if (CHudItem* itm = object().cast_hud_item())
	{
		return itm->PlayHUDMotion(M, bMixIn, state);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CHudItem!");
	return 0;
}

void CScriptGameObject::AmmoSetCount(u16 count)
{
	if (CWeaponAmmo* ammo = object().cast_weapon_ammo())
	{
		ammo->m_boxSize = count;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CWeaponAmmo!");
	}
}

u16 CScriptGameObject::AmmoBoxSize()
{
	if (CWeaponAmmo* ammo = object().cast_weapon_ammo())
	{
		return ammo->m_boxSize;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CWeaponAmmo!");
	return 0;
}

bool CScriptGameObject::InstallUpgrade(LPCSTR upgrade)
{
	if (CInventoryItem* item = object().cast_inventory_item())
	{
		if (pSettings->section_exist(upgrade))
		{
			item->pre_install_upgrade();

			shared_str upgrade_id(upgrade);
			return Level().m_upgrade_manager->upgrade_install(*item, upgrade_id, true);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryItem : cannot access class member InstallUpgrade!");
	}

	return false;
}

bool CScriptGameObject::HasUpgrade(LPCSTR upgrade)
{
	if (CInventoryItem* item = object().cast_inventory_item())
	{
		if (pSettings->section_exist(upgrade))
		{
			shared_str upgrade_id(upgrade);
			return item->has_upgrade(upgrade_id);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryItem : cannot access class member HasUpgrade!");
	}

	return false;
}

u16 CScriptGameObject::AmmoGetCount()
{
	if (CWeaponAmmo* ammo = object().cast_weapon_ammo())
	{
		return ammo->m_boxCurr;
	}

	return 0;
}

void CScriptGameObject::SwitchState(u32 state)
{
	if (CHudItem* itm = object().cast_hud_item())
	{
		itm->SwitchState(state);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CHudItem!");
	}
}

u32 CScriptGameObject::GetState()
{
	if (CHudItem* itm = object().cast_hud_item())
	{
		return itm->GetState();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CHudItem!");
	return 65535;
}

bool CScriptGameObject::ActorIsJump() const
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->is_jump();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot be cast to CActor!");
	return false;
}

bool CScriptGameObject::RayPick(const Fvector3& Pos, const Fvector3& Dir, float Range)
{
	collide::rq_result R;
	return Level().ObjectSpace.RayPick(Pos, Dir, Range, (collide::rq_target)(collide::rq_target::rqtBoth | collide::rq_target::rqtObstacle), R, nullptr);
}

const CCoverPoint *CScriptGameObject::best_cover(const Fvector &position, const Fvector &enemy_position, float radius, float min_enemy_distance, float max_enemy_distance)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->m_ce_best->setup(enemy_position, min_enemy_distance, max_enemy_distance, 0.0f);
		const CCoverPoint* point = ai().cover_manager().best_cover(position, radius, *stalker->m_ce_best);
		return point;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member best_cover!");
	return 0;
}

const CCoverPoint *CScriptGameObject::safe_cover(const Fvector &position, float radius, float min_distance)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->m_ce_safe->setup(min_distance);
		const CCoverPoint* point = ai().cover_manager().best_cover(position, radius, *stalker->m_ce_safe);
		return point;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member best_cover!");
	return 0;
}

const xr_vector<MemorySpace::CVisibleObject>& CScriptGameObject::memory_visible_objects() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().visual().objects();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member memory_visible_objects!");
	NODEFAULT;
	return {};
}

const xr_vector<MemorySpace::CSoundObject>& CScriptGameObject::memory_sound_objects() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().sound().objects();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member memory_sound_objects!");
	NODEFAULT;
	return {};
}

const xr_vector<MemorySpace::CHitObject>& CScriptGameObject::memory_hit_objects() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().hit().objects();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member memory_hit_objects!");
	NODEFAULT;
	return {};
}

void CScriptGameObject::ChangeTeam(u8 team, u8 squad, u8 group)
{
	if (CCustomMonster* custom_monster = object().cast_custom_monster())
	{
		custom_monster->ChangeTeam(team, squad, group);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster: cannot access class member ChangeTeam!");
	}
}

void CScriptGameObject::SetVisualMemoryEnabled(bool enabled)
{
	if (CCustomMonster* custom_monster = object().cast_custom_monster())
	{
		custom_monster->memory().visual().enable(enabled);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster: cannot access class member SetVisualMemoryEnabled!");
	}
}

CScriptGameObject *CScriptGameObject::GetEnemy() const
{
	if (CCustomMonster* l_tpCustomMonster = object().cast_custom_monster())
	{
		CEntity* current_enemy = l_tpCustomMonster->GetCurrentEnemy();
		if (l_tpCustomMonster->g_Alive() && current_enemy != nullptr && !current_enemy->getDestroy())
			return current_enemy->lua_game_object();
		else
			return 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member GetEnemy!");
	return 0;
}

CScriptGameObject *CScriptGameObject::GetCorpse() const
{
	if (CCustomMonster* l_tpCustomMonster = object().cast_custom_monster())
	{
		CEntity* current_corpse = l_tpCustomMonster->GetCurrentCorpse();
		if (l_tpCustomMonster->g_Alive() && current_corpse != nullptr && !current_corpse->getDestroy())
			return current_corpse->lua_game_object();
		else
			return 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member GetCurrentCorpse!");
	return 0;
}

bool CScriptGameObject::CheckTypeVisibility(const char *section_name)
{
	if (CCustomMonster* l_tpCustomMonster = object().cast_custom_monster())
	{
		return l_tpCustomMonster->CheckTypeVisibility(section_name);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member CheckTypeVisibility!");
	return false;
}

CScriptGameObject* CScriptGameObject::GetCurrentWeapon() const
{
	if (CAI_Stalker* l_tpStalker = object().cast_stalker())
	{
		CGameObject* current_weapon = l_tpStalker->GetCurrentWeapon();
		return current_weapon != nullptr ? current_weapon->lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member GetCurrentWeapon!");
	return 0;
}

void CScriptGameObject::deadbody_closed(bool status)
{
	if (CInventoryOwner* inventoryOwner = object().cast_inventory_owner())
	{
		inventoryOwner->deadbody_closed(status);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member deadbody_closed!");
	}
}

bool CScriptGameObject::deadbody_closed_status()
{
	if (CInventoryOwner* inventoryOwner = object().cast_inventory_owner())
	{
		return inventoryOwner->deadbody_closed_status();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member deadbody_closed_status!");
	return false;
}

void CScriptGameObject::can_select_weapon(bool status)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->can_select_weapon(status);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member can_select_weapon!");
	}
}

bool CScriptGameObject::can_select_weapon() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->can_select_weapon();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member can_select_weapon!");
	return false;
}

void CScriptGameObject::deadbody_can_take(bool status)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->deadbody_can_take(status);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member deadbody_can_take!");
	}
}

bool CScriptGameObject::deadbody_can_take_status()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->deadbody_can_take_status();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member deadbody_can_take_status!");
	return false;
}

CScriptGameObject *CScriptGameObject::GetCurrentOutfit() const
{
	if (CInventoryOwner* inventoryOwner = object().cast_inventory_owner())
	{
		CGameObject* current_equipment = inventoryOwner->GetOutfit();
		return current_equipment != nullptr ? current_equipment->lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member GetCurrentOutfit!");
	return 0;
}

float CScriptGameObject::GetCurrentOutfitProtection(int hit_type)
{
	if (CInventoryOwner* inventoryOwner = object().cast_inventory_owner())
	{
		if (CCustomOutfit* current_outfit = inventoryOwner->GetOutfit())
		{
			return current_outfit->GetDefHitTypeProtection(ALife::EHitType(hit_type));
		}

		return 0.0f;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member GetCurrentOutfitProtection!");
	return 0.0f;
}

CScriptGameObject *CScriptGameObject::GetFood() const
{
	if (CAI_Stalker* l_tpStalker = object().cast_stalker())
	{
		CGameObject* food = l_tpStalker->GetFood() ? &l_tpStalker->GetFood()->object() : 0;
		return food != nullptr ? food->lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member GetFood!");
	return 0;
}

CScriptGameObject *CScriptGameObject::GetMedikit() const
{
	if (CAI_Stalker* l_tpStalker = object().cast_stalker())
	{
		CGameObject* medkit = l_tpStalker->GetFood() ? &l_tpStalker->GetMedikit()->object() : 0;
		return medkit != nullptr ? medkit->lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member GetMedikit!");
	return 0;
}

LPCSTR CScriptGameObject::GetPatrolPathName()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return *stalker->movement().patrol().path_name();
	}
	else if (CScriptEntity* script_monster = object().cast_script_entity())
	{
		return script_monster->GetPatrolPathName();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member GetPatrolPathName!");
	return "";
}

void CScriptGameObject::add_animation(LPCSTR animation, bool hand_usage, bool use_movement_controller)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (stalker->movement().current_params().cover())
		{
			ai().script_engine().script_log(eLuaMessageTypeError, "Cannot add animation [%s]: object [%s] is in smart_cover!", animation, stalker->cName().c_str());
			//return;???
		}

		if (stalker->animation().global_selector())
		{
			ai().script_engine().script_log(eLuaMessageTypeError, "Cannot add animation [%s]: global selector is set for object [%s], in_smart_cover returned [%s]!", animation, stalker->cName().c_str(), in_smart_cover() ? "true" : "false");
		}
		else
		{
			stalker->animation().add_script_animation(animation, hand_usage, use_movement_controller);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member add_animation!");
	}

}

void CScriptGameObject::add_animation(LPCSTR animation, bool hand_usage, Fvector position, Fvector rotation, bool local_animation)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (stalker->movement().current_params().cover())
		{
			ai().script_engine().script_log(eLuaMessageTypeError, "Cannot add animation [%s]: object [%s] is in smart_cover!", animation, stalker->cName().c_str());
			//return;???
		}

		if (stalker->animation().global_selector())
		{
			ai().script_engine().script_log(eLuaMessageTypeError, "Cannot add animation [%s]: global selector is set for object [%s], in_smart_cover returned [%s]!", animation, stalker->cName().c_str(), in_smart_cover() ? "true" : "false");
		}
		else
		{
			stalker->animation().add_script_animation(animation, hand_usage, position, rotation, local_animation);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member add_animation!");
	}
}

void CScriptGameObject::clear_animations()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->animation().clear_script_animations();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member clear_animations!");
	}
}

int	CScriptGameObject::animation_count() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return (int)stalker->animation().script_animations().size();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member clear_animations!");
	return -1;
}

Flags32 CScriptGameObject::get_actor_relation_flags() const
{
	CAI_Stalker* stalker = object().cast_stalker();
	THROW(stalker);

	return stalker->m_actor_relation_flags;
}

void CScriptGameObject::set_actor_relation_flags(Flags32 flags)
{
	CAI_Stalker* stalker = object().cast_stalker();
	THROW(stalker);

	stalker->m_actor_relation_flags = flags;
}

void CScriptGameObject::set_patrol_path(LPCSTR path_name, const PatrolPathManager::EPatrolStartType patrol_start_type, const PatrolPathManager::EPatrolRouteType patrol_route_type, bool random)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().patrol().set_path(path_name, patrol_start_type, patrol_route_type, random);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::inactualize_patrol_path()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().patrol().make_inactual();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::inactualize_level_path()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().level_path().make_inactual();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::inactualize_game_path()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().game_path().make_inactual();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

u32 CScriptGameObject::get_dest_game_vertex_id()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().game_dest_vertex_id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member get_dest_game_vertex_id!");
	return u32(-1);
}

u32 CScriptGameObject::get_dest_level_vertex_id()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().level_dest_vertex_id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member get_dest_level_vertex_id!");
	return u32(-1);
}

void CScriptGameObject::set_dest_level_vertex_id(u32 level_vertex_id)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!ai().level_graph().valid_vertex_id(level_vertex_id))
		{
#ifdef DEBUG
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : invalid vertex id being setup by action %s!", stalker->brain().CStalkerPlanner::current_action().m_action_name);
#endif
			return;
		}

		if (!stalker->movement().restrictions().accessible(level_vertex_id))
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "! you are trying to setup destination for the stalker %s, which is not accessible by its restrictors in[%s] out[%s]",
			stalker->cName().c_str(), Level().space_restriction_manager().in_restrictions(stalker->ID()).c_str(), Level().space_restriction_manager().out_restrictions(stalker->ID()).c_str());
		}
		else
		{
			stalker->movement().set_level_dest_vertex(level_vertex_id);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_level_vertex_id!");
	}
}

void CScriptGameObject::set_dest_game_vertex_id(GameGraph::_GRAPH_ID game_vertex_id)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (!ai().game_graph().valid_vertex_id(game_vertex_id))
		{
#ifdef DEBUG
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : invalid vertex id being setup by action %s!", stalker->brain().CStalkerPlanner::current_action().m_action_name);
#endif
		}
		else
		{
			stalker->movement().set_game_dest_vertex(game_vertex_id);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_dest_game_vertex_id!");
	}
}

void CScriptGameObject::set_movement_selection_type(ESelectionType selection_type)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().game_selector().set_selection_type(selection_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_movement_selection_type!");
	}
}

CHARACTER_RANK_VALUE CScriptGameObject::GetRank()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->Rank();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member GetRank!");
	return CHARACTER_RANK_VALUE(0);
}

void CScriptGameObject::set_desired_position()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_desired_position(0);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_desired_position(const Fvector* desired_position)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		THROW2(desired_position || stalker->movement().restrictions().accessible(*desired_position), *stalker->cName());
		stalker->movement().set_desired_position(desired_position);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_desired_direction()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_desired_direction(0);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_desired_direction(const Fvector* desired_direction)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if (fsimilar(desired_direction->magnitude(), 0.0f))
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : [%s] set_desired_direction - you passed zero direction!", stalker->cName().c_str());
		}
		else
		{
			if (!fsimilar(desired_direction->magnitude(), 1.0f))
			{
				ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : [%s] set_desired_direction - you passed non-normalized direction!", stalker->cName().c_str());
			}
		}

		Fvector direction = *desired_direction;
		direction.normalize_safe();
		stalker->movement().set_desired_direction(&direction);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_body_state(EBodyState body_state)
{
	THROW((body_state == eBodyStateStand) || (body_state == eBodyStateCrouch));

	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_body_state(body_state);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_movement_type(EMovementType movement_type)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_movement_type(movement_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_mental_state(EMentalState mental_state)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
#if 0//def DEBUG
		if (mental_state != eMentalStateDanger)
		{
			if (stalker->brain().initialized())
			{
				if (stalker->brain().current_action_id() == StalkerDecisionSpace::eWorldOperatorCombatPlanner)
				{
					ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : set_mental_state is used during universal combat!, object[%s]", stalker->cName().c_str());
					//return;
				}
			}
	}
#endif // DEBUG
		stalker->movement().set_mental_state(mental_state);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_path_type(MovementManager::EPathType path_type)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_path_type(path_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

void CScriptGameObject::set_detail_path_type(DetailPathManager::EDetailPathType detail_path_type)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->movement().set_detail_path_type(detail_path_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement!");
	}
}

MonsterSpace::EBodyState CScriptGameObject::body_state() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().body_state();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member body_state!");
	return MonsterSpace::eBodyStateStand;
}

MonsterSpace::EBodyState CScriptGameObject::target_body_state() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().target_body_state();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member body_state!");
	return MonsterSpace::eBodyStateStand;
}

MonsterSpace::EMovementType CScriptGameObject::movement_type() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().movement_type();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member movement_type!");
	return MonsterSpace::eMovementTypeStand;
}

MonsterSpace::EMovementType CScriptGameObject::target_movement_type() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().target_movement_type();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member target_movement_type!");
	return MonsterSpace::eMovementTypeStand;
}

MonsterSpace::EMentalState CScriptGameObject::mental_state() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().mental_state();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member mental_state!");
	return MonsterSpace::eMentalStateDanger;
}

MonsterSpace::EMentalState CScriptGameObject::target_mental_state() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().target_mental_state();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member mental_state!");
	return MonsterSpace::eMentalStateDanger;
}

MovementManager::EPathType CScriptGameObject::path_type() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->movement().path_type();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member path_type!");
	return MovementManager::ePathTypeNoPath;
}

DetailPathManager::EDetailPathType CScriptGameObject::detail_path_type() const
{
	//kak ponyat...
	//if (CAI_Stalker* stalker = object().cast_stalker())
	//{
	//	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member detail_path_type!");
	//	return DetailPathManager::eDetailPathTypeSmooth;
	//}

	return DetailPathManager::eDetailPathTypeSmooth;
}

void CScriptGameObject::set_sight(SightManager::ESightType sight_type, Fvector* vector3d, u32 dwLookOverDelay)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if ((sight_type == SightManager::eSightTypeDirection) && vector3d && (_abs(vector3d->magnitude() - 1.f) > .01f))
		{
			VERIFY2(false, make_string<const char*>("non-normalized direction passed [%f][%f][%f]", VPUSH(*vector3d)));
			vector3d->normalize();
		}

		stalker->sight().setup(sight_type, vector3d);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(SightManager::ESightType sight_type, bool torso_look, bool path)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(sight_type, torso_look, path);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(SightManager::ESightType sight_type, Fvector& vector3d, bool torso_look = false)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if ((sight_type == SightManager::eSightTypeDirection) && (_abs(vector3d.magnitude() - 1.f) > .01f))
		{
			VERIFY2(false, make_string<const char*>("non-normalized direction passed [%f][%f][%f]", VPUSH(vector3d)));
			vector3d.normalize();
		}

		stalker->sight().setup(sight_type, vector3d, torso_look);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(SightManager::ESightType sight_type, Fvector* vector3d)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		if ((sight_type == SightManager::eSightTypeDirection) && vector3d && (_abs(vector3d->magnitude() - 1.f) > .01f))
		{
			VERIFY2(false, make_string<const char*>("non-normalized direction passed [%f][%f][%f]", VPUSH(*vector3d)));
			vector3d->normalize();
		}

		stalker->sight().setup(sight_type, vector3d);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(CScriptGameObject* object_to_look)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(&object_to_look->object());
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(CScriptGameObject* object_to_look, bool torso_look)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(&object_to_look->object(), torso_look);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(CScriptGameObject* object_to_look, bool torso_look, bool fire_object)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(&object_to_look->object(), torso_look, fire_object);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(CScriptGameObject* object_to_look, bool torso_look, bool fire_object, bool no_pitch)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(CSightAction(&object_to_look->object(), torso_look, fire_object, no_pitch));
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

void CScriptGameObject::set_sight(const CMemoryInfo* memory_object, bool torso_look)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->sight().setup(memory_object, torso_look);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSightManager : cannot access class member set_sight!");
	}
}

// CAI_Stalker
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

u32	CScriptGameObject::GetInventoryObjectCount() const
{
	if (CInventoryOwner* l_tpInventoryOwner = object().cast_inventory_owner())
	{
		return (l_tpInventoryOwner->inventory().dwfGetObjectCount());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member obj_count!");
	return 0;
}

CScriptGameObject* CScriptGameObject::GetActiveItem()
{
	if (CInventoryOwner* l_tpInventoryOwner = object().cast_inventory_owner())
	{
		if (l_tpInventoryOwner->inventory().ActiveItem())
		{
			return l_tpInventoryOwner->inventory().ActiveItem()->object().lua_game_object();
		}
		else
		{
			return 0;
		}
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member activge_item!");
	return 0;
}

CScriptGameObject* CScriptGameObject::GetObjectByName(LPCSTR caObjectName) const
{
	if (CInventoryOwner* l_tpInventoryOwner = object().cast_inventory_owner())
	{
		CInventoryItem* l_tpInventoryItem = l_tpInventoryOwner->inventory().GetItemFromInventory(caObjectName);
		CGameObject* l_tpGameObject = l_tpInventoryItem != nullptr ? l_tpInventoryItem->cast_game_object() : nullptr;

		if (l_tpGameObject == nullptr)
		{
			return 0;
		}
		else
		{
			return l_tpGameObject->lua_game_object();
		}
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member object!");
	return 0;
}

CScriptGameObject* CScriptGameObject::GetObjectByIndex(int iIndex) const
{
	if (CInventoryOwner* l_tpInventoryOwner = object().cast_inventory_owner())
	{
		CInventoryItem* l_tpInventoryItem = l_tpInventoryOwner->inventory().tpfGetObjectByIndex(iIndex);
		CGameObject* l_tpGameObject = l_tpInventoryItem != nullptr ? l_tpInventoryItem->cast_game_object() : nullptr;

		if (l_tpGameObject == nullptr)
		{
			return 0;
		}
		else
		{
			return l_tpGameObject->lua_game_object();
		}
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member object!");
	return 0;
}

void CScriptGameObject::EnableAnomaly()
{
	CCustomZone* zone = object().cast_custom_zone();
	THROW(zone);

	zone->ZoneEnable();
}

void CScriptGameObject::DisableAnomaly()
{
	CCustomZone* zone = object().cast_custom_zone();
	THROW(zone);

	zone->ZoneDisable();
}

float CScriptGameObject::GetAnomalyPower()
{
	CCustomZone* zone = object().cast_custom_zone();
	THROW(zone);

	return zone->GetMaxPower();
}

void CScriptGameObject::SetAnomalyPower(float p)
{
	CCustomZone* zone = object().cast_custom_zone();
	THROW(zone);

	zone->SetMaxPower(p);
}

bool CScriptGameObject::weapon_strapped() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->weapon_strapped();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member weapon_strapped!");
	return false;
}

bool CScriptGameObject::weapon_unstrapped() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->weapon_unstrapped();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member weapon_unstrapped!");
	return false;
}

bool CScriptGameObject::path_completed() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->movement().path_completed();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member path_completed!");
	return false;
}

void CScriptGameObject::patrol_path_make_inactual()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().patrol().make_inactual();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member patrol_path_make_inactual!");
	}
}

Fvector	CScriptGameObject::head_orientation() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		const SRotation& r = stalker->movement().head_orientation().current;
		return Fvector().setHP(-r.yaw, -r.pitch);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member head_orientation!");
	return Fvector().set(flt_max, flt_max, flt_max);
}

void CScriptGameObject::info_add(LPCSTR text)
{
	if (g_dedicated_server)
	{
		return;
	}

#ifdef DEBUG
	DBG().object_info(&object(), this).add_item(text, color_xrgb(255, 0, 0), 0);
#endif
}

void CScriptGameObject::info_clear()
{
	if (g_dedicated_server)
	{
		return;
}

#ifdef DEBUG
	DBG().object_info(&object(),this).clear();
#endif
}

void CScriptGameObject::jump(const Fvector &position, float factor)
{
	if (CBaseMonster* monster = object().cast_base_monster())
	{
		monster->jump(position, factor);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot process jump for not a monster!");
	}
}

void CScriptGameObject::make_object_visible_somewhen(CScriptGameObject* object)
{
	if (CAI_Stalker* stalker = this->object().cast_stalker())
	{
		if (CEntityAlive* entity_alive = object->object().cast_entity_alive())
		{
			stalker->memory().make_object_visible_somewhen(entity_alive);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CEntityAlive : cannot access class member make_object_visible_somewhen!");
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member make_object_visible_somewhen!");
	}
}

void CScriptGameObject::sell_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().process(CTradeParameters::action_sell(0), *ini_file, section);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member sell_condition!");
	}
}

void CScriptGameObject::sell_condition(float friend_factor, float enemy_factor)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().default_factors(CTradeParameters::action_sell(0), CTradeFactors(friend_factor, enemy_factor));
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member sell_condition!");
	}
}

void CScriptGameObject::buy_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().process(CTradeParameters::action_buy(0), *ini_file, section);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member buy_condition!");
	}
}

void CScriptGameObject::buy_condition(float friend_factor, float enemy_factor)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().default_factors(CTradeParameters::action_buy(0), CTradeFactors(friend_factor, enemy_factor));
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member buy_condition!");
	}
}

void CScriptGameObject::show_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().process(CTradeParameters::action_show(0), *ini_file, section);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member show_condition!");
	}
}

void CScriptGameObject::buy_supplies(CScriptIniFile* ini_file, LPCSTR section)
{
	if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->buy_supplies(*ini_file, section);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member buy_condition!");
	}
}

void CScriptGameObject::buy_item_condition_factor(float factor)
{
	if (CInventoryOwner * inventory_owner = object().cast_inventory_owner())
	{
		inventory_owner->trade_parameters().buy_item_condition_factor = factor;
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member buy_item_condition_factor!");
	}
}

void sell_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	default_trade_parameters().process(CTradeParameters::action_sell(0), *ini_file, section);
}

void sell_condition(float friend_factor, float enemy_factor)
{
	default_trade_parameters().default_factors(CTradeParameters::action_sell(0), CTradeFactors(friend_factor, enemy_factor));
}

void buy_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	default_trade_parameters().process(CTradeParameters::action_buy(0), *ini_file, section);
}

void buy_condition(float friend_factor, float enemy_factor)
{
	default_trade_parameters().default_factors(CTradeParameters::action_buy(0), CTradeFactors(friend_factor, enemy_factor));
}

void show_condition(CScriptIniFile* ini_file, LPCSTR section)
{
	default_trade_parameters().process(CTradeParameters::action_show(0), *ini_file, section);
}

LPCSTR CScriptGameObject::sound_prefix() const
{
	if (CCustomMonster* custom_monster = object().cast_custom_monster())
	{
		return *custom_monster->sound().sound_prefix();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member sound_prefix!");
	return 0;
}

void CScriptGameObject::sound_prefix(LPCSTR sound_prefix)
{
	if (CCustomMonster* custom_monster = object().cast_custom_monster())
	{
		custom_monster->sound().sound_prefix(sound_prefix);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member sound_prefix!");
	}
}

bool CScriptGameObject::is_weapon_going_to_be_strapped(CScriptGameObject const* object) const
{
	if (object == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member is_weapon_going_to_be_strapped (object passed is null)!");
		return false;
	}

	if (CAI_Stalker const* stalker = this->object().cast_stalker())
	{
		return stalker->is_weapon_going_to_be_strapped(&object->object());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member is_weapon_going_to_be_strapped!");
	return false;
}

float CScriptGameObject::GetArtefactHealthRestoreSpeed()
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	return artefact->GetHealthPower();
}

float CScriptGameObject::GetArtefactRadiationRestoreSpeed()
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	return artefact->GetRadiationPower();
}

float CScriptGameObject::GetArtefactSatietyRestoreSpeed()
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	return artefact->GetSatietyPower();
}
float CScriptGameObject::GetArtefactPowerRestoreSpeed()
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	return artefact->GetPowerPower();
}

float CScriptGameObject::GetArtefactBleedingRestoreSpeed()
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	return artefact->GetBleedingPower();
}

void CScriptGameObject::SetArtefactHealthRestoreSpeed(float value)
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	artefact->SetHealthPower(value);
}

void CScriptGameObject::SetArtefactRadiationRestoreSpeed(float value)
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	artefact->SetRadiationPower(value);
}

void CScriptGameObject::SetArtefactSatietyRestoreSpeed(float value)
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	artefact->SetSatietyPower(value);
}

void CScriptGameObject::SetArtefactPowerRestoreSpeed(float value)
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	artefact->SetPowerPower(value);
}

void CScriptGameObject::SetArtefactBleedingRestoreSpeed(float value)
{
	CArtefact* artefact = object().cast_artefact();
	THROW(artefact);

	artefact->SetBleedingPower(value);
}

bool CScriptGameObject::WeaponInGrenadeMode()
{
	if (CWeaponMagazinedWGrenade* wpn = object().cast_weapon_magazined_w_grenade())
	{
		return wpn->IsGrenadeMode();
	}

	return false;
}

void CScriptGameObject::SetBoneVisible(LPCSTR bone_name, bool bVisibility, bool bRecursive)
{
	if (IKinematics* k = PKinematics(object().Visual()))
	{
		u16 bone_id = k->LL_BoneID(bone_name);
		if (bone_id != BI_NONE && k->LL_GetBoneVisible(bone_id) != (BOOL)bVisibility)
		{
			k->LL_SetBoneVisible(bone_id, bVisibility, bRecursive);
		}
	}
}

bool CScriptGameObject::IsBoneVisible(LPCSTR bone_name)
{
	if (IKinematics* k = PKinematics(object().Visual()))
	{
		u16 bone_id = k->LL_BoneID(bone_name);
		if (bone_id == BI_NONE)
		{
			return false;
		}

		return k->LL_GetBoneVisible(bone_id) == TRUE ? true : false;
	}

	return false;
}

float CScriptGameObject::GetLuminocityHemi()
{
	if (CObject* e = object().dcast_CObject())
	{
		if (e->renderable_ROS())
		{
			return e->renderable_ROS()->get_luminocity_hemi();
		}
	}

	return 0.0f;
}

float CScriptGameObject::GetLuminocity()
{
	if (CObject* e = object().dcast_CObject())
	{
		if (e->renderable_ROS())
		{
			return e->renderable_ROS()->get_luminocity();
		}
	}

	return 0.0f;
}

void CScriptGameObject::ForceSetPosition(Fvector pos, bool bActivate)
{
	Fmatrix M = object().XFORM();
	M.translate(pos);
	object().ForceTransform(M);

	if (CPhysicsShellHolder* sh = object().cast_physics_shell_holder())
	{
		if (bActivate)
		{
			sh->activate_physic_shell();
		}

		if (sh->PPhysicsShell())
		{
			sh->PPhysicsShell()->SetTransform(M, mh_unspecified);
		}
	}
}								 

LPCSTR CScriptGameObject::bones_protection_sect()
{
	if (IKinematics* pKinematics = PKinematics(object().Visual()))
	{
		if (CInifile* ini = pKinematics->LL_UserData())
		{
			return ini->r_string("bone_protection", "bones_protection_sect");
		}
	}

	return "";
}

void CScriptGameObject::RemoveDanger(const CDangerObject& dobject)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().danger().remove(dobject);
	}
}

void CScriptGameObject::SetSpatialType(u32 sptype)
{
	object().SpatialComponent->spatial.type = sptype;
}

u32 CScriptGameObject::GetSpatialType()
{
	return object().SpatialComponent->spatial.type;
}

u8 CScriptGameObject::GetRestrictionType()
{
	if (CSpaceRestrictor* restr = object().cast_restrictor())
	{
		return restr->m_space_restrictor_type;
	}

	return u8(-1);
}

void CScriptGameObject::SetRestrictionType(u8 typ)
{
	if (CSpaceRestrictor* restr = object().cast_restrictor())
	{
		restr->m_space_restrictor_type = typ;
		if (typ != RestrictionSpace::eRestrictorTypeNone)
		{
			Level().space_restriction_manager().register_restrictor(restr, RestrictionSpace::ERestrictorTypes(typ));
		}
	}
}

void CScriptGameObject::setMechanic(bool cond)
{
	CInventoryOwner* invOwn = smart_cast<CInventoryOwner*>(&this->object());

	if (!invOwn)
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member is_weapon_going_to_be_strapped!");

	invOwn->SpecificCharacter().updateMechanic(cond);
}

const bool CScriptGameObject::getMechanic() const
{
	CInventoryOwner const* invOwn = smart_cast<CInventoryOwner const*>(&this->object());

	if (!invOwn)
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryOwner : cannot access class member is_weapon_going_to_be_strapped!");

	return invOwn->SpecificCharacter().upgrade_mechanic();
}