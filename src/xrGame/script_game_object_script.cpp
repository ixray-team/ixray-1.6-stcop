////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object_script.cpp
//	Created 	: 25.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script game object script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "game_object_space.h"
#include "sight_manager_space.h"
#include "../xrScripts/exports/script_ini_file.h"
#include "alife_space.h"
#include "script_entity_space.h"
#include "movement_manager_space.h"
#include "pda_space.h"
#include "cover_point.h"
#include "script_binder_object.h"
#include "script_entity_action.h"
#include "action_planner.h"
#include "relation_registry.h"
#include "InventoryOwner.h"
#include "GameTask.h"
#include "Artefact.h"
#include "Inventory.h"
#include "memory_space.h"
#include "script_monster_hit_info.h"
#include "script_sound_info.h"
#include "ActorHelmet.h"
#include "PhysicObject.h"
#include "ZoneCampfire.h"
#include "holder_custom.h"
#include "PhysicsShellHolder.h"
#include "HangingLamp.h"
#include "Car.h"
#include "helicopter.h"
#include "script_hit.h"
#include "physics_shell_scripted.h"

using namespace luabind;

extern CScriptActionPlanner *script_action_planner(CScriptGameObject *obj);

void IterateActiveItem(CScriptGameObject* Owner, luabind::object func, luabind::object context)
{
	if (Actor()->inventory().GetActiveSlot() != NO_ACTIVE_SLOT)
	{
		try
		{
			luabind::call_function<void>(func, Owner, Actor()->inventory().ItemFromSlot(Actor()->inventory().GetActiveSlot())->object().lua_game_object());
		}
		catch (...)
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Error in iterate_activeitems callback!");
		}
	}

	if (CInventoryItem* result = Actor()->inventory().ItemFromSlot(DEVICE_SLOT))
	{
		luabind::call_function<void>(func, Owner, result->object().lua_game_object());
	}
};

#pragma optimize("s",on)
void CScriptGameObject::script_register(lua_State *L)
{
	class_<CScriptGameObject>	instance("game_object");

	module(L)
	[
		class_<CSightParams>("CSightParams")
			.enum_("bla-bla")
			[
				value("eSightTypeCurrentDirection",		int(SightManager::eSightTypeCurrentDirection	)),
				value("eSightTypePathDirection",		int(SightManager::eSightTypePathDirection		)),
				value("eSightTypeDirection",			int(SightManager::eSightTypeDirection			)),
				value("eSightTypePosition",				int(SightManager::eSightTypePosition			)),
				value("eSightTypeObject",				int(SightManager::eSightTypeObject				)),
				value("eSightTypeCover",				int(SightManager::eSightTypeCover				)),
				value("eSightTypeSearch",				int(SightManager::eSightTypeSearch				)),
				value("eSightTypeLookOver",				int(SightManager::eSightTypeLookOver			)),
				value("eSightTypeCoverLookOver",		int(SightManager::eSightTypeCoverLookOver		)),
				value("eSightTypeFireObject",			int(SightManager::eSightTypeFireObject			)),
				value("eSightTypeFirePosition",			int(SightManager::eSightTypeFirePosition		)),
				value("eSightTypeAnimationDirection",	int(SightManager::eSightTypeAnimationDirection	)),
				value("eSightTypeDummy",				int(SightManager::eSightTypeDummy				))
			]
			.def(							constructor<>())
			.def_readonly("m_object",		&CSightParams::m_object)
			.def_readonly("m_vector",		&CSightParams::m_vector)
			.def_readonly("m_sight_type",	&CSightParams::m_sight_type),

		class_<CScriptGameObject>("game_object")
			.def("set_trader_global_anim",	&CScriptGameObject::set_trader_global_anim)
			.def("set_trader_head_anim",	&CScriptGameObject::set_trader_head_anim)
			.def("set_trader_sound",		&CScriptGameObject::set_trader_sound)
			.def("external_sound_start",	&CScriptGameObject::external_sound_start)
			.def("external_sound_stop",		&CScriptGameObject::external_sound_stop)
		.enum_("relation")
		[
			value("friend",					int(ALife::eRelationTypeFriend)),
			value("neutral",				int(ALife::eRelationTypeNeutral)),
			value("enemy",					int(ALife::eRelationTypeEnemy)),
			value("dummy",					int(ALife::eRelationTypeDummy))
		]
		.enum_("action_types")
		[
			value("movement",				int(ScriptEntity::eActionTypeMovement)),
			value("watch",					int(ScriptEntity::eActionTypeWatch)),
			value("animation",				int(ScriptEntity::eActionTypeAnimation)),
			value("sound",					int(ScriptEntity::eActionTypeSound)),
			value("particle",				int(ScriptEntity::eActionTypeParticle)),
			value("object",					int(ScriptEntity::eActionTypeObject)),
			value("action_type_count",		int(ScriptEntity::eActionTypeCount))
		]
		.enum_("EPathType")
		[
			value("game_path",				int(MovementManager::ePathTypeGamePath)),
			value("level_path",				int(MovementManager::ePathTypeLevelPath)),
			value("patrol_path",			int(MovementManager::ePathTypePatrolPath)),
			value("no_path",				int(MovementManager::ePathTypeNoPath))
		]
		.enum_("ESelectionType")
		[
			value("alifeMovementTypeMask",	int(eSelectionTypeMask)),
			value("alifeMovementTypeRandom",int(eSelectionTypeRandomBranching))
		]
	
//		.property("visible",				&CScriptGameObject::getVisible,			&CScriptGameObject::setVisible)
//		.property("enabled",				&CScriptGameObject::getEnabled,			&CScriptGameObject::setEnabled)

//		.def_readonly("health",				&CScriptGameObject::GetHealth,			&CScriptGameObject::SetHealth)
		.property("health",					&CScriptGameObject::GetHealth,			&CScriptGameObject::SetHealth)
		.property("psy_health",				&CScriptGameObject::GetPsyHealth,		&CScriptGameObject::SetPsyHealth)
		.property("power",					&CScriptGameObject::GetPower,			&CScriptGameObject::SetPower)
		.property("satiety",				&CScriptGameObject::GetSatiety,			&CScriptGameObject::ChangeSatiety)
		.property("radiation",				&CScriptGameObject::GetRadiation,		&CScriptGameObject::SetRadiation)
		.property("morale",					&CScriptGameObject::GetMorale,			&CScriptGameObject::SetMorale)
		.property("bleeding",				&CScriptGameObject::GetBleeding,		&CScriptGameObject::SetBleeding)
		.property("thirst",					&CScriptGameObject::GetThirst,			&CScriptGameObject::SetThirst)
		.property("sleepiness",				&CScriptGameObject::GetSleepiness,		&CScriptGameObject::SetSleepiness)
		.property("intoxication",			&CScriptGameObject::GetIntoxication,	&CScriptGameObject::SetIntoxication)

//		.def("get_bleeding",				&CScriptGameObject::GetBleeding)
		.def("center",						&CScriptGameObject::Center)
		.def("position",					&CScriptGameObject::Position)
		.def("direction",					&CScriptGameObject::Direction)
		.def("clsid",						&CScriptGameObject::clsid)
		.def("id",							&CScriptGameObject::ID)
		.def("story_id",					&CScriptGameObject::story_id)
		.def("section",						&CScriptGameObject::Section)
		.def("name",						&CScriptGameObject::Name)
		.def("parent",						&CScriptGameObject::Parent)
		.def("mass",						&CScriptGameObject::Mass)
		.def("cost",						&CScriptGameObject::Cost)
		.def("condition",					&CScriptGameObject::GetCondition)
		.def("set_condition",				&CScriptGameObject::SetCondition)
		.def("death_time",					&CScriptGameObject::DeathTime)
//		.def("armor",						&CScriptGameObject::Armor)
		.def("max_health",					&CScriptGameObject::MaxHealth)
		.def("accuracy",					&CScriptGameObject::Accuracy)
		.def("alive",						&CScriptGameObject::Alive)
		.def("team",						&CScriptGameObject::Team)
		.def("squad",						&CScriptGameObject::Squad)
		.def("group",						&CScriptGameObject::Group)
		.def("change_team",					(void (CScriptGameObject::*)(u8,u8,u8))(&CScriptGameObject::ChangeTeam))
		.def("set_visual_memory_enabled",	&CScriptGameObject::SetVisualMemoryEnabled)
		.def("kill",						&CScriptGameObject::Kill)
		.def("kill",						&CScriptGameObject::KillNotBypassActorCheck)
		.def("hit",							&CScriptGameObject::Hit)
		.def("play_cycle",					(void (CScriptGameObject::*)(str_c))(&CScriptGameObject::play_cycle))
		.def("play_cycle",					(void (CScriptGameObject::*)(str_c,bool))(&CScriptGameObject::play_cycle))
		.def("fov",							&CScriptGameObject::GetFOV)
		.def("range",						&CScriptGameObject::GetRange)
		.def("relation",					&CScriptGameObject::GetRelationType)
		.def("script",						&CScriptGameObject::SetScriptControl)
		.def("get_script",					&CScriptGameObject::GetScriptControl)
		.def("get_script_name",				&CScriptGameObject::GetScriptControlName)
		.def("reset_action_queue",			&CScriptGameObject::ResetActionQueue)
		.def("see",							&CScriptGameObject::CheckObjectVisibility)
		.def("see",							&CScriptGameObject::CheckTypeVisibility)

		.def("who_hit_name",				&CScriptGameObject::WhoHitName)
		.def("who_hit_section_name",		&CScriptGameObject::WhoHitSectionName)
		
		.def("rank",						&CScriptGameObject::GetRank)
		.def("command",						&CScriptGameObject::AddAction)
		.def("action",						&CScriptGameObject::GetCurrentAction, adopt<0>())
		.def("object_count",				&CScriptGameObject::GetInventoryObjectCount)
		.def("object",						(CScriptGameObject *(CScriptGameObject::*)(str_c))(&CScriptGameObject::GetObjectByName))
		.def("object",						(CScriptGameObject *(CScriptGameObject::*)(int))(&CScriptGameObject::GetObjectByIndex))
		.def("active_item",					&CScriptGameObject::GetActiveItem)
		
		.def("set_callback",				(void (CScriptGameObject::*)(GameObject::ECallbackType, const luabind::functor<void> &))(&CScriptGameObject::SetCallback))
		.def("set_callback",				(void (CScriptGameObject::*)(GameObject::ECallbackType, const luabind::functor<void> &, const luabind::object &))(&CScriptGameObject::SetCallback))
		.def("set_callback",				(void (CScriptGameObject::*)(GameObject::ECallbackType))(&CScriptGameObject::SetCallback))

		.def("set_patrol_extrapolate_callback",	(void (CScriptGameObject::*)())(&CScriptGameObject::set_patrol_extrapolate_callback))
		.def("set_patrol_extrapolate_callback",	(void (CScriptGameObject::*)(const luabind::functor<bool> &))(&CScriptGameObject::set_patrol_extrapolate_callback))
		.def("set_patrol_extrapolate_callback",	(void (CScriptGameObject::*)(const luabind::functor<bool> &, const luabind::object &))(&CScriptGameObject::set_patrol_extrapolate_callback))

		.def("set_enemy_callback",			(void (CScriptGameObject::*)())(&CScriptGameObject::set_enemy_callback))
		.def("set_enemy_callback",			(void (CScriptGameObject::*)(const luabind::functor<bool> &))(&CScriptGameObject::set_enemy_callback))
		.def("set_enemy_callback",			(void (CScriptGameObject::*)(const luabind::functor<bool> &, const luabind::object &))(&CScriptGameObject::set_enemy_callback))

		.def("patrol",						&CScriptGameObject::GetPatrolPathName)

		//FFx0001++
		.def("get_item_additional_description", &CScriptGameObject::GetItemAdditionalDescription)
		.def("set_item_additional_description", &CScriptGameObject::SetItemAdditionalDescription)
		.def("unset_item_additional_description", &CScriptGameObject::UnsetItemAdditionalDescription)
		.def("is_item_used_additional_description", &CScriptGameObject::IsItemUsedAdditionalDescription)

		.def("get_ammo_in_magazine",		&CScriptGameObject::GetAmmoElapsed)
		.def("get_ammo_in_magazine_and_chamber", &CScriptGameObject::GetAmmoElapsedWithChamber) //FFx0001++
		.def("is_weapon_use_chamber",			 &CScriptGameObject::IsWeaponUseChamber) //FFx0001++
		.def("get_ammo_total",				&CScriptGameObject::GetSuitableAmmoTotal)
		.def("set_ammo_elapsed",			&CScriptGameObject::SetAmmoElapsed)


		.def("set_sub_inventory_icon_text", &CScriptGameObject::SetSubIconText)
		.def("set_sub_inventory_icon",		&CScriptGameObject::SetSubIcon)

		//Alundaio
		.def("use",							&CScriptGameObject::Use)
		.def("start_trade",					&CScriptGameObject::StartTrade)
		.def("start_upgrade",				&CScriptGameObject::StartUpgrade)
		.def("set_ammo_type",				&CScriptGameObject::SetAmmoType)
		.def("get_ammo_type",				&CScriptGameObject::GetAmmoType)
		.def("get_ammo_count_for_type",     &CScriptGameObject::GetAmmoCount)
		.def("get_main_weapon_type",		&CScriptGameObject::GetMainWeaponType)
		.def("get_weapon_type",				&CScriptGameObject::GetWeaponType)
		.def("set_main_weapon_type",		&CScriptGameObject::SetMainWeaponType)
		.def("set_weapon_type",				&CScriptGameObject::SetWeaponType)
		.def("has_ammo_type",				&CScriptGameObject::HasAmmoType)
		.def("get_weapon_substate",			&CScriptGameObject::GetWeaponSubstate)
		.def("set_weight",					&CScriptGameObject::SetWeight)
		//-Alundaio			
		.def("set_queue_size",				&CScriptGameObject::SetQueueSize)
//		.def("best_hit",					&CScriptGameObject::GetBestHit)
//		.def("best_sound",					&CScriptGameObject::GetBestSound)
		.def("best_danger",					&CScriptGameObject::GetBestDanger)
		.def("best_enemy",					&CScriptGameObject::GetBestEnemy)
		.def("best_item",					&CScriptGameObject::GetBestItem)
		.def("action_count",				&CScriptGameObject::GetActionCount)
		.def("action_by_index",				&CScriptGameObject::GetActionByIndex)
		
		.def("memory_time",					&CScriptGameObject::memory_time)
		.def("memory_position",				&CScriptGameObject::memory_position)
		.def("best_weapon",					&CScriptGameObject::best_weapon)
		.def("explode",						&CScriptGameObject::explode)
		.def("get_enemy",					&CScriptGameObject::GetEnemy)
		.def("get_corpse",					&CScriptGameObject::GetCorpse)
		.def("get_enemy_strength",			&CScriptGameObject::GetEnemyStrength)
		.def("get_sound_info",				&CScriptGameObject::GetSoundInfo)
		.def("get_monster_hit_info",		&CScriptGameObject::GetMonsterHitInfo)
		.def("bind_object",					&CScriptGameObject::bind_object,adopt<2>())
		.def("motivation_action_manager",	&script_action_planner)

		// basemonster
		.def("set_force_anti_aim",			&CScriptGameObject::set_force_anti_aim)
		.def("get_force_anti_aim",			&CScriptGameObject::get_force_anti_aim)

		.def("set_override_animation",		&CScriptGameObject::set_override_animation)
		.def("clear_override_animation",	&CScriptGameObject::clear_override_animation)

		// burer
		.def("burer_set_force_gravi_attack",&CScriptGameObject::burer_set_force_gravi_attack)
		.def("burer_get_force_gravi_attack",&CScriptGameObject::burer_get_force_gravi_attack)

		// poltergeist
		.def("poltergeist_set_actor_ignore",&CScriptGameObject::poltergeist_set_actor_ignore)
		.def("poltergeist_get_actor_ignore",&CScriptGameObject::poltergeist_get_actor_ignore)

		// bloodsucker
		.def("force_visibility_state",		&CScriptGameObject::force_visibility_state)
		.def("get_visibility_state",		&CScriptGameObject::get_visibility_state)
		.def("force_stand_sleep_animation",	&CScriptGameObject::force_stand_sleep_animation)
		.def("release_stand_sleep_animation",	&CScriptGameObject::release_stand_sleep_animation)
		.def("set_invisible",				&CScriptGameObject::set_invisible)
		.def("set_manual_invisibility",		&CScriptGameObject::set_manual_invisibility)
		.def("set_alien_control",			&CScriptGameObject::set_alien_control)
		.def("set_enemy",					&CScriptGameObject::set_enemy)
		.def("set_vis_state",				&CScriptGameObject::set_vis_state)
		.def("set_collision_off",			&CScriptGameObject::off_collision)
		.def("set_capture_anim",			&CScriptGameObject::bloodsucker_drag_jump)

		// zombie
		.def("fake_death_fall_down",		&CScriptGameObject::fake_death_fall_down)
		.def("fake_death_stand_up",			&CScriptGameObject::fake_death_stand_up)

		// base monster
		.def("skip_transfer_enemy",			&CScriptGameObject::skip_transfer_enemy)
		.def("set_home",					(void (CScriptGameObject::*)(str_c,float,float,bool,float))(&CScriptGameObject::set_home))
		.def("set_home",					(void (CScriptGameObject::*)(u32,float,float,bool,float))(&CScriptGameObject::set_home))
		.def("set_home", +[](CScriptGameObject* self, const char* name, float r_min, float r_max, bool aggressive)
			{
				const float r_middle = (r_min + r_max) / 2;
				self->set_home(name, r_min, r_max, aggressive, r_middle);
			})
		.def("remove_home",					&CScriptGameObject::remove_home)
		.def("berserk",						&CScriptGameObject::berserk)
		.def("can_script_capture",			&CScriptGameObject::can_script_capture)
		.def("set_custom_panic_threshold",	&CScriptGameObject::set_custom_panic_threshold)
		.def("set_default_panic_threshold",	&CScriptGameObject::set_default_panic_threshold)

		// inventory owner
		.def("get_current_outfit",			&CScriptGameObject::GetCurrentOutfit)
		.def("get_current_outfit_protection",&CScriptGameObject::GetCurrentOutfitProtection)
		
		.def("deadbody_closed",				&CScriptGameObject::deadbody_closed)
		.def("deadbody_closed_status",		&CScriptGameObject::deadbody_closed_status)
		.def("deadbody_can_take",			&CScriptGameObject::deadbody_can_take)
		.def("deadbody_can_take_status",	&CScriptGameObject::deadbody_can_take_status)

		.def("can_select_weapon",			(bool (CScriptGameObject::*)() const)&CScriptGameObject::can_select_weapon)
		.def("can_select_weapon",			(void (CScriptGameObject::*)(bool))&CScriptGameObject::can_select_weapon)
		// searchlight
		.def("get_current_direction",		&CScriptGameObject::GetCurrentDirection)

		// movement manager
		.def("set_body_state",				&CScriptGameObject::set_body_state			)
		.def("set_movement_type",			&CScriptGameObject::set_movement_type		)
		.def("set_mental_state",			&CScriptGameObject::set_mental_state		)
		.def("set_path_type",				&CScriptGameObject::set_path_type			)
		.def("set_detail_path_type",		&CScriptGameObject::set_detail_path_type	)

		.def("body_state",					&CScriptGameObject::body_state				)
		.def("target_body_state",			&CScriptGameObject::target_body_state		)
		.def("movement_type",				&CScriptGameObject::movement_type			)
		.def("target_movement_type",		&CScriptGameObject::target_movement_type	)
		.def("mental_state",				&CScriptGameObject::mental_state			)
		.def("target_mental_state",			&CScriptGameObject::target_mental_state		)
		.def("path_type",					&CScriptGameObject::path_type				)
		.def("detail_path_type",			&CScriptGameObject::detail_path_type		)

		//
		.def("set_desired_position",		(void (CScriptGameObject::*)())(&CScriptGameObject::set_desired_position))
		.def("set_desired_position",		(void (CScriptGameObject::*)(const Fvector *))(&CScriptGameObject::set_desired_position))
		.def("set_desired_direction",		(void (CScriptGameObject::*)())(&CScriptGameObject::set_desired_direction))
		.def("set_desired_direction",		(void (CScriptGameObject::*)(const Fvector *))(&CScriptGameObject::set_desired_direction))
		.def("set_patrol_path",				&CScriptGameObject::set_patrol_path)
		.def("inactualize_patrol_path",		&CScriptGameObject::inactualize_patrol_path)
		.def("inactualize_level_path",		&CScriptGameObject::inactualize_level_path)
		.def("inactualize_game_path",		&CScriptGameObject::inactualize_game_path)
		.def("set_dest_level_vertex_id",	&CScriptGameObject::set_dest_level_vertex_id)
		.def("get_dest_level_vertex_id",	&CScriptGameObject::get_dest_level_vertex_id)
		.def("set_dest_game_vertex_id",		&CScriptGameObject::set_dest_game_vertex_id)
		.def("get_dest_game_vertex_id",		&CScriptGameObject::get_dest_game_vertex_id)
		.def("set_movement_selection_type",	&CScriptGameObject::set_movement_selection_type)
		.def("level_vertex_id",				&CScriptGameObject::level_vertex_id)
		.def("game_vertex_id",				&CScriptGameObject::game_vertex_id)
		.def("add_animation",				(void (CScriptGameObject::*)(str_c, bool, bool))(&CScriptGameObject::add_animation))
		.def("add_animation",				(void (CScriptGameObject::*)(str_c, bool, Fvector, Fvector, bool))(&CScriptGameObject::add_animation))
		.def("clear_animations",			&CScriptGameObject::clear_animations)
		.def("animation_count",				&CScriptGameObject::animation_count)
		.def("animation_slot",				&CScriptGameObject::animation_slot)

		.def("ignore_monster_threshold",				&CScriptGameObject::set_ignore_monster_threshold)
		.def("restore_ignore_monster_threshold",		&CScriptGameObject::restore_ignore_monster_threshold)
		.def("ignore_monster_threshold",				&CScriptGameObject::ignore_monster_threshold)
		.def("max_ignore_monster_distance",				&CScriptGameObject::set_max_ignore_monster_distance)
		.def("restore_max_ignore_monster_distance",		&CScriptGameObject::restore_max_ignore_monster_distance)
		.def("max_ignore_monster_distance",				&CScriptGameObject::max_ignore_monster_distance)

		.def("eat",							&CScriptGameObject::eat)

		.def("extrapolate_length",			(float (CScriptGameObject::*)() const)(&CScriptGameObject::extrapolate_length))
		.def("extrapolate_length",			(void (CScriptGameObject::*)(float))(&CScriptGameObject::extrapolate_length))

		.def("set_fov",						&CScriptGameObject::set_fov)
		.def("set_range",					&CScriptGameObject::set_range)

		.def("head_orientation",			&CScriptGameObject::head_orientation)

		.def("set_actor_position",			&CScriptGameObject::SetActorPosition)
		.def("set_actor_direction",			&CScriptGameObject::SetActorDirection)
		.def("camera_move",					&CScriptGameObject::CameraMove) // FNAS
		.def("switch_torch",				&CScriptGameObject::SwitchTorch) // FNAS
		.def("set_actor_crouch",			&CScriptGameObject::SetActorCrouch) // FNAS
		.def("disable_hit_marks",			(void (CScriptGameObject::*)	(bool))&CScriptGameObject::DisableHitMarks)
		.def("disable_hit_marks",			(bool (CScriptGameObject::*)	() const)&CScriptGameObject::DisableHitMarks)
		.def("get_movement_speed",			&CScriptGameObject::GetMovementSpeed)

		.def("set_npc_position",			&CScriptGameObject::SetNpcPosition)

		.def("vertex_in_direction",			&CScriptGameObject::vertex_in_direction)

		.def("item_in_slot",				&CScriptGameObject::item_in_slot)
		.def("active_detector",				&CScriptGameObject::active_detector)
		.def("active_slot",					&CScriptGameObject::active_slot)
		.def("activate_slot",				&CScriptGameObject::activate_slot)

#ifdef DEBUG
		.def("debug_planner",				&CScriptGameObject::debug_planner)
#endif // DEBUG
		.def("invulnerable",				(bool (CScriptGameObject::*)() const)&CScriptGameObject::invulnerable)
		.def("invulnerable",				(void (CScriptGameObject::*)(bool))&CScriptGameObject::invulnerable)

		.def("get_smart_cover_description",	&CScriptGameObject::get_smart_cover_description)
		.def("set_visual_name",				&CScriptGameObject::set_visual_name)
		.def("set_visual_name",				&CScriptGameObject::set_visual_name_notForce)
		.def("get_visual_name",				&CScriptGameObject::get_visual_name)

		.def("can_throw_grenades",			(bool (CScriptGameObject::*)	() const)&CScriptGameObject::can_throw_grenades)
		.def("can_throw_grenades",			(void (CScriptGameObject::*)	(bool ))&CScriptGameObject::can_throw_grenades)

		.def("group_throw_time_interval",	(u32 (CScriptGameObject::*)		() const)&CScriptGameObject::group_throw_time_interval)
		.def("group_throw_time_interval",	(void (CScriptGameObject::*)	(u32 ))&CScriptGameObject::group_throw_time_interval)

		.def("register_in_combat",			&CScriptGameObject::register_in_combat)
		.def("unregister_in_combat",		&CScriptGameObject::unregister_in_combat)
		.def("find_best_cover",				&CScriptGameObject::find_best_cover)

		.def("use_smart_covers_only",		(bool (CScriptGameObject::*)	() const)&CScriptGameObject::use_smart_covers_only)
		.def("use_smart_covers_only",		(void (CScriptGameObject::*)	(bool ))&CScriptGameObject::use_smart_covers_only)

		.def("in_smart_cover",				&CScriptGameObject::in_smart_cover)

		.def("set_dest_smart_cover",		(void (CScriptGameObject::*)	(str_c))&CScriptGameObject::set_dest_smart_cover)
		.def("set_dest_smart_cover",		(void (CScriptGameObject::*)	())&CScriptGameObject::set_dest_smart_cover)
		.def("get_dest_smart_cover",		(CCoverPoint const* (CScriptGameObject::*) ())&CScriptGameObject::get_dest_smart_cover)
		.def("get_dest_smart_cover_name",	&CScriptGameObject::get_dest_smart_cover_name)

		.def("set_dest_loophole",			(void (CScriptGameObject::*)	(str_c))&CScriptGameObject::set_dest_loophole)
		.def("set_dest_loophole",			(void (CScriptGameObject::*)	())&CScriptGameObject::set_dest_loophole)

		.def("set_smart_cover_target",		(void (CScriptGameObject::*)	(Fvector))&CScriptGameObject::set_smart_cover_target)
		.def("set_smart_cover_target",		(void (CScriptGameObject::*)	(CScriptGameObject*))&CScriptGameObject::set_smart_cover_target)
		.def("set_smart_cover_target",		(void (CScriptGameObject::*)	())&CScriptGameObject::set_smart_cover_target)

		.def("set_smart_cover_target_selector",	(void (CScriptGameObject::*)(luabind::functor<void>))&CScriptGameObject::set_smart_cover_target_selector)
		.def("set_smart_cover_target_selector",	(void (CScriptGameObject::*)(luabind::functor<void>, luabind::object))&CScriptGameObject::set_smart_cover_target_selector)
		.def("set_smart_cover_target_selector",	(void (CScriptGameObject::*)())&CScriptGameObject::set_smart_cover_target_selector)

		.def("set_smart_cover_target_idle",				&CScriptGameObject::set_smart_cover_target_idle)
		.def("set_smart_cover_target_lookout",			&CScriptGameObject::set_smart_cover_target_lookout)
		.def("set_smart_cover_target_fire",				&CScriptGameObject::set_smart_cover_target_fire)
		.def("set_smart_cover_target_fire_no_lookout",	&CScriptGameObject::set_smart_cover_target_fire_no_lookout)
		.def("set_smart_cover_target_default",			&CScriptGameObject::set_smart_cover_target_default)

		.def("idle_min_time",				(void (CScriptGameObject::*)	(float))&CScriptGameObject::idle_min_time)
		.def("idle_min_time",				(const float (CScriptGameObject::*)	() const)&CScriptGameObject::idle_min_time)

		.def("idle_max_time",				(void (CScriptGameObject::*)	(float))&CScriptGameObject::idle_max_time)
		.def("idle_max_time",				(const float (CScriptGameObject::*)	() const)&CScriptGameObject::idle_max_time)

		.def("lookout_min_time",			(void (CScriptGameObject::*)	(float))&CScriptGameObject::lookout_min_time)
		.def("lookout_min_time",			(const float (CScriptGameObject::*)	() const)&CScriptGameObject::lookout_min_time)

		.def("lookout_max_time",			(void (CScriptGameObject::*)	(float))&CScriptGameObject::lookout_max_time)
		.def("lookout_max_time",			(const float (CScriptGameObject::*)	() const)&CScriptGameObject::lookout_max_time)

		.def("in_loophole_fov",				&CScriptGameObject::in_loophole_fov)
		.def("in_current_loophole_fov",		&CScriptGameObject::in_current_loophole_fov)
		.def("in_loophole_range",			&CScriptGameObject::in_loophole_range)
		.def("in_current_loophole_range",	&CScriptGameObject::in_current_loophole_range)

		.def("apply_loophole_direction_distance",	(void (CScriptGameObject::*)	(float))&CScriptGameObject::apply_loophole_direction_distance)
		.def("apply_loophole_direction_distance",	(float (CScriptGameObject::*)	() const)&CScriptGameObject::apply_loophole_direction_distance)

		.def("movement_target_reached",			&CScriptGameObject::movement_target_reached)
		.def("suitable_smart_cover",			&CScriptGameObject::suitable_smart_cover)

		.def("take_items_enabled",				(void (CScriptGameObject::*)	(bool))&CScriptGameObject::take_items_enabled)
		.def("take_items_enabled",				(bool (CScriptGameObject::*)	() const)&CScriptGameObject::take_items_enabled)

		.def("death_sound_enabled",				(void (CScriptGameObject::*)	(bool))&CScriptGameObject::death_sound_enabled)
		.def("death_sound_enabled",				(bool (CScriptGameObject::*)	() const)&CScriptGameObject::death_sound_enabled)

		.def("register_door_for_npc",			&CScriptGameObject::register_door)
		.def("unregister_door_for_npc",			&CScriptGameObject::unregister_door)
		.def("on_door_is_open",					&CScriptGameObject::on_door_is_open)
		.def("on_door_is_closed",				&CScriptGameObject::on_door_is_closed)
		.def("lock_door_for_npc",				&CScriptGameObject::lock_door_for_npc)
		.def("unlock_door_for_npc",				&CScriptGameObject::unlock_door_for_npc)
		.def("is_door_locked_for_npc",			&CScriptGameObject::is_door_locked_for_npc)
		.def("is_door_blocked_by_npc",			&CScriptGameObject::is_door_blocked_by_npc)
		.def("is_weapon_going_to_be_strapped",	&CScriptGameObject::is_weapon_going_to_be_strapped)
		.def("start_hud_animator", &CScriptGameObject::StartActorAnimator)
		.def("stop_hud_animator", &CScriptGameObject::StopActorAnimator)
		.def("is_hud_animator_active", &CScriptGameObject::IsAnimatorActive)
		.def("get_hud_animator_section", &CScriptGameObject::GetActorAnimatorSection)
		.def("get_hud_animator_restored_slot", &CScriptGameObject::GetActorAnimatorRestoredSlot)
		.def("get_hud_animator_force_hide_items", &CScriptGameObject::GetAnimatorForceHideItems)
		.def("set_hud_animator_force_hide_items", &CScriptGameObject::SetAnimatorForceHideItems)
		.def("show_state_animator", &CScriptGameObject::ShowStateAnimator)
		.def("hide_state_animator", &CScriptGameObject::HideStateAnimator)

		.def("IsSafemode", (bool (CScriptGameObject::*)() const)& CScriptGameObject::IsActorSafemode)
		.def("SetSafemode", &CScriptGameObject::SetActorSafemode)
	
		.def("UseExternalStorageForTrade", &CScriptGameObject::UseExternalStorageForTrade)
			
		.def("SetCharacterMaxWeight",			&CScriptGameObject::SetCharacterMaxWeight)

		.property("mechanic",					&CScriptGameObject::getMechanic, &CScriptGameObject::setMechanic)
		.def("add_sound",					(u32 (CScriptGameObject::*)(str_c,u32,ESoundTypes,u32,u32,u32))(&CScriptGameObject::add_sound))
		.def("add_sound",					(u32 (CScriptGameObject::*)(str_c,u32,ESoundTypes,u32,u32,u32,str_c))(&CScriptGameObject::add_sound))
		.def("add_combat_sound",			(u32 (CScriptGameObject::*)(str_c,u32,ESoundTypes,u32,u32,u32,str_c))(&CScriptGameObject::add_combat_sound))
		.def("remove_sound",				&CScriptGameObject::remove_sound)
		.def("set_sound_mask",				&CScriptGameObject::set_sound_mask)
		.def("play_sound",					(void (CScriptGameObject::*)(u32))(&CScriptGameObject::play_sound))
		.def("play_sound",					(void (CScriptGameObject::*)(u32,u32))(&CScriptGameObject::play_sound))
		.def("play_sound",					(void (CScriptGameObject::*)(u32,u32,u32))(&CScriptGameObject::play_sound))
		.def("play_sound",					(void (CScriptGameObject::*)(u32,u32,u32,u32))(&CScriptGameObject::play_sound))
		.def("play_sound",					(void (CScriptGameObject::*)(u32,u32,u32,u32,u32))(&CScriptGameObject::play_sound))
		.def("play_sound",					(void (CScriptGameObject::*)(u32,u32,u32,u32,u32,u32))(&CScriptGameObject::play_sound))
		.def("binded_object",				&CScriptGameObject::binded_object)
		.def("set_previous_point",			&CScriptGameObject::set_previous_point)
		.def("set_start_point",				&CScriptGameObject::set_start_point)
		.def("get_current_point_index",		&CScriptGameObject::get_current_patrol_point_index)
		.def("path_completed",				&CScriptGameObject::path_completed)
		.def("patrol_path_make_inactual",	&CScriptGameObject::patrol_path_make_inactual)
		.def("enable_memory_object",		&CScriptGameObject::enable_memory_object)
		.def("active_sound_count",			(int (CScriptGameObject::*)())(&CScriptGameObject::active_sound_count))
		.def("active_sound_count",			(int (CScriptGameObject::*)(bool))(&CScriptGameObject::active_sound_count))
		.def("active_sound_count_script",	(int (CScriptGameObject::*)())(&CScriptGameObject::active_sound_count_script))
		.def("active_sound_count_script",	(int (CScriptGameObject::*)(bool))(&CScriptGameObject::active_sound_count_script))
		.def("best_cover",					&CScriptGameObject::best_cover)
		.def("safe_cover",					&CScriptGameObject::safe_cover)
		.def("spawn_ini",					&CScriptGameObject::spawn_ini)
		.def("memory_visible_objects",		&CScriptGameObject::memory_visible_objects, return_stl_iterator)
		.def("memory_sound_objects",		&CScriptGameObject::memory_sound_objects, return_stl_iterator)
		.def("memory_hit_objects",			&CScriptGameObject::memory_hit_objects, return_stl_iterator)
		.def("not_yet_visible_objects",		&CScriptGameObject::not_yet_visible_objects, return_stl_iterator)
		.def("visibility_threshold",		&CScriptGameObject::visibility_threshold)
		.def("enable_vision",				&CScriptGameObject::enable_vision)
		.def("vision_enabled",				&CScriptGameObject::vision_enabled)
		.def("set_sound_threshold",			&CScriptGameObject::set_sound_threshold)
		.def("restore_sound_threshold",		&CScriptGameObject::restore_sound_threshold)

		// sight manager
		.def("set_sight",					(void (CScriptGameObject::*)(SightManager::ESightType sight_type, Fvector *vector3d, u32 dwLookOverDelay))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(SightManager::ESightType sight_type, bool torso_look, bool path))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(SightManager::ESightType sight_type, Fvector &vector3d, bool torso_look))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(SightManager::ESightType sight_type, Fvector *vector3d))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(CScriptGameObject *object_to_look))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(CScriptGameObject *object_to_look, bool torso_look))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(CScriptGameObject *object_to_look, bool torso_look, bool fire_object))(&CScriptGameObject::set_sight))
		.def("set_sight",					(void (CScriptGameObject::*)(CScriptGameObject *object_to_look, bool torso_look, bool fire_object, bool no_pitch))(&CScriptGameObject::set_sight))
//		.def("set_sight",					(void (CScriptGameObject::*)(const MemorySpace::CMemoryInfo *memory_object, bool	torso_look))(&CScriptGameObject::set_sight))

		// object handler
		.def("set_item",					(void (CScriptGameObject::*)(MonsterSpace::EObjectAction ))(&CScriptGameObject::set_item))
		.def("set_item",					(void (CScriptGameObject::*)(MonsterSpace::EObjectAction, CScriptGameObject *))(&CScriptGameObject::set_item))
		.def("set_item",					(void (CScriptGameObject::*)(MonsterSpace::EObjectAction, CScriptGameObject *, u32))(&CScriptGameObject::set_item))
		.def("set_item",					(void (CScriptGameObject::*)(MonsterSpace::EObjectAction, CScriptGameObject *, u32, u32))(&CScriptGameObject::set_item))

		.def("bone_position",				(Fvector (CScriptGameObject::*)(u16))(&CScriptGameObject::bone_position))
		.def("bone_position",				(Fvector (CScriptGameObject::*)(const char*))(&CScriptGameObject::bone_position))

		.def("bone_direction",				(Fvector(CScriptGameObject::*)(u16))(&CScriptGameObject::bone_direction))
		.def("bone_direction",				(Fvector(CScriptGameObject::*)(const char*))(&CScriptGameObject::bone_direction))
		
		.def("get_bone_name_by_id",			&CScriptGameObject::get_bone_name_by_id)
		.def("get_bone_id_by_name",			&CScriptGameObject::get_bone_id_by_name)

		.def("get_root_bone_name",			&CScriptGameObject::get_root_bone_name)
		.def("get_root_bone_id",			&CScriptGameObject::get_root_bone_id)

		.def("is_body_turning",				&CScriptGameObject::is_body_turning)

		//////////////////////////////////////////////////////////////////////////
		// Space restrictions
		//////////////////////////////////////////////////////////////////////////
		.def("add_restrictions",			&CScriptGameObject::add_restrictions)
		.def("remove_restrictions",			&CScriptGameObject::remove_restrictions)
		.def("remove_all_restrictions",		&CScriptGameObject::remove_all_restrictions)
		.def("in_restrictions",				&CScriptGameObject::in_restrictions)
		.def("out_restrictions",			&CScriptGameObject::out_restrictions)
		.def("base_in_restrictions",		&CScriptGameObject::base_in_restrictions)
		.def("base_out_restrictions",		&CScriptGameObject::base_out_restrictions)
		.def("accessible",					&CScriptGameObject::accessible_position)
		.def("accessible",					&CScriptGameObject::accessible_vertex_id)
		.def("accessible_nearest",			&CScriptGameObject::accessible_nearest, out_value<3>())

		//////////////////////////////////////////////////////////////////////////
		.def("enable_attachable_item",		&CScriptGameObject::enable_attachable_item)
		.def("attachable_item_enabled",		&CScriptGameObject::attachable_item_enabled)
		.def("night_vision_allowed",		&CScriptGameObject::night_vision_allowed)
		.def("enable_night_vision",			&CScriptGameObject::enable_night_vision)
		.def("night_vision_enabled",		&CScriptGameObject::night_vision_enabled)
		.def("enable_torch",				&CScriptGameObject::enable_torch)
		.def("torch_enabled",				&CScriptGameObject::torch_enabled)
		.def("attachable_item_load_attach", &CScriptGameObject::attachable_item_load_attach)
		.def("weapon_strapped",				&CScriptGameObject::weapon_strapped)
		.def("weapon_unstrapped",			&CScriptGameObject::weapon_unstrapped)

		//////////////////////////////////////////////////////////////////////////
		//inventory owner
		//////////////////////////////////////////////////////////////////////////

		.enum_("EPdaMsg")
		[
			value("dialog_pda_msg",			int(ePdaMsgDialog)),
			value("info_pda_msg",			int(ePdaMsgInfo)),
			value("no_pda_msg",				int(ePdaMsgMax))
		]

		.def("give_info_portion",			&CScriptGameObject::GiveInfoPortion)
		.def("disable_info_portion",		&CScriptGameObject::DisableInfoPortion)
		.def("give_game_news",				(void (CScriptGameObject::*)(str_c,str_c,str_c,int,int))(&CScriptGameObject::GiveGameNews))
		.def("give_game_news",				(void (CScriptGameObject::*)(str_c,str_c,str_c,int,int,int))(&CScriptGameObject::GiveGameNews))
		.def("give_game_news",				(bool (CScriptGameObject::*)(str_c,str_c,Frect,int,int))(&CScriptGameObject::GiveGameNews))

        .def("give_talk_message",			(void (CScriptGameObject::*)(str_c,str_c,Frect,str_c))(&CScriptGameObject::AddIconedTalkMessage))
		.def("give_talk_message",			(void (CScriptGameObject::*)(str_c,str_c,str_c))(&CScriptGameObject::AddIconedTalkMessage_old))//old version, must remove!
		.def("give_talk_message2",			(void (CScriptGameObject::*)(str_c,str_c,str_c,str_c))(&CScriptGameObject::AddIconedTalkMessage))

		.def("has_info",					&CScriptGameObject::HasInfo)
		.def("dont_has_info",				&CScriptGameObject::DontHasInfo)
		.def("get_info_time",				&CScriptGameObject::GetInfoTime)

		.def("get_task_state",				&CScriptGameObject::GetGameTaskState)
        .def("get_task_state", +[](CScriptGameObject* self, const char* task_id)
        {
            return self->GetGameTaskState(task_id, ROOT_TASK_OBJECTIVE);
        })
		.def("set_task_state",				&CScriptGameObject::SetGameTaskState)
        .def("set_task_state", +[](CScriptGameObject* self, ETaskState state, const char* task_id)
        {
            self->SetGameTaskState(state, task_id, ROOT_TASK_OBJECTIVE);
        })
		.def("give_task",					&CScriptGameObject::GiveTaskToActor,		adopt<2>())
        .def("give_task", +[](CScriptGameObject* self, CGameTask* t, u32 dt, bool bCheckExisting)
        {
            self->GiveTaskToActor(t, dt, bCheckExisting, 0);
        }, adopt<2>())
		.def("set_active_task",				&CScriptGameObject::SetActiveTask)
		.def("is_active_task",				&CScriptGameObject::IsActiveTask)
		.def("get_task",					&CScriptGameObject::GetTask)

		.def("is_talking",					&CScriptGameObject::IsTalking)
		.def("stop_talk",					&CScriptGameObject::StopTalk)
		.def("enable_talk",					&CScriptGameObject::EnableTalk)
		.def("disable_talk",				&CScriptGameObject::DisableTalk)
		.def("is_talk_enabled",				&CScriptGameObject::IsTalkEnabled)

		.def("enable_trade",				&CScriptGameObject::EnableTrade)
		.def("disable_trade",				&CScriptGameObject::DisableTrade)
		.def("is_trade_enabled",			&CScriptGameObject::IsTradeEnabled)
		.def("enable_inv_upgrade",			&CScriptGameObject::EnableInvUpgrade)
		.def("disable_inv_upgrade",			&CScriptGameObject::DisableInvUpgrade)
		.def("is_inv_upgrade_enabled",		&CScriptGameObject::IsInvUpgradeEnabled)

		.def("disable_show_hide_sounds",	&CScriptGameObject::SetPlayShHdRldSounds)
		.def("inventory_for_each",			&CScriptGameObject::ForEachInventoryItems)
		.def("drop_item",					&CScriptGameObject::DropItem)
		.def("drop_item_and_teleport",		&CScriptGameObject::DropItemAndTeleport)
		.def("transfer_item",				&CScriptGameObject::TransferItem)
		.def("transfer_money",				&CScriptGameObject::TransferMoney)
		.def("give_money",					&CScriptGameObject::GiveMoney)
		.def("money",						&CScriptGameObject::Money)
		.def("actor_money_earned",			&CScriptGameObject::GetActorMoneyEarned)
		.def("actor_money_spent",			&CScriptGameObject::GetActorMoneySpent)
		.def("actor_distance_km",			&CScriptGameObject::GetActorDistanceKm)
		.def("actor_headshots",				&CScriptGameObject::GetActorHeadshots)
		.def("actor_deaths",				&CScriptGameObject::GetActorDeaths)
		.def("actor_help_wounded",			&CScriptGameObject::GetActorHelpWounded)
		.def("make_item_active",			&CScriptGameObject::MakeItemActive)

		.def("switch_to_trade",				&CScriptGameObject::SwitchToTrade)
		.def("switch_to_upgrade",			&CScriptGameObject::SwitchToUpgrade)
		.def("switch_to_talk",				&CScriptGameObject::SwitchToTalk)
		.def("run_talk_dialog",				&CScriptGameObject::RunTalkDialog)
		.def("run_talk_dialog", +[](CScriptGameObject* self, CScriptGameObject* pToWho)
			{
				self->RunTalkDialog(pToWho, true);
				pToWho->DisableTrade();
			})
		.def("allow_break_talk_dialog",		&CScriptGameObject::AllowBreakTalkDialog)

		.def("set_pda_disabled",			&CScriptGameObject::SetPdaDisabled)
		.def("is_pda_disabled",				&CScriptGameObject::IsPdaDisabled)
		.def("set_inventory_disabled",		&CScriptGameObject::SetInventoryDisabled)
		.def("is_inventory_disabled",		&CScriptGameObject::IsInventoryDisabled)
		.def("set_use_disabled",			&CScriptGameObject::SetUseDisabled)

		.def("hide_weapon",					&CScriptGameObject::HideWeapon)
		.def("hide_detector",				&CScriptGameObject::HideDetector)
		.def("switch_detector",				&CScriptGameObject::SwitchDetector)
		.def("restore_weapon",				&CScriptGameObject::RestoreWeapon)
		
		.def("weapon_is_grenadelauncher",	&CScriptGameObject::Weapon_IsGrenadeLauncherAttached)
		.def("weapon_is_scope",				&CScriptGameObject::Weapon_IsScopeAttached)
		.def("weapon_is_silencer",			&CScriptGameObject::Weapon_IsSilencerAttached)

		.def("weapon_grenadelauncher_status",	&CScriptGameObject::Weapon_GrenadeLauncher_Status)
		.def("weapon_scope_status",				&CScriptGameObject::Weapon_Scope_Status)
		.def("weapon_silencer_status",			&CScriptGameObject::Weapon_Silencer_Status)

		.def("allow_sprint",				&CScriptGameObject::AllowSprint)

		.def("set_start_dialog",			&CScriptGameObject::SetStartDialog)
		.def("get_start_dialog",			&CScriptGameObject::GetStartDialog)
		.def("restore_default_start_dialog",&CScriptGameObject::RestoreDefaultStartDialog)

		.def("goodwill",					&CScriptGameObject::GetGoodwill)
		.def("set_goodwill",				&CScriptGameObject::SetGoodwill)
		.def("force_set_goodwill",			&CScriptGameObject::ForceSetGoodwill)
		.def("change_goodwill",				&CScriptGameObject::ChangeGoodwill)

		.def("general_goodwill",			&CScriptGameObject::GetAttitude)
		.def("set_relation",				&CScriptGameObject::SetRelation)
		
		.def("community_goodwill",			&CScriptGameObject::GetCommunityGoodwill_obj)
		.def("set_community_goodwill",		&CScriptGameObject::SetCommunityGoodwill_obj)

		.def("sympathy",					&CScriptGameObject::GetSympathy)
		.def("set_sympathy",				&CScriptGameObject::SetSympathy)

		//////////////////////////////////////////////////////////////////////////
		.def("profile_name",				&CScriptGameObject::ProfileName)
		.def("character_name",				&CScriptGameObject::CharacterName)
		.def("character_icon",				&CScriptGameObject::CharacterIcon)
		.def("character_rank",				&CScriptGameObject::CharacterRank)
		.def("set_character_rank",			&CScriptGameObject::SetCharacterRank)
		.def("change_character_rank",		&CScriptGameObject::ChangeCharacterRank)
		.def("character_reputation",		&CScriptGameObject::CharacterReputation)
		.def("set_character_reputation",	&CScriptGameObject::SetCharacterReputation)
		.def("change_character_reputation",	&CScriptGameObject::ChangeCharacterReputation)
		.def("character_community",			&CScriptGameObject::CharacterCommunity)
		.def("set_character_community",		&CScriptGameObject::SetCharacterCommunity)

		.def("get_actor_relation_flags",	&CScriptGameObject::get_actor_relation_flags)
		.def("set_actor_relation_flags",	&CScriptGameObject::set_actor_relation_flags)
		.def("sound_voice_prefix",	&CScriptGameObject::sound_voice_prefix)

		.enum_("ACTOR_RELATIONS")
		[
			value("relation_attack",						int(RELATION_REGISTRY::ATTACK)),
			value("relation_fight_help_monster",			int(RELATION_REGISTRY::FIGHT_HELP_MONSTER)),
			value("relation_fight_help_human",				int(RELATION_REGISTRY::FIGHT_HELP_HUMAN)),
			value("relation_kill",							int(RELATION_REGISTRY::KILL))
		]

		.enum_("CLSIDS")
		[
			value("no_pda_msg",				int(ePdaMsgMax))
		]

		//Boosters
		.def("is_booster_influence", &CScriptGameObject::IsBoosterInfluence)
		.def("get_booster_influence_time", &CScriptGameObject::GetBoosterInfluenceTime)
		.def("apply_booster", &CScriptGameObject::ApplyBooster)
		.def("set_booster_time", &CScriptGameObject::SetBoosterTime)

		//Actor states
		.def("get_movement_state", &CScriptGameObject::GetActorMovementState)
		.def("set_movement_state", &CScriptGameObject::SetActorMovementState)
		.def("fire", &CScriptGameObject::ActorFire)
		.def("set_actor_crouch", &CScriptGameObject::SetActorCrouch)

		//CustomZone
		.def("set_restrictor_type",			&CScriptGameObject::SetRestrictionType) 
		.def("get_restrictor_type",			&CScriptGameObject::GetRestrictionType)
		.def("enable_anomaly",              &CScriptGameObject::EnableAnomaly)
		.def("disable_anomaly",             &CScriptGameObject::DisableAnomaly)
		.def("get_anomaly_power",			&CScriptGameObject::GetAnomalyPower)
		.def("set_anomaly_power",			&CScriptGameObject::SetAnomalyPower)

        .def("get_artefact_health",			&CScriptGameObject::GetArtefactHealthRestoreSpeed)
        .def("get_artefact_radiation",			&CScriptGameObject::GetArtefactRadiationRestoreSpeed)
        .def("get_artefact_satiety",			&CScriptGameObject::GetArtefactSatietyRestoreSpeed)
        .def("get_artefact_thirst",			&CScriptGameObject::GetArtefactThirstRestoreSpeed)
        .def("get_artefact_sleepiness",			&CScriptGameObject::GetArtefactSleepinessRestoreSpeed)
		.def("get_artefact_equipment_durability",	&CScriptGameObject::GetArtefactEquipmentDurabilityModifier)
		.def("get_artefact_inventory_weight",		&CScriptGameObject::GetArtefactInventoryWeightModifier)
        .def("get_artefact_power",			&CScriptGameObject::GetArtefactPowerRestoreSpeed)
        .def("get_artefact_bleeding",			&CScriptGameObject::GetArtefactBleedingRestoreSpeed)        

        .def("set_artefact_health",			&CScriptGameObject::SetArtefactHealthRestoreSpeed)
        .def("set_artefact_radiation",			&CScriptGameObject::SetArtefactRadiationRestoreSpeed)
        .def("set_artefact_satiety",			&CScriptGameObject::SetArtefactSatietyRestoreSpeed)
        .def("set_artefact_thirst",			&CScriptGameObject::SetArtefactThirstRestoreSpeed)
        .def("set_artefact_sleepiness",			&CScriptGameObject::SetArtefactSleepinessRestoreSpeed)
		.def("set_artefact_equipment_durability",	&CScriptGameObject::SetArtefactEquipmentDurabilityModifier)
		.def("set_artefact_inventory_weight",		&CScriptGameObject::SetArtefactInventoryWeightModifier)
        .def("set_artefact_power",			&CScriptGameObject::SetArtefactPowerRestoreSpeed)
        .def("set_artefact_bleeding",			&CScriptGameObject::SetArtefactBleedingRestoreSpeed)
		//HELICOPTER
		.def("get_helicopter",              &CScriptGameObject::get_helicopter)
		.def("get_car",						&CScriptGameObject::get_car)
		.def("get_hanging_lamp",            &CScriptGameObject::get_hanging_lamp)
		.def("get_bone_id",					&CScriptGameObject::get_bone_id)
		.def("get_physics_shell",			&CScriptGameObject::get_physics_shell)
		.def("get_holder_class",			&CScriptGameObject::get_custom_holder)
		.def("get_current_holder",			&CScriptGameObject::get_current_holder)
		//usable object
		.def("set_tip_text",				&CScriptGameObject::SetTipText)
		.def("set_tip_text_default",		&CScriptGameObject::SetTipTextDefault)
		.def("set_nonscript_usable",		&CScriptGameObject::SetNonscriptUsable)

		// Script Zone
		.def("active_zone_contact",			&CScriptGameObject::active_zone_contact)
		.def("inside",						(bool (CScriptGameObject::*)(const Fvector &, float) const)(&CScriptGameObject::inside))
		.def("inside",						(bool (CScriptGameObject::*)(const Fvector &) const)(&CScriptGameObject::inside))
		.def("set_fastcall",				&CScriptGameObject::set_fastcall)
		.def("set_const_force",				&CScriptGameObject::set_const_force)
		.def("info_add",					&CScriptGameObject::info_add)
		.def("info_clear",					&CScriptGameObject::info_clear)

		// inv box
		.def("is_inv_box_empty",			&CScriptGameObject::IsInvBoxEmpty)
		.def("inv_box_closed",				&CScriptGameObject::inv_box_closed)
		.def("inv_box_closed_status",		&CScriptGameObject::inv_box_closed_status)
		.def("inv_box_can_take",			&CScriptGameObject::inv_box_can_take)
		.def("inv_box_can_take_status",		&CScriptGameObject::inv_box_can_take_status)

		// monster jumper
		.def("jump",						&CScriptGameObject::jump)

		.def("make_object_visible_somewhen",&CScriptGameObject::make_object_visible_somewhen)

		.def("buy_condition",				(void (CScriptGameObject::*)(CScriptIniFile*,str_c))(&CScriptGameObject::buy_condition))
		.def("buy_condition",				(void (CScriptGameObject::*)(float,float))(&CScriptGameObject::buy_condition))
		.def("show_condition",				&CScriptGameObject::show_condition)
		.def("sell_condition",				(void (CScriptGameObject::*)(CScriptIniFile*,str_c))(&CScriptGameObject::sell_condition))
		.def("sell_condition",				(void (CScriptGameObject::*)(float,float))(&CScriptGameObject::sell_condition))
		.def("buy_supplies",				&CScriptGameObject::buy_supplies)
		.def("buy_item_condition_factor",	&CScriptGameObject::buy_item_condition_factor)

		.def("sound_prefix",				(str_c (CScriptGameObject::*)() const)(&CScriptGameObject::sound_prefix))
		.def("sound_prefix",				(void (CScriptGameObject::*)(str_c))(&CScriptGameObject::sound_prefix))

		.def("location_on_path",			&CScriptGameObject::location_on_path)
		.def("is_there_items_to_pickup",	&CScriptGameObject::is_there_items_to_pickup)
		.def("is_ladder",					&CScriptGameObject::IsActorLadder)

		.def("wounded",						(bool (CScriptGameObject::*)() const)(&CScriptGameObject::wounded))
		.def("wounded",						(void (CScriptGameObject::*)(bool))(&CScriptGameObject::wounded))

		.def("iterate_inventory",			&CScriptGameObject::IterateInventory)
		.def("iterate_inventory_box",		&CScriptGameObject::IterateInventoryBox)
		.def("mark_item_dropped",			&CScriptGameObject::MarkItemDropped)
		.def("marked_dropped",				&CScriptGameObject::MarkedDropped)
		.def("unload_magazine",				&CScriptGameObject::UnloadMagazine)

		.def("sight_params",				&CScriptGameObject::sight_params)

		.def("movement_enabled",			&CScriptGameObject::enable_movement)
		.def("movement_enabled",			&CScriptGameObject::movement_enabled)

		.def("critically_wounded",			&CScriptGameObject::critically_wounded)
		.def("get_campfire",				&CScriptGameObject::get_campfire)
		.def("get_artefact",				&CScriptGameObject::get_artefact)
		.def("get_physics_object",			&CScriptGameObject::get_physics_object)
		.def("aim_time",					(void (CScriptGameObject::*) (CScriptGameObject*, u32))&CScriptGameObject::aim_time)
		.def("aim_time",					(u32 (CScriptGameObject::*) (CScriptGameObject*))&CScriptGameObject::aim_time)

		.def("special_danger_move",			(void (CScriptGameObject::*) (bool))&CScriptGameObject::special_danger_move)
		.def("special_danger_move",			(bool (CScriptGameObject::*) ())&CScriptGameObject::special_danger_move)

		.def("sniper_update_rate",			(void (CScriptGameObject::*) (bool))&CScriptGameObject::sniper_update_rate)
		.def("sniper_update_rate",			(bool (CScriptGameObject::*) () const)&CScriptGameObject::sniper_update_rate)

		.def("sniper_fire_mode",			(void (CScriptGameObject::*) (bool))&CScriptGameObject::sniper_fire_mode)
		.def("sniper_fire_mode",			(bool (CScriptGameObject::*) () const)&CScriptGameObject::sniper_fire_mode)

		.def("aim_bone_id",					(void (CScriptGameObject::*) (str_c))&CScriptGameObject::aim_bone_id)
		.def("aim_bone_id",					(str_c (CScriptGameObject::*) () const)&CScriptGameObject::aim_bone_id)

		.def("actor_look_at_point",			&CScriptGameObject::ActorLookAtPoint)
		.def("enable_level_changer",		&CScriptGameObject::enable_level_changer)
		.def("is_level_changer_enabled",	&CScriptGameObject::is_level_changer_enabled)

		.def("is_actor_outdoors",			&CScriptGameObject::IsActorOutdoors)

		.def("set_level_changer_invitation",&CScriptGameObject::set_level_changer_invitation)
		.def("start_particles",				&CScriptGameObject::start_particles)
		.def("stop_particles",				&CScriptGameObject::stop_particles)
					//For Car
		.def("attach_vehicle",				&CScriptGameObject::AttachVehicle)
		.def("detach_vehicle",				&CScriptGameObject::DetachVehicle)
		.def("get_attached_vehicle",		&CScriptGameObject::GetAttachedVehicle)
		.def("ray",							&CScriptGameObject::RayPick)
		.def("is_jump",						&CScriptGameObject::ActorIsJump)

		//
		.def("iterate_feel_touch",			&CScriptGameObject::IterateFeelTouch)
		.def("get_weapon_substate",			&CScriptGameObject::GetWeaponSubstate)
		.def("get_ammo_count_for_type",     &CScriptGameObject::GetAmmoCount)
		.def("get_main_weapon_type",		&CScriptGameObject::GetMainWeaponType)
		.def("get_luminocity", 				&CScriptGameObject::GetLuminocity)
		.def("bone_visible", 				&CScriptGameObject::IsBoneVisible)
		.def("set_bone_visible", 			&CScriptGameObject::SetBoneVisible)
		.def("force_set_position", 			&CScriptGameObject::ForceSetPosition)
		.def("set_spatial_type", 			&CScriptGameObject::SetSpatialType)
		.def("get_spatial_type", 			&CScriptGameObject::GetSpatialType)
		.def("remove_danger", 				&CScriptGameObject::RemoveDanger)
		.def("remove_memory_sound_object", 	&CScriptGameObject::RemoveMemorySoundObject)
		.def("remove_memory_visible_object", &CScriptGameObject::RemoveMemoryVisibleObject)
		.def("remove_memory_hit_object", 	&CScriptGameObject::RemoveMemoryHitObject)
		.def("get_weapon_type",				&CScriptGameObject::GetWeaponType)
			
		///////////////////////////////////////////////////////////////////////////////
		// CoC
		.def("weapon_in_grenade_mode", &CScriptGameObject::WeaponInGrenadeMode)
		.def("weapon_set_scope", &CScriptGameObject::Weapon_SetCurrentScope)
		.def("weapon_get_scope", &CScriptGameObject::Weapon_GetCurrentScope)
		.def("phantom_set_enemy", &CScriptGameObject::PhantomSetEnemy)
		.def("cast_GameObject", &CScriptGameObject::cast_GameObject)
		.def("cast_Car", &CScriptGameObject::cast_Car)
		.def("cast_Heli", &CScriptGameObject::cast_Heli)
		.def("cast_HolderCustom", &CScriptGameObject::cast_HolderCustom)
		.def("cast_EntityAlive", &CScriptGameObject::cast_EntityAlive)
		.def("cast_InventoryItem", &CScriptGameObject::cast_InventoryItem)
		.def("cast_InventoryOwner", &CScriptGameObject::cast_InventoryOwner)
		.def("cast_Actor", &CScriptGameObject::cast_Actor)
		.def("cast_Medkit", &CScriptGameObject::cast_Medkit)
		.def("cast_EatableItem", &CScriptGameObject::cast_EatableItem)
		.def("cast_Antirad", &CScriptGameObject::cast_Antirad)
		.def("cast_CustomOutfit", &CScriptGameObject::cast_CustomOutfit)
		.def("cast_Scope", &CScriptGameObject::cast_Scope)
		.def("cast_Silencer", &CScriptGameObject::cast_Silencer)
		.def("cast_GrenadeLauncher", &CScriptGameObject::cast_GrenadeLauncher)
		.def("cast_SpaceRestrictor", &CScriptGameObject::cast_SpaceRestrictor)
		.def("cast_Stalker", &CScriptGameObject::cast_Stalker)
		.def("cast_CustomZone", &CScriptGameObject::cast_CustomZone)
		.def("cast_Monster", &CScriptGameObject::cast_Monster)
		.def("cast_Explosive", &CScriptGameObject::cast_Explosive)
		.def("cast_ScriptZone", &CScriptGameObject::cast_ScriptZone)
		//.def("cast_Projector", &CScriptGameObject::cast_Projector)
		.def("cast_Trader", &CScriptGameObject::cast_Trader)
		.def("cast_HudItem", &CScriptGameObject::cast_HudItem)
		.def("cast_FoodItem", &CScriptGameObject::cast_FoodItem)
		.def("cast_Artefact", &CScriptGameObject::cast_Artefact)
		.def("cast_Ammo", &CScriptGameObject::cast_Ammo)
		//.def("cast_Missile", &CScriptGameObject::cast_Missile)
		.def("cast_PhysicsShellHolder", &CScriptGameObject::cast_PhysicsShellHolder)
		//.def("cast_Grenade", &CScriptGameObject::cast_Grenade)
		.def("cast_BottleItem", &CScriptGameObject::cast_BottleItem)
		.def("cast_Torch", &CScriptGameObject::cast_Torch)
		.def("cast_InventoryBox", &CScriptGameObject::cast_InventoryBox)

		.def("cast_CHelmet", &CScriptGameObject::cast_CHelmet)
		.def("cast_AntigasFilter", &CScriptGameObject::cast_AntigasFilter)

		.def("bones_protection_sect", &CScriptGameObject::bones_protection_sect)	

		// FFx0001 ++
		.def("is_ruck_to_default", &CScriptGameObject::IsDefaultToRuck)
		.def("set_ruck_to_default", &CScriptGameObject::SetDefaultToRuck)
		.def("get_entity_ignored_by_monsters_state", &CScriptGameObject::GetEntityIgnoredByMonstersState)
		.def("set_entity_ignored_by_monsters_state", &CScriptGameObject::SetEntityIgnoredByMonstersState)
		// FFx0001 --
		
		.def("is_on_belt",					&CScriptGameObject::IsOnBelt)
		.def("item_on_belt",				&CScriptGameObject::ItemOnBelt) 
		.def("belt_count",					&CScriptGameObject::BeltSize)  													   
		.def("get_actor_max_weight",		&CScriptGameObject::GetActorMaxWeight)
		.def("set_actor_max_weight",		&CScriptGameObject::SetActorMaxWeight)
		.def("get_actor_max_walk_weight",	&CScriptGameObject::GetActorMaxWalkWeight)
		.def("set_actor_max_walk_weight",	&CScriptGameObject::SetActorMaxWalkWeight)
		.def("get_additional_max_weight",		&CScriptGameObject::GetAdditionalMaxWeight)
		.def("set_additional_max_weight",		&CScriptGameObject::SetAdditionalMaxWeight)
		.def("get_additional_max_walk_weight",	&CScriptGameObject::GetAdditionalMaxWalkWeight)
		.def("set_additional_max_walk_weight",	&CScriptGameObject::SetAdditionalMaxWalkWeight)
		.def("get_total_weight",			&CScriptGameObject::GetTotalWeight)
		.def("weight",						&CScriptGameObject::Weight)   
		.def("get_inventory_volume",		&CScriptGameObject::GetInventoryVolume)
		.def("get_inventory_volume_capacity", &CScriptGameObject::GetInventoryVolumeCapacity)
		.def("get_inventory_volume_overload", &CScriptGameObject::GetInventoryVolumeOverload)
		.def("get_item_volume",				&CScriptGameObject::GetItemVolume)     

		.def("get_actor_jump_speed",		&CScriptGameObject::GetActorJumpSpeed)
		.def("set_actor_jump_speed",		&CScriptGameObject::SetActorJumpSpeed)
		.def("get_actor_sprint_koef",		&CScriptGameObject::GetActorSprintKoef)
		.def("set_actor_sprint_koef",		&CScriptGameObject::SetActorSprintKoef) 
		.def("get_actor_run_coef",		&CScriptGameObject::GetActorRunCoef)
		.def("set_actor_run_coef",		&CScriptGameObject::SetActorRunCoef) 
		.def("get_actor_runback_coef",		&CScriptGameObject::GetActorRunBackCoef)
		.def("set_actor_runback_coef",		&CScriptGameObject::SetActorRunBackCoef)   
		.def("get_actor_power_boost_time", &CScriptGameObject::GetActorPowerBoostTime)

		// FFx0001 (manipulate bones)
		.def("is_world_object_bone_visible", &CScriptGameObject::IsWorldObjectBoneVisible)
		.def("set_world_object_bone_visibility", &CScriptGameObject::SetWorldObjectBoneVisibility)
		.def("is_hud_object_bone_visible", &CScriptGameObject::IsHudObjectBoneVisible)
		.def("set_hud_object_bone_visibility", &CScriptGameObject::SetHudObjectBoneVisibility)

		// FFx0001 (actor params forvard controll)
		.def("set_actor_sleepiness", &CScriptGameObject::SetActorSleepiness)
		.def("set_actor_satiety", &CScriptGameObject::SetActorSatiety)
		.def("set_actor_thirst", &CScriptGameObject::SetActorThirst)
		.def("set_actor_health", &CScriptGameObject::SetActorHealth)
		.def("set_actor_power", &CScriptGameObject::SetActorPower)
		.def("set_actor_radiation", &CScriptGameObject::SetActorRadiation)
		.def("set_actor_psy_health", &CScriptGameObject::SetActorPsyHealth)
		.def("set_actor_morale", &CScriptGameObject::SetActorMorale)

		//For Antigas
		.def("install_antigas_filter", &CScriptGameObject::InstallAntigasFilter)
		.def("uninstall_antigas_filter", &CScriptGameObject::UnInstallAntigasFilter)

		//For Weapons
		.def("weapon_get_ammo_section",		&CScriptGameObject::Weapon_GetAmmoSection)
		.def("weapon_addon_attach",			&CScriptGameObject::Weapon_AddonAttach)
		.def("weapon_addon_detach",			&CScriptGameObject::Weapon_AddonDetach)
		.def("addons_attach",				&CScriptGameObject::AddonsAttacher)
		.def("get_ammo_count_for_type",     &CScriptGameObject::GetAmmoCount)
		.def("get_main_weapon_type",		&CScriptGameObject::GetMainWeaponType)
		.def("get_weapon_type",				&CScriptGameObject::GetWeaponType)
		.def("get_weapon_substate",			&CScriptGameObject::GetWeaponSubstate)

		// For CHudItem
		.def("play_hud_motion",				&CScriptGameObject::PlayHudMotion)
		.def("switch_state",				&CScriptGameObject::SwitchState)
		.def("get_state",					&CScriptGameObject::GetState)
			
		// For EatableItem
		.def("set_remaining_uses",			&CScriptGameObject::SetRemainingUses)
		.def("get_remaining_uses",			&CScriptGameObject::GetRemainingUses)
		.def("get_max_uses",				&CScriptGameObject::GetMaxUses)

		//For Ammo
		.def("ammo_get_count",				&CScriptGameObject::AmmoGetCount)
		.def("ammo_set_count",				&CScriptGameObject::AmmoSetCount)
		.def("ammo_box_size",				&CScriptGameObject::AmmoBoxSize)
		.def("is_ammo",						&CScriptGameObject::IsAmmo)
		// Actor
		.def("set_character_icon", &CScriptGameObject::SetCharacterIcon)
		.def("get_total_telepatic_protection", &CScriptGameObject::GetTotalTelepaticProtection)

		//For Weapon & Outfit
		.def("install_upgrade",				&CScriptGameObject::InstallUpgrade)
		.def("has_upgrade",					&CScriptGameObject::HasUpgrade)
		.def("iterate_installed_upgrades",	&CScriptGameObject::IterateInstalledUpgrades)
        .def("set_health_ex",				&CScriptGameObject::SetHealthEx)

		// 2055
		.def("get_cutscene_visual",			&CScriptGameObject::GetCutsceneVisual)
		.def("set_invulnerable",			&CScriptGameObject::SetInvulnerable)
		.def("set_best_enemy",				&CScriptGameObject::SetBestEnemy)
		.def("set_fire",					&CScriptGameObject::SetFire)
		.def("get_gasmask_status",			&CScriptGameObject::GetGasmaskStatus)
		.def("get_gasmask_condition",		&CScriptGameObject::GetGasmaskCondition)
		.def("set_head_rotate",				&CScriptGameObject::SetHeadRotate)
		.def("set_default_visual",			&CScriptGameObject::SetActorDefaultVisual)
		.def("IsInCar", &CScriptGameObject::IsInCar)
		.def("iterate_activeitems", &IterateActiveItem),

		class_<enum_exporter<GameObject::ECallbackType> >("callback")
			.enum_("callback_types")
			[
				value("trade_start",				int(GameObject::eTradeStart)),
				value("trade_stop",					int(GameObject::eTradeStop)),
				value("trade_sell_buy_item",		int(GameObject::eTradeSellBuyItem)),
				value("trade_perform_operation",	int(GameObject::eTradePerformTradeOperation)),
				value("trader_global_anim_request",	int(GameObject::eTraderGlobalAnimationRequest)),
				value("trader_head_anim_request",	int(GameObject::eTraderHeadAnimationRequest)),
				value("trader_sound_end",			int(GameObject::eTraderSoundEnd)),
				value("zone_enter",					int(GameObject::eZoneEnter)),
				value("zone_exit",					int(GameObject::eZoneExit)),
				value("level_border_exit",			int(GameObject::eExitLevelBorder)),
				value("level_border_enter",			int(GameObject::eEnterLevelBorder)),
				value("death",						int(GameObject::eDeath)),
				value("patrol_path_in_point",		int(GameObject::ePatrolPathInPoint)),
				value("inventory_pda",				int(GameObject::eInventoryPda)),
				value("inventory_info",				int(GameObject::eInventoryInfo)),
				value("article_info",				int(GameObject::eArticleInfo)),
				value("use_object",					int(GameObject::eUseObject)),
				value("hit",						int(GameObject::eHit)),
				value("sound",						int(GameObject::eSound)),
				value("action_removed",				int(GameObject::eActionTypeRemoved)),
				value("action_movement",			int(GameObject::eActionTypeMovement)),
				value("action_watch",				int(GameObject::eActionTypeWatch)),
				value("action_animation",			int(GameObject::eActionTypeAnimation)),
				value("action_sound",				int(GameObject::eActionTypeSound)),
				value("action_particle",			int(GameObject::eActionTypeParticle)),
				value("action_object",				int(GameObject::eActionTypeObject)),
                value("actor_before_death", 		int(GameObject::eActorBeforeDeath)),
				value("hud_animation_end",			int(GameObject::eActorHudAnimationEnd)),
				value("helicopter_on_point",		int(GameObject::eHelicopterOnPoint)),
				value("helicopter_on_hit",			int(GameObject::eHelicopterOnHit)),
				value("helicopter_on_sam_hit",		int(GameObject::eHelicopterOnSamHit)),
				value("on_item_take",				int(GameObject::eOnItemTake)),
				value("on_item_drop",				int(GameObject::eOnItemDrop)),
				value("script_animation",			int(GameObject::eScriptAnimation)),
				value("task_state",					int(GameObject::eTaskStateChange)),
				value("take_item_from_box",			int(GameObject::eInvBoxItemTake)),
				value("weapon_no_ammo",				int(GameObject::eWeaponNoAmmoAvailable)),

				value("on_actor_hit_fall",			int(GameObject::eFallHit)),
				value("on_actor_jump_begin",		int(GameObject::eOnActorJumpBegin)),
				value("on_actor_jump_end",			int(GameObject::eOnActorJumpEnd)),

				//weapon
				value("weapon_fired",				int(GameObject::eOnWeaponFired)),
				value("weapon_jammed",				int(GameObject::eOnWeaponJammed)),
				value("weapon_zoom_in",				int(GameObject::eOnWeaponZoomIn)),
				value("weapon_zoom_out",			int(GameObject::eOnWeaponZoomOut)),
				value("weapon_magazine_empty",		int(GameObject::eOnWeaponMagazineEmpty)),
					
				// inventory
				value("item_to_belt",				int(GameObject::eItemToBelt)),
				value("item_to_slot",				int(GameObject::eItemToSlot)),
				value("item_to_ruck",				int(GameObject::eItemToRuck)),
				value("on_foot_step",				int(GameObject::eOnFootStep)),

				// anomal pseudogigant
				value("shield_on", int(GameObject::eShieldOn)),
				value("shield_off", int(GameObject::eShieldOff)),
				value("jump", int(GameObject::eJump)),

				// dangerous material touch
				value("dangerous_material_touch", int(GameObject::eDangerousMaterialTouch)),
				
				// car
				value("on_attach_vehicle", 			int(GameObject::eAttachVehicle)),
				value("on_detach_vehicle", 			int(GameObject::eDetachVehicle)),
				value("on_use_vehicle", 			int(GameObject::eUseVehicle)),

				value("key_press",					int(GameObject::eKeyPress)),
				value("key_release",				int(GameObject::eKeyRelease)),
				value("key_hold",					int(GameObject::eKeyHold)),
				value("mouse_move",                 int(GameObject::eMouseMove)),
                value("mouse_wheel",                int(GameObject::eMouseWheel)),
                value("on_enemy_selected",          int(GameObject::eOnBestEnemySelected)),
	

				value("map_location_added",			int(GameObject::eMapLocationAdded))
			],

		def("buy_condition",				(void (*)(CScriptIniFile*,const char*))(&::buy_condition)),
		def("buy_condition",				(void (*)(float,float))(&::buy_condition)),
		def("sell_condition",				(void (*)(CScriptIniFile*,const char*))(&::sell_condition)),
		def("sell_condition",				(void (*)(float,float))(&::sell_condition)),
		def("show_condition",				&::show_condition)
	];
}