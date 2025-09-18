////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object.cpp
//	Created 	: 25.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "script_entity_action.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "script_entity.h"
#include "PhysicsShellHolder.h"
#include "helicopter.h"
#include "holder_custom.h"
#include "InventoryOwner.h"
#include "movement_manager.h"
#include "entity_alive.h"
#include "WeaponMagazined.h"
#include "xrMessages.h"
#include "Inventory.h"
//#include "script_ini_file.h"
#include "../Include/xrRender/Kinematics.h"
#include "HangingLamp.h"
#include "patrol_path_manager.h"
#include "ai_object_location.h"
#include "CustomMonster.h"
#include "EntityCondition.h"
#include "space_restrictor.h"
#include "detail_path_manager.h"
#include "level_graph.h"
#include "Actor.h"
#include "actor_memory.h"
#include "visual_memory_manager.h"
#include "smart_cover_object.h"
#include "smart_cover.h"
#include "smart_cover_description.h"
#include "physics_shell_scripted.h"
#include "ai/phantom/phantom.h"

#include "UIGameCustom.h"
#include "ui/UIActorMenu.h"
#include "InventoryBox.h"

class CScriptBinderObject;

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
Fvector	CScriptGameObject::Center()
{
	Fvector c;
	m_game_object->Center(c);
	return	c;
}

BIND_FUNCTION10	(&object(),	CScriptGameObject::Position,			CGameObject,	Position,			Fvector,						Fvector());
BIND_FUNCTION10	(&object(),	CScriptGameObject::Direction,			CGameObject,	Direction,			Fvector,						Fvector());
BIND_FUNCTION10	(&object(),	CScriptGameObject::Mass,		CPhysicsShellHolder,	GetMass,			float,							float(-1));
BIND_FUNCTION10	(&object(),	CScriptGameObject::ID,					CGameObject,	ID,					u16,							u16(-1));
BIND_FUNCTION10	(&object(),	CScriptGameObject::getVisible,			CGameObject,	getVisible,			BOOL,							FALSE);
//BIND_FUNCTION01	(&object(),	CScriptGameObject::setVisible,			CGameObject,	setVisible,			BOOL,							BOOL);
BIND_FUNCTION10	(&object(),	CScriptGameObject::getEnabled,			CGameObject,	getEnabled,			BOOL,							FALSE);
//BIND_FUNCTION01	(&object(),	CScriptGameObject::setEnabled,			CGameObject,	setEnabled,			BOOL,							BOOL);
BIND_FUNCTION10	(&object(),	CScriptGameObject::story_id,			CGameObject,	story_id,			ALife::_STORY_ID,				ALife::_STORY_ID(-1));
BIND_FUNCTION10	(&object(),	CScriptGameObject::DeathTime,			CEntity,		GetLevelDeathTime,	u32,							0);
BIND_FUNCTION10	(&object(),	CScriptGameObject::MaxHealth,			CEntity,		GetMaxHealth,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::Accuracy,			CInventoryOwner,GetWeaponAccuracy,	float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::Team,				CEntity,		g_Team,				int,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::Squad,				CEntity,		g_Squad,			int,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::Group,				CEntity,		g_Group,			int,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetFOV,				CEntityAlive,	ffGetFov,			float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetRange,			CEntityAlive,	ffGetRange,			float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetHealth,			CEntityAlive,	conditions().GetHealth,			float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetPsyHealth,		CEntityAlive,	conditions().GetPsyHealth,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetPower,			CEntityAlive,	conditions().GetPower,			float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetSatiety,			CEntityAlive,	conditions().GetSatiety,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetRadiation,		CEntityAlive,	conditions().GetRadiation,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetBleeding,			CEntityAlive,	conditions().BleedingSpeed,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetMorale,			CEntityAlive,	conditions().GetEntityMorale,	float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetSleepiness,		CEntityAlive,	conditions().GetSleepiness,		float,							-1);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetThirst,			CEntityAlive,	conditions().GetThirst,			float,							-1);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetHealth,			CEntityAlive,	conditions().ChangeHealth,		float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetPsyHealth,		CEntityAlive,	conditions().ChangePsyHealth,	float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetPower,			CEntityAlive,	conditions().ChangePower,		float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::ChangeSatiety,		CEntityAlive,	conditions().ChangeSatiety,		float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetRadiation,		CEntityAlive,	conditions().ChangeRadiation,	float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetBleeding,			CEntityAlive,	conditions().ChangeBleeding,	float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetSleepiness,		CEntityAlive,	conditions().ChangeSleepiness,	float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetThirst,			CEntityAlive,	conditions().ChangeThirst,		float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetCircumspection,	CEntityAlive,	conditions().ChangeCircumspection,float,							float);
BIND_FUNCTION01	(&object(),	CScriptGameObject::SetMorale,			CEntityAlive,	conditions().ChangeEntityMorale,	float,							float);
BIND_FUNCTION02	(&object(),	CScriptGameObject::SetScriptControl,	CScriptEntity,	SetScriptControl,	bool,								LPCSTR,					bool,					shared_str);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetScriptControl,	CScriptEntity,	GetScriptControl,	bool,								false);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetScriptControlName,CScriptEntity,GetScriptControlName,LPCSTR,					"");
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetEnemyStrength,	CScriptEntity,	get_enemy_strength,	int,					0);
BIND_FUNCTION10	(&object(),	CScriptGameObject::GetActionCount,		CScriptEntity,	GetActionCount,		u32,					0);
BIND_FUNCTION10	(&object(),	CScriptGameObject::can_script_capture,	CScriptEntity,	can_script_capture,	bool,					0);

u32	CScriptGameObject::level_vertex_id() const
{
	return object().ai_location().level_vertex_id();
}

u32 CScriptGameObject::game_vertex_id() const
{
	return object().ai_location().game_vertex_id();
}

CScriptIniFile *CScriptGameObject::spawn_ini() const
{
	return (CScriptIniFile*)object().spawn_ini();
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

void CScriptGameObject::ResetActionQueue()
{
	if (CScriptEntity* l_tpScriptMonster = object().cast_script_entity())
	{
		l_tpScriptMonster->ClearActionQueue();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member ResetActionQueue!");
	}
}

CScriptEntityAction* CScriptGameObject::GetCurrentAction() const
{
	if (CScriptEntity* l_tpScriptMonster = object().cast_script_entity())
	{
		if (l_tpScriptMonster->GetCurrentAction())
		{
			return new CScriptEntityAction(l_tpScriptMonster->GetCurrentAction());
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member GetCurrentAction!");
	}

	return 0;
}

void CScriptGameObject::AddAction(const CScriptEntityAction* tpEntityAction, bool bHighPriority)
{
	if (CScriptEntity* l_tpScriptMonster = object().cast_script_entity())
	{
		l_tpScriptMonster->AddAction(tpEntityAction, bHighPriority);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member AddAction!");
	}
}

const CScriptEntityAction* CScriptGameObject::GetActionByIndex(u32 action_index)
{
	if (CScriptEntity* l_tpScriptMonster = object().cast_script_entity())
	{
		return l_tpScriptMonster->GetActionByIndex(action_index);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptEntity : cannot access class member GetActionByIndex!");
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

u16 CScriptGameObject::get_bone_id(LPCSTR bone_name) const
{
	return object().Visual()->dcast_PKinematics()->LL_BoneID(bone_name);
}

cphysics_shell_scripted* CScriptGameObject::get_physics_shell() const
{
	if (CPhysicsShellHolder* ph_shell_holder = object().cast_physics_shell_holder())
	{
		if (CPhysicsShell* ph_shell = ph_shell_holder->PPhysicsShell())
		{
			return get_script_wrapper<cphysics_shell_scripted>(*ph_shell);
		}
	}

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

CHelicopter* CScriptGameObject::get_helicopter()
{
	if (CHelicopter* helicopter = object().cast_helicopter())
	{
		return helicopter;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member get_helicopter!");
	return 0;
}

CHangingLamp* CScriptGameObject::get_hanging_lamp()
{
	if (CHangingLamp* lamp = object().cast_hanging_lamp())
	{
		return lamp;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : it is not a lamp!");
	return 0;
}

CHolderCustom* CScriptGameObject::get_custom_holder()
{
	if (CHolderCustom* holder = object().cast_holder_custom())
	{
		return holder;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : it is not a holder!");
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

LPCSTR CScriptGameObject::WhoHitName()
{
	if (CEntityAlive* entity_alive = object().cast_entity_alive())
	{
		return entity_alive->conditions().GetWhoHitLastTime() ? *entity_alive->conditions().GetWhoHitLastTime()->cName() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member WhoHitName()");
	return 0;
}

LPCSTR CScriptGameObject::WhoHitSectionName()
{
	if (CEntityAlive* entity_alive = object().cast_entity_alive())
	{
		return entity_alive->conditions().GetWhoHitLastTime() ? *entity_alive->conditions().GetWhoHitLastTime()->cNameSect() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member WhoHitName()");
	return 0;
}

bool CScriptGameObject::CheckObjectVisibility(const CScriptGameObject* tpLuaGameObject)
{
	if (CEntityAlive* entity_alive = object().cast_entity_alive())
	{
		if (!entity_alive->g_Alive())
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot check visibility of dead object!");
			return false;
		}
	}

	if (CScriptEntity* script_entity = object().cast_script_entity())
	{
		return tpLuaGameObject ? script_entity->CheckObjectVisibility(&tpLuaGameObject->object()) : false;
	}
	else if (CActor* actor = object().cast_actor())
	{
		return tpLuaGameObject ? actor->memory().visual().visible_now(&tpLuaGameObject->object()) : false;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : cannot access class member CheckObjectVisibility!");
	return false;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

CScriptBinderObject* CScriptGameObject::binded_object()
{
	return object().object();
}

void CScriptGameObject::bind_object(CScriptBinderObject* game_object)
{
	object().set_object(game_object);
}

void CScriptGameObject::set_previous_point(int point_index)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().patrol().set_previous_point(point_index);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member set_previous_point!");
	}
}

void CScriptGameObject::set_start_point(int point_index)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().patrol().set_start_point(point_index);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member set_start_point!");
	}
}

u32 CScriptGameObject::get_current_patrol_point_index()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->movement().patrol().get_current_point_index();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot call [get_current_patrol_point_index()]!");
	return (u32(-1));
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

Fvector	CScriptGameObject::bone_position(LPCSTR bone_name) const
{
	u16 bone_id = BI_NONE;

	IKinematics* kin = PKinematics(object().Visual());

	if (xr_strlen(bone_name))
	{
		bone_id = kin->LL_BoneID(bone_name);
	}
	else
	{
		bone_id = kin->LL_GetBoneRoot();
	}

	Fmatrix matrix;
	matrix.mul_43(object().XFORM(), kin->LL_GetBoneInstance(bone_id).mTransform);
	return matrix.c;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

u32 CScriptGameObject::GetAmmoElapsed()
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->GetAmmoElapsed();
	}

	return 0;
}

void CScriptGameObject::SetAmmoElapsed(int ammo_elapsed)
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		weapon->SetAmmoElapsed(ammo_elapsed);
	}
}

//Alundaio

void CScriptGameObject::SetAmmoType(u8 type)
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		weapon->SetAmmoType(type);
	}
}

u8 CScriptGameObject::GetAmmoType()
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->GetAmmoType();
	}

	return 255;
}

void CScriptGameObject::SetMainWeaponType(u32 type)
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		weapon->set_ef_main_weapon_type(type);
	}
}

void CScriptGameObject::SetWeaponType(u32 type)
{
	if (CWeapon* weapon = object().cast_weapon())
	{
		weapon->set_ef_weapon_type(type);
	}
}

u32 CScriptGameObject::GetMainWeaponType()
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->ef_main_weapon_type();
	}

	return 255;
}

u32 CScriptGameObject::GetWeaponType()
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->ef_weapon_type();
	}

	return 255;
}

bool CScriptGameObject::HasAmmoType(u8 type)
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return type < weapon->m_ammoTypes.size();
	}

	return false;
}

u8 CScriptGameObject::GetWeaponSubstate()
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->m_sub_state;
	}

	return 255;
}
//-Alundaio

u32 CScriptGameObject::GetSuitableAmmoTotal() const
{
	if (const CWeapon* weapon = object().cast_weapon())
	{
		return weapon->GetSuitableAmmoTotal(true);
	}

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

void CScriptGameObject::SetQueueSize(u32 queue_size)
{
	if (CWeaponMagazined* weapon = object().cast_weapon_magazined())
	{
		weapon->SetQueueSize(queue_size);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CWeaponMagazined : cannot access class member SetQueueSize!");
	}
}

////////////////////////////////////////////////////////////////////////////
//// Inventory Owner
////////////////////////////////////////////////////////////////////////////

u32	CScriptGameObject::Cost() const
{
	if (const CInventoryItem* inventory_item = object().cast_inventory_item())
	{
		return inventory_item->Cost();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member Cost!");
	return false;
}

float CScriptGameObject::GetCondition() const
{
	if (const CInventoryItem* inventory_item = object().cast_inventory_item())
	{
		return inventory_item->GetCondition();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member GetCondition!");
	return false;
}

void CScriptGameObject::SetCondition(float val)
{
	if (CInventoryItem* inventory_item = object().cast_inventory_item())
	{
		val -= inventory_item->GetCondition();
		inventory_item->ChangeCondition(val);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member SetCondition!");
	}
}

void CScriptGameObject::eat(CScriptGameObject* item)
{
	if (CInventoryItem* inventory_item = item != nullptr ? item->object().cast_inventory_item() : nullptr)
	{
		if (CInventoryOwner* inventory_owner = object().cast_inventory_owner())
		{
			inventory_owner->inventory().Eat(inventory_item);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member eat!");
	}
}

bool CScriptGameObject::inside(const Fvector& position, float epsilon) const
{
	if (CSpaceRestrictor* space_restrictor = object().cast_restrictor())
	{
		Fsphere	sphere;
		sphere.P = position;
		sphere.R = epsilon;
		return space_restrictor->inside(sphere);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSpaceRestrictor : cannot access class member inside!");
	return false;
}

bool CScriptGameObject::inside(const Fvector& position) const
{
	return inside(position, EPS_L);
}

void CScriptGameObject::set_patrol_extrapolate_callback(const luabind::functor<bool>& functor)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().patrol().extrapolate_callback().set(functor);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_patrol_extrapolate_callback!");
	}
}

void CScriptGameObject::set_patrol_extrapolate_callback(const luabind::functor<bool>& functor, const luabind::object& object)
{
	if (CCustomMonster* monster = this->object().cast_custom_monster())
	{
		monster->movement().patrol().extrapolate_callback().set(functor, object);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_patrol_extrapolate_callback!");
	}
}

void CScriptGameObject::set_patrol_extrapolate_callback()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().patrol().extrapolate_callback().clear();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_patrol_extrapolate_callback!");
	}
}

void CScriptGameObject::extrapolate_length(float extrapolate_length)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->movement().detail().extrapolate_length(extrapolate_length);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member extrapolate_length!");
	}
}

float CScriptGameObject::extrapolate_length() const
{
	if (const CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->movement().detail().extrapolate_length();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member extrapolate_length!");
	return 0.0f;
}

void CScriptGameObject::set_fov(float new_fov)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->set_fov(new_fov);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_fov!");
	}
}

void CScriptGameObject::set_range(float new_range)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->set_range(new_range);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_range!");
	}
}

u32	CScriptGameObject::vertex_in_direction(u32 level_vertex_id, Fvector direction, float max_distance) const
{
	CCustomMonster* monster = object().cast_custom_monster();
	if (monster == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member vertex_in_direction!");
		return (u32(-1));
	}

	if (!monster->movement().restrictions().accessible(level_vertex_id))
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster::vertex_in_direction - start vertex id is not accessible!");
		return (u32(-1));
	}

	direction.normalize_safe();
	direction.mul(max_distance);

	Fvector start_position = ai().level_graph().vertex_position(level_vertex_id);
	Fvector finish_position = Fvector(start_position).add(direction);

	u32	result = u32(-1);

	monster->movement().restrictions().add_border(level_vertex_id, max_distance);
	ai().level_graph().farthest_vertex_in_direction(level_vertex_id, start_position, finish_position, result, 0, true);
	monster->movement().restrictions().remove_border();

	return ai().level_graph().valid_vertex_id(result) ? result : level_vertex_id;
}

bool CScriptGameObject::invulnerable() const
{
	if (const CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->invulnerable();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member invulnerable!");
	return false;
}

void CScriptGameObject::invulnerable(bool invulnerable)
{
	if (CActor* pActor = object().cast_actor())
	{
		psActorFlags.set(AF_GODMODE, invulnerable);
	}
	else if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->invulnerable(invulnerable);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member invulnerable!");
	}
}

LPCSTR CScriptGameObject::get_smart_cover_description() const
{
	if (smart_cover::object* smart_cover_object = smart_cast<smart_cover::object*>(&object()))
	{
		return *smart_cover_object->cover().description()->table_id();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "smart_cover::object : cannot access class member get_smart_cover_description!");
	return 0;
}

CGameObject& CScriptGameObject::object() const
{
#ifdef DEBUG
	__try
	{
		if (m_game_object && m_game_object->lua_game_object() == this)
		{
			return *m_game_object;
		}
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
	}

	ai().script_engine().script_log(eLuaMessageTypeError, "you are trying to use a destroyed object [%x]", m_game_object);
	THROW2(m_game_object && m_game_object->lua_game_object() == this, "Probably, you are trying to use a destroyed object!");

#endif // #ifdef DEBUG

	return *m_game_object;
}

bool CScriptGameObject::IsActorOutdoors() const
{
	// Check to make sure all the params are available (we're in game and such).
	if (!g_pGameLevel)
	{
		return FALSE;
	}
	CObject* e = g_pGameLevel->CurrentViewEntity();
	if (!e || !e->renderable_ROS())
	{
		return FALSE;
	}
	// Now do the real check! This is a copy out of another section of code that is also hard coded.
	// I don't know what the proper limit for this is supposed to be, but this seems good enough.
	return e->renderable_ROS()->get_luminocity_hemi() > 0.05f;
}

void CScriptGameObject::StartTrade(CScriptGameObject* obj)
{
	if (CActor* actor = obj->object().cast_actor())
	{
		CUIActorMenu& ActorMenu = CurrentGameUI()->ActorMenu();

		ActorMenu.SetActor(actor->cast_inventory_owner());
		ActorMenu.SetPartner(object().cast_inventory_owner());

		ActorMenu.SetMenuMode(mmTrade);
		ActorMenu.ShowDialog(true);
	}
}

void CScriptGameObject::StartUpgrade(CScriptGameObject* obj)
{
	if (CActor* actor = obj->object().cast_actor())
	{
		CUIActorMenu& ActorMenu = CurrentGameUI()->ActorMenu();

		ActorMenu.SetActor(actor->cast_inventory_owner());
		ActorMenu.SetPartner(object().cast_inventory_owner());

		ActorMenu.SetMenuMode(mmUpgrade);
		ActorMenu.ShowDialog(true);
	}
}

void CScriptGameObject::PhantomSetEnemy(CScriptGameObject* enemy)
{
	if (CPhantom* phant = smart_cast<CPhantom*>(&object()))
	{
		phant->SetEnemy(&enemy->object());
	}
}

//Allows to force use an object if passed obj is the actor
bool CScriptGameObject::Use(CScriptGameObject* obj)
{
	bool ret = object().use(&obj->object());

	CActor* actor = obj->object().cast_actor();
	if (actor == nullptr)
	{
		return ret;
	}

	CInventoryOwner* pActorInv = actor->cast_inventory_owner();
	if (pActorInv == nullptr)
	{
		return ret;
	}

	CUIActorMenu& ActorMenu = CurrentGameUI()->ActorMenu();

	if (CInventoryBox* pBox = object().cast_inventory_box())
	{
		ActorMenu.SetActor(pActorInv);
		ActorMenu.SetInvBox(pBox);

		ActorMenu.SetMenuMode(mmDeadBodySearch);
		ActorMenu.ShowDialog(true);

		return true;
	}
	else
	{
		CInventoryOwner* pOtherOwner = object().cast_inventory_owner();
		if (pOtherOwner == nullptr)
		{
			return ret;
		}

		ActorMenu.SetActor(pActorInv);
		ActorMenu.SetPartner(pOtherOwner);

		ActorMenu.SetMenuMode(mmDeadBodySearch);
		ActorMenu.ShowDialog(true);

		return true;
	}

	return false;
}