////////////////////////////////////////////////////////////////////////////
// script_game_object_trader.сpp :	функции для торговли и торговцев
//////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"

#include "script_zone.h"
#include "ai/trader/ai_trader.h"

#include "ai_space.h"
#include "alife_simulator.h"

#include "ai/stalker/ai_stalker.h"
#include "stalker_movement_manager_smart_cover.h"

#include "sight_manager_space.h"
#include "sight_control_action.h"
#include "sight_manager.h"
#include "InventoryBox.h"
#include "ZoneCampfire.h"
#include "PhysicObject.h"
#include "Artefact.h"
#include "stalker_sound_data.h"
#include "Torch.h"
#include "WeaponMagazinedWGrenade.h"
#include "helicopter.h"
#include "Car.h"
#include "level_changer.h"

class CWeapon;

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

bool CScriptGameObject::is_body_turning() const
{
	CCustomMonster* monster = object().cast_custom_monster();
	if (monster == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member is_turning!");
		return false;
	}

	if (CAI_Stalker* stalker = monster->cast_stalker())
	{
		return !fis_zero(angle_difference(stalker->movement().head_orientation().target.yaw, stalker->movement().head_orientation().current.yaw)) || !fis_zero(angle_difference(monster->movement().body_orientation().target.yaw, monster->movement().body_orientation().current.yaw));
	}

	return !fis_zero(angle_difference(monster->movement().body_orientation().target.yaw,monster->movement().body_orientation().current.yaw));
}

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

u32	CScriptGameObject::add_sound(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, LPCSTR bone_name)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->sound().add(prefix, max_count, type, priority, mask, internal_type, bone_name);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member add!");
	return 0;
}

u32	CScriptGameObject::add_combat_sound(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, LPCSTR bone_name)
{
	if (CAI_Stalker* const stalker = object().cast_stalker())
	{
		return stalker->sound().add(prefix, max_count, type, priority, mask, internal_type, bone_name, new CStalkerSoundData(stalker));
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member add!");
	return 0;
}

u32	CScriptGameObject::add_sound(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type)
{
	return add_sound(prefix,max_count,type,priority,mask,internal_type,"bip01_head");
}

void CScriptGameObject::remove_sound(u32 internal_type)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().remove(internal_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member add!");
	}
}

void CScriptGameObject::set_sound_mask(u32 sound_mask)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		if (CEntityAlive* entity_alive = monster->cast_entity_alive())
		{
			VERIFY2(entity_alive->g_Alive(), "Stalker try talk after death!");
		}

		monster->sound().set_sound_mask(sound_mask);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member set_sound_mask!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type, u32 max_start_time)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type, max_start_time);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type, u32 max_start_time, u32 min_start_time)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type, max_start_time, min_start_time);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type, max_start_time, min_start_time, max_stop_time);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time, u32 min_stop_time)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type, max_start_time, min_start_time, max_stop_time, min_stop_time);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

void CScriptGameObject::play_sound(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time, u32 min_stop_time, u32 id)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->sound().play(internal_type, max_start_time, min_start_time, max_stop_time, min_stop_time, id);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundPlayer : cannot access class member play!");
	}
}

int CScriptGameObject::active_sound_count(bool only_playing)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->sound().active_sound_count(only_playing);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member active_sound_count!");
	return -1;
}

int CScriptGameObject::active_sound_count()
{
	return active_sound_count(false);
}

bool CScriptGameObject::wounded() const
{
	if (const CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->wounded();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member wounded!");
	return false;
}

void CScriptGameObject::wounded(bool value)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->wounded(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member wounded!");
	}
}

CSightParams CScriptGameObject::sight_params()
{
	CAI_Stalker* stalker = object().cast_stalker();
	if (stalker == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member sight_params!");

		CSightParams result;
		result.m_object = 0;
		result.m_vector = Fvector().set(flt_max,flt_max,flt_max);
		result.m_sight_type = SightManager::eSightTypeDummy;
		return result;
	}

	const CSightControlAction& action = stalker->sight().current_action();
	CSightParams result;
	result.m_sight_type = action.sight_type();
	result.m_object = action.object_to_look() ? action.object_to_look()->lua_game_object() : 0;
	result.m_vector = action.vector3d();
	return result;
}

bool CScriptGameObject::critically_wounded()
{
	if (CCustomMonster* custom_monster = object().cast_custom_monster())
	{
		return custom_monster->critically_wounded();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member critically_wounded!");
	return false;
}

bool CScriptGameObject::IsInvBoxEmpty()
{
	if (CInventoryBox* ib = object().cast_inventory_box())
	{
		return ib->IsEmpty();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryBox : cannot access class member IsEmpty!");
	return false;
}

bool CScriptGameObject::inv_box_closed(bool status, LPCSTR reason)
{
	if (CInventoryBox* ib = object().cast_inventory_box())
	{
		ib->set_closed(status, reason);
		return true;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryBox : cannot access class member set_closed!");
	return false;
}

bool CScriptGameObject::inv_box_closed_status()
{
	if (CInventoryBox* ib = object().cast_inventory_box())
	{
		return ib->closed();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryBox : cannot access class member closed!");
	return false;
}

bool CScriptGameObject::inv_box_can_take(bool status)
{
	if (CInventoryBox* ib = object().cast_inventory_box())
	{
		ib->set_can_take(status);
		return true;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryBox : cannot access class member set_can_take!");
	return false;
}

bool CScriptGameObject::inv_box_can_take_status()
{
	if (CInventoryBox* ib = object().cast_inventory_box())
	{
		return ib->can_take();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CInventoryBox : cannot access class member can_take!");
	return false;
}

CZoneCampfire* CScriptGameObject::get_campfire()
{
	return smart_cast<CZoneCampfire*>(&object());
}

CArtefact* CScriptGameObject::get_artefact()
{
	return object().cast_artefact();
}

CPhysicObject* CScriptGameObject::get_physics_object()
{
	return smart_cast<CPhysicObject*>(&object());
}

void CScriptGameObject::enable_level_changer(bool b)
{
	if (CLevelChanger* lch = object().cast_level_changer())
	{
		lch->EnableLevelChanger(b);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CLevelChanger : cannot access class member EnableLevelChanger!");
	}
}

bool CScriptGameObject::is_level_changer_enabled()
{
	if (CLevelChanger* lch = object().cast_level_changer())
	{
		return lch->IsLevelChangerEnabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CLevelChanger : cannot access class member IsLevelChangerEnabled!");
	return false;
}

void CScriptGameObject::set_level_changer_invitation(LPCSTR str)
{
	if (CLevelChanger* lch = object().cast_level_changer())
	{
		lch->SetLEvelChangerInvitationStr(str);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CLevelChanger : cannot access class member SetLEvelChangerInvitationStr!");
	}
}

void CScriptGameObject::start_particles(LPCSTR pname, LPCSTR bone)
{
	CParticlesPlayer* PP = object().cast_particles_player();
	if (PP == nullptr)
	{
		return;
	}

	IKinematics* K = PKinematics(object().Visual());
	R_ASSERT(K);

	u16 play_bone = K->LL_BoneID(bone);
	R_ASSERT(play_bone != BI_NONE);

	if (K->LL_GetBoneVisible(play_bone))
	{
		PP->StartParticles(pname, play_bone, Fvector().set(0, 1, 0), 9999);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cant start particles, bone [%s] is not visible now", bone);
	}
}

void CScriptGameObject::stop_particles(LPCSTR pname, LPCSTR bone)
{
	CParticlesPlayer* PP = object().cast_particles_player();
	if (PP == nullptr)
	{
		return;
	}

	IKinematics* K = PKinematics(object().Visual());
	R_ASSERT(K);

	u16 play_bone = K->LL_BoneID(bone);
	R_ASSERT(play_bone != BI_NONE);

	if (K->LL_GetBoneVisible(play_bone))
	{
		PP->StopParticles(9999, play_bone, true);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cant stop particles, bone [%s] is not visible now", bone);
	}
}

// Directly set entity health instead of going throuhg normal health property which operates on delta
void CScriptGameObject::SetHealthEx(float hp)
{
	if (CEntity* obj = object().cast_entity())
	{
		clamp(hp, -0.01f, 1.0f);
		obj->SetfHealth(hp);
	}
}

#define SPECIFIC_CAST(A,B)\
B* A ()\
{\
    B				*l_tpEntity = smart_cast<B*>(&object());\
    if (!l_tpEntity)\
        return (0);\
                else\
        return l_tpEntity;\
};\

SPECIFIC_CAST(CScriptGameObject::cast_GameObject, CScriptGameObject);
SPECIFIC_CAST(CScriptGameObject::cast_Car, CCar);
SPECIFIC_CAST(CScriptGameObject::cast_Heli, CHelicopter);
SPECIFIC_CAST(CScriptGameObject::cast_HolderCustom, CHolderCustom);
SPECIFIC_CAST(CScriptGameObject::cast_EntityAlive, CEntityAlive);
SPECIFIC_CAST(CScriptGameObject::cast_InventoryItem, CInventoryItem);
SPECIFIC_CAST(CScriptGameObject::cast_InventoryOwner, CInventoryOwner);
SPECIFIC_CAST(CScriptGameObject::cast_Actor, CActor);

CMedkit* CScriptGameObject::cast_Medkit()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? smart_cast<CMedkit*>(ii) : 0;
}

CEatableItem* CScriptGameObject::cast_EatableItem()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? ii->cast_eatable_item() : 0;
}

CAntirad* CScriptGameObject::cast_Antirad()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? smart_cast<CAntirad*>(ii) : 0;
}

SPECIFIC_CAST(CScriptGameObject::cast_CustomOutfit, CCustomOutfit);
SPECIFIC_CAST(CScriptGameObject::cast_Scope, CScope);
SPECIFIC_CAST(CScriptGameObject::cast_Silencer, CSilencer);
SPECIFIC_CAST(CScriptGameObject::cast_GrenadeLauncher, CGrenadeLauncher);
SPECIFIC_CAST(CScriptGameObject::cast_SpaceRestrictor, CSpaceRestrictor);
SPECIFIC_CAST(CScriptGameObject::cast_Stalker, CAI_Stalker);
SPECIFIC_CAST(CScriptGameObject::cast_CustomZone, CCustomZone);
SPECIFIC_CAST(CScriptGameObject::cast_Monster, CCustomMonster);
SPECIFIC_CAST(CScriptGameObject::cast_Explosive, CExplosive);
SPECIFIC_CAST(CScriptGameObject::cast_ScriptZone, CScriptZone);
//SPECIFIC_CAST(CScriptGameObject::cast_Projector, CProjector);
SPECIFIC_CAST(CScriptGameObject::cast_Trader, CAI_Trader);

CHudItem* CScriptGameObject::cast_HudItem()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? ii->cast_hud_item() : 0;
}

CFoodItem* CScriptGameObject::cast_FoodItem()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? ii->cast_food_item() : 0;
}

SPECIFIC_CAST(CScriptGameObject::cast_Artefact, CArtefact);
SPECIFIC_CAST(CScriptGameObject::cast_Ammo, CWeaponAmmo);
//SPECIFIC_CAST(CScriptGameObject::cast_Missile, CMissile);
SPECIFIC_CAST(CScriptGameObject::cast_PhysicsShellHolder, CPhysicsShellHolder);
//SPECIFIC_CAST(CScriptGameObject::cast_Grenade, CGrenade);

CBottleItem* CScriptGameObject::cast_BottleItem()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? smart_cast<CBottleItem*>(ii) : (0);
}

CTorch* CScriptGameObject::cast_Torch()
{
	CInventoryItem* ii = object().cast_inventory_item();
	return ii != nullptr ? ii->cast_torch() : 0;
}

SPECIFIC_CAST(CScriptGameObject::cast_InventoryBox, CInventoryBox);
