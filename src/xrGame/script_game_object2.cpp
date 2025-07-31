////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object_script2.cpp
//	Created 	: 17.11.2004
//  Modified 	: 17.11.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "Explosive.h"
#include "script_zone.h"
#include "script_hit.h"
#include "../Include/xrRender/Kinematics.h"
#include "PDA.h"
#include "InfoPortion.h"
#include "memory_manager.h"
#include "AI_PhraseDialogManager.h"
#include "xrMessages.h"
#include "CustomMonster.h"
#include "memory_manager.h"
#include "visual_memory_manager.h"
#include "sound_memory_manager.h"
#include "hit_memory_manager.h"
#include "enemy_manager.h"
#include "item_manager.h"
#include "danger_manager.h"
#include "memory_space.h"
#include "Actor.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrEngine/CameraBase.h"
#include "ai/stalker/ai_stalker.h"
#include "Car.h"
#include "movement_manager.h"
#include "detail_path_manager.h"
#include "CharacterPhysicsSupport.h"
#include "stalker_animation_manager.h"

void CScriptGameObject::explode(u32 level_time)
{
	if (object().H_Parent())
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CExplosive : cannot explode object wiht parent!");
		return;
	}

	if (CExplosive* explosive = object().cast_explosive())
	{
		Fvector normal;
		explosive->FindNormal(normal);
		explosive->SetInitiator(object().ID());
		explosive->GenExplodeEvent(object().Position(), normal);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CExplosive : cannot access class member explode!");
	}
}

bool CScriptGameObject::active_zone_contact(u16 id)
{
	if (CScriptZone* script_zone = object().cast_script_zone())
	{
		return script_zone->active_contact(id);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptZone : cannot access class member active_zone_contact!");
	return false;
}

CScriptGameObject* CScriptGameObject::best_weapon()
{
	if (CObjectHandler* object_handler = smart_cast<CAI_Stalker*>(&object()))
	{
		CGameObject* game_object = object_handler->best_weapon() != nullptr ? &object_handler->best_weapon()->object() : 0;
		return game_object != nullptr ? game_object->lua_game_object() : 0;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptEntity : cannot access class member best_weapon!");
	return 0;
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action)
{
	if (CObjectHandler* object_handler = smart_cast<CAI_Stalker*>(&object()))
	{
		object_handler->set_goal(object_action);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CObjectHandler : cannot access class member set_item!");
	}
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action, CScriptGameObject* lua_game_object)
{
	if (CObjectHandler* object_handler = smart_cast<CAI_Stalker*>(&object()))
	{
		object_handler->set_goal(object_action, lua_game_object ? &lua_game_object->object() : 0);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CObjectHandler : cannot access class member set_item!");
	}
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action, CScriptGameObject* lua_game_object, u32 queue_size)
{
	if (CObjectHandler* object_handler = smart_cast<CAI_Stalker*>(&object()))
	{
		object_handler->set_goal(object_action, lua_game_object ? &lua_game_object->object() : 0, queue_size, queue_size);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CObjectHandler : cannot access class member set_item!");
	}
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action, CScriptGameObject* lua_game_object, u32 queue_size, u32 queue_interval)
{
	if (CObjectHandler* object_handler = smart_cast<CAI_Stalker*>(&object()))
	{
		object_handler->set_goal(object_action, lua_game_object ? &lua_game_object->object() : 0, queue_size, queue_size, queue_interval, queue_interval);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CObjectHandler : cannot access class member set_item!");
	}
}

void CScriptGameObject::play_cycle(LPCSTR anim, bool mix_in)
{
	if (IKinematicsAnimated* sa = object().Visual()->dcast_PKinematicsAnimated())
	{
		if (MotionID m = sa->ID_Cycle(anim))
		{
			sa->PlayCycle(m, (BOOL)mix_in);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : has not cycle %s", anim);
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : is not animated object");
	}
}

void CScriptGameObject::play_cycle(LPCSTR anim)
{
	play_cycle(anim, true);
}

void CScriptGameObject::Hit(CScriptHit* tpLuaHit)
{
	CScriptHit& tLuaHit = *tpLuaHit;
	NET_Packet P;
	SHit HS;

	HS.GenHeader(GE_HIT, object().ID());										//	object().u_EventGen(P,GE_HIT,object().ID());
	THROW2(tLuaHit.m_tpDraftsman, "Where is hit initiator??!");	//	THROW2			(tLuaHit.m_tpDraftsman,"Where is hit initiator??!");
	HS.whoID = u16(tLuaHit.m_tpDraftsman->ID());							//	P.w_u16			(u16(tLuaHit.m_tpDraftsman->ID()));
	HS.weaponID = 0;														//	P.w_u16			(0);
	HS.dir = tLuaHit.m_tDirection;											//	P.w_dir			(tLuaHit.m_tDirection);
	HS.power = tLuaHit.m_fPower;											//	P.w_float		(tLuaHit.m_fPower);
	IKinematics* V = PKinematics(object().Visual());		//	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());
	VERIFY(V);													//	VERIFY			(V);

	if (xr_strlen(tLuaHit.m_caBoneName))									//	if (xr_strlen	(tLuaHit.m_caBoneName))
	{
		HS.boneID = (V->LL_BoneID(tLuaHit.m_caBoneName));			//		P.w_s16		(V->LL_BoneID(tLuaHit.m_caBoneName));
	}
	else																	//	else
	{
		HS.boneID = (s16(0));										//		P.w_s16		(s16(0));
	}

	HS.p_in_bone_space = Fvector().set(0, 0, 0);								//	P.w_vec3		(Fvector().set(0,0,0));
	HS.impulse = tLuaHit.m_fImpulse;										//	P.w_float		(tLuaHit.m_fImpulse);
	HS.hit_type = (ALife::EHitType)(tLuaHit.m_tHitType);					//	P.w_u16			(u16(tLuaHit.m_tHitType));
	HS.Write_Packet(P);

	object().u_EventSend(P);
}

#pragma todo("Dima to Dima : find out why user defined conversion operators work incorrect")

CScriptGameObject::operator CObject*()
{
	return &object();
}

CScriptGameObject* CScriptGameObject::GetBestEnemy()
{
	if (const CCustomMonster* monster = object().cast_custom_monster())
	{
		if (monster->memory().enemy().selected())
		{
			return monster->memory().enemy().selected()->lua_game_object();
		}
	}

	return 0;
}

const CDangerObject* CScriptGameObject::GetBestDanger()
{
	if (const CCustomMonster* monster = object().cast_custom_monster())
	{
		if (!monster->memory().danger().selected())
		{
			return monster->memory().danger().selected();
		}
	}

	return 0;
}

CScriptGameObject* CScriptGameObject::GetBestItem()
{
	if (const CCustomMonster* monster = object().cast_custom_monster())
	{
		if (monster->memory().item().selected())
		{
			return monster->memory().item().selected()->lua_game_object();
		}
	}

	return 0;
}

u32 CScriptGameObject::memory_time(const CScriptGameObject& lua_game_object)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().memory_time(&lua_game_object.object());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptEntity : cannot access class member memory!");
	return 0;
}

Fvector CScriptGameObject::memory_position(const CScriptGameObject& lua_game_object)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().memory_position(&lua_game_object.object());
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptEntity : cannot access class member memory!");
	return zero_vel;
}

void CScriptGameObject::enable_memory_object(CScriptGameObject* game_object, bool enable)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().enable(&game_object->object(), enable);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member enable_memory_object!");
	}
}

const xr_vector<CNotYetVisibleObject>& CScriptGameObject::not_yet_visible_objects() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().visual().not_yet_visible_objects();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member not_yet_visible_objects!");
	NODEFAULT;

	return {};
}

float CScriptGameObject::visibility_threshold() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().visual().visibility_threshold();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member visibility_threshold!");
	return 0.0f;
}

void CScriptGameObject::enable_vision(bool value)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().visual().enable(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CVisualMemoryManager : cannot access class member enable_vision!");
	}
}

bool CScriptGameObject::vision_enabled() const
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		return monster->memory().visual().enabled();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CVisualMemoryManager : cannot access class member vision_enabled!");
	return false;
}

void CScriptGameObject::set_sound_threshold(float value)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().sound().set_threshold(value);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundMemoryManager : cannot access class member set_sound_threshold!");
	}
}

void CScriptGameObject::restore_sound_threshold()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().sound().restore_threshold();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSoundMemoryManager : cannot access class member restore_sound_threshold!");
	}
}

void CScriptGameObject::SetStartDialog(LPCSTR dialog_id)
{
	if (CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object()))
	{
		pDialogManager->SetStartDialog(dialog_id);
	}
}

void CScriptGameObject::GetStartDialog()
{
	if (CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object()))
	{
		pDialogManager->GetStartDialog();
	}
}

void CScriptGameObject::RestoreDefaultStartDialog()
{
	if (CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object()))
	{
		pDialogManager->RestoreDefaultStartDialog();
	}
}

void CScriptGameObject::SetActorPosition(Fvector pos)
{
	if (CActor* actor = object().cast_actor())
	{
		CHolderCustom* holder = actor->Holder();
		if (CCar* car = holder != nullptr ? holder->cast_car() : nullptr)
		{
			car->DoExit();
		}

		Fmatrix F = actor->XFORM();
		F.c = pos;
		actor->ForceTransform(F);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call SetActorPosition method for non-actor object");
	}
}

void CScriptGameObject::SetNpcPosition(Fvector pos)
{
	if (CCustomMonster* obj = object().cast_custom_monster())
	{
		Fmatrix F = obj->XFORM();
		F.c = pos;
		obj->movement().detail().make_inactual();

		if (obj->animation_movement_controlled())
		{
			obj->destroy_anim_mov_ctrl();
		}

		obj->ForceTransform(F);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call SetActorPosition method for non-CCustomMonster object");
	}

}

void CScriptGameObject::SetActorDirection(float dir)
{
	if (CActor* actor = object().cast_actor())
	{
		actor->cam_Active()->Set(dir, 0.0f, 0.0f);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call SetActorDirection method for non-actor object");
	}
}

void CScriptGameObject::DisableHitMarks(bool disable)
{
	if (CActor* actor = object().cast_actor())
	{
		actor->DisableHitMarks(disable);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call DisableHitMarks method for non-actor object");
	}
}

bool CScriptGameObject::DisableHitMarks() const
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->DisableHitMarks();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call DisableHitMarks method for non-actor object");
	return false;
}

Fvector CScriptGameObject::GetMovementSpeed() const
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->GetMovementSpeed();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call GetMovementSpeed method for non-actor object");
	return zero_vel;
}

CHolderCustom* CScriptGameObject::get_current_holder()
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->Holder();
	}

	return 0;
}

void CScriptGameObject::set_ignore_monster_threshold(float ignore_monster_threshold)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		clamp(ignore_monster_threshold, 0.0f, 1.0f);
		stalker->memory().enemy().ignore_monster_threshold(ignore_monster_threshold);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_ignore_monster_threshold!");
	}
}

void CScriptGameObject::restore_ignore_monster_threshold()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().enemy().restore_ignore_monster_threshold();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member restore_ignore_monster_threshold!");
	}
}

float CScriptGameObject::ignore_monster_threshold() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->memory().enemy().ignore_monster_threshold();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member ignore_monster_threshold!");
	return 0.0f;
}

void CScriptGameObject::set_max_ignore_monster_distance(const float& max_ignore_monster_distance)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().enemy().max_ignore_monster_distance(max_ignore_monster_distance);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member set_max_ignore_monster_distance!");
	}
}

void CScriptGameObject::restore_max_ignore_monster_distance()
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().enemy().restore_max_ignore_monster_distance();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member restore_max_ignore_monster_distance!");
	}
}

float CScriptGameObject::max_ignore_monster_distance() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return stalker->memory().enemy().max_ignore_monster_distance();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member max_ignore_monster_distance!");
	return 0.0f;
}

CCar* CScriptGameObject::get_car()
{
	if (CCar* car = object().cast_car())
	{
		return car;
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CGameObject : cannot access class member get_car!");
	return 0;
}

#ifdef DEBUG
void CScriptGameObject::debug_planner(const script_planner* planner)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->debug_planner(planner);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member debug_planner!");
	}
}
#endif

u32 CScriptGameObject::location_on_path(float distance, Fvector* location)
{
	if (!location)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : location_on_path -> specify destination location!");
		return (u32(-1));
	}

	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		VERIFY(location);
		return monster->movement().detail().location_on_path(monster, distance, *location);
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member location_on_path!");
	return (u32(-1));
}

bool CScriptGameObject::is_there_items_to_pickup() const
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		return !!stalker->memory().item().selected();
	}
	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member is_there_items_to_pickup!");
	return false;
}

bool CScriptGameObject::IsActorLadder() const
{
	if (CActor* actor = object().cast_actor())
	{
		return actor->is_ladder();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call IsActorLadder method for non-actor object");
	return false;
}

void CScriptGameObject::ResetBoneProtections(LPCSTR imm_sect, LPCSTR bone_sect)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->ResetBoneProtections(imm_sect, bone_sect);
	}
}

void CScriptGameObject::set_visual_name(LPCSTR visual, bool bForce)
{
	if (strcmp(visual, *object().cNameVisual()) == 0)
	{
		return;
	}

	if (!bForce)
	{
		object().cNameVisual_set(visual);
		return;
	}

	if (CActor* actor = object().cast_actor())
	{
		actor->ChangeVisual(visual);
		actor->OnChangeVisual();
		return;
	}

	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		NET_Packet P;
		object().u_EventGen(P, GE_CHANGE_VISUAL, object().ID());
		P.w_stringZ(visual);
		object().u_EventSend(P);

		stalker->ChangeVisual(visual);

		CPhysicsShell* tmp_shell = stalker->PPhysicsShell();
		stalker->PPhysicsShell() = nullptr;
		stalker->OnChangeVisual();
		stalker->PPhysicsShell() = tmp_shell;
		tmp_shell = nullptr;

		if (IKinematicsAnimated* V = stalker->Visual()->dcast_PKinematicsAnimated())
		{
			if (!stalker->already_dead())
			{
				stalker->CStepManager::reload(*stalker->cNameSect());
			}

			stalker->CDamageManager::reload(*stalker->cNameSect(), "damage", pSettings);
			stalker->ResetBoneProtections(NULL, NULL);
			stalker->reattach_items();
			stalker->m_pPhysics_support->in_ChangeVisual();
			stalker->animation().reload();
		}
	}
}

void CScriptGameObject::set_visual_name_notForce(LPCSTR visual)
{
	set_visual_name(visual, false);
}

LPCSTR CScriptGameObject::get_visual_name() const
{
	return object().cNameVisual().c_str();
}

void CScriptGameObject::RemoveMemorySoundObject(const MemorySpace::CSoundObject &memory_object)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().sound().remove(&memory_object);
	}
}

void CScriptGameObject::RemoveMemoryHitObject(const MemorySpace::CHitObject &memory_object)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().hit().remove(&memory_object);
	}
}

void CScriptGameObject::RemoveMemoryVisibleObject(const MemorySpace::CVisibleObject &memory_object)
{
	if (CAI_Stalker* stalker = object().cast_stalker())
	{
		stalker->memory().visual().remove(&memory_object);
	}
}
