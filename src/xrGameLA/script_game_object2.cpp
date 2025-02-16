////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object_script2.cpp
//	Created 	: 17.11.2004
//  Modified 	: 17.11.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class script export
////////////////////////////////////////////////////////////////////////////

#include "pch_script.h"
#include "script_game_object.h"
#include "script_game_object_impl.h"
#include "ai_space.h"
#include "script_engine.h"
#include "explosive.h"
#include "script_zone.h"
#include "object_handler.h"
#include "script_hit.h"
#include "../../Include/xrRender/Kinematics.h"
#include "pda.h"
#include "InfoPortion.h"
#include "memory_manager.h"
#include "ai_phrasedialogmanager.h"
#include "../../xrCore/net_utils.h"
#include "xrMessages.h"
#include "custommonster.h"
#include "memory_manager.h"
#include "visual_memory_manager.h"
#include "sound_memory_manager.h"
#include "hit_memory_manager.h"
#include "enemy_manager.h"
#include "item_manager.h"
#include "danger_manager.h"
#include "memory_space.h"
#include "actor.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../CameraBase.h"
#include "ai/stalker/ai_stalker.h"
#include "car.h"
#include "pda.h"
#include "mounted_turret.h"
#include "movement_manager.h"
#include "detail_path_manager.h"
#include "inventory.h"
#include "holder_custom.h"
#include "level.h"

void CScriptGameObject::explode	(u32 level_time)
{
	CExplosive			*explosive = smart_cast<CExplosive*>(&object());
	if (object().H_Parent())
	{
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CExplosive : cannot explode object wiht parent!");
		return;
	}

	if (!explosive)
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CExplosive : cannot access class member explode!");
	else {
		Fvector normal;
		explosive->FindNormal(normal);
		explosive->SetInitiator(object().ID());
		explosive->GenExplodeEvent(object().Position(), normal);
	}
}

bool CScriptGameObject::active_zone_contact		(u16 id)
{
	CScriptZone		*script_zone = smart_cast<CScriptZone*>(&object());
	if (!script_zone) {
		ai().script_engine().script_log	(ScriptStorage::eLuaMessageTypeError,"CScriptZone : cannot access class member active_zone_contact!");
		return		(false);
	}
	return			(script_zone->active_contact(id));
}

CScriptGameObject *CScriptGameObject::best_weapon()
{
	CObjectHandler	*object_handler = smart_cast<CAI_Stalker*>(&object());
	if (!object_handler) {
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CScriptEntity : cannot access class member best_weapon!");
		return			(0);
	}
	else {
		CGameObject		*game_object = object_handler->best_weapon() ? &object_handler->best_weapon()->object() : 0;
		return			(game_object ? game_object->lua_game_object() : 0);
	}
}

void CScriptGameObject::set_item		(MonsterSpace::EObjectAction object_action)
{
	CObjectHandler			*object_handler = smart_cast<CAI_Stalker*>(&object());
	if (!object_handler)
		ai().script_engine().script_log					(ScriptStorage::eLuaMessageTypeError,"CObjectHandler : cannot access class member set_item!");
	else
		object_handler->set_goal(object_action);
}

void CScriptGameObject::set_item		(MonsterSpace::EObjectAction object_action, CScriptGameObject *lua_game_object)
{
	CObjectHandler			*object_handler = smart_cast<CAI_Stalker*>(&object());
	if (!object_handler)
		ai().script_engine().script_log					(ScriptStorage::eLuaMessageTypeError,"CObjectHandler : cannot access class member set_item!");
	else
		object_handler->set_goal(object_action,lua_game_object ? &lua_game_object->object() : 0);
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action, CScriptGameObject *lua_game_object, u32 queue_size)
{
	CObjectHandler			*object_handler = smart_cast<CAI_Stalker*>(&object());
	if (!object_handler)
		ai().script_engine().script_log					(ScriptStorage::eLuaMessageTypeError,"CObjectHandler : cannot access class member set_item!");
	else
		object_handler->set_goal(object_action,lua_game_object ? &lua_game_object->object() : 0, queue_size, queue_size);
}

void CScriptGameObject::set_item(MonsterSpace::EObjectAction object_action, CScriptGameObject *lua_game_object, u32 queue_size, u32 queue_interval)
{
	CObjectHandler			*object_handler = smart_cast<CAI_Stalker*>(&object());
	if (!object_handler)
		ai().script_engine().script_log					(ScriptStorage::eLuaMessageTypeError,"CObjectHandler : cannot access class member set_item!");
	else
		object_handler->set_goal(object_action,lua_game_object ? &lua_game_object->object() : 0, queue_size, queue_size, queue_interval, queue_interval);
}

void CScriptGameObject::play_cycle(LPCSTR anim, bool mix_in)
{
	IKinematicsAnimated* sa=smart_cast<IKinematicsAnimated*>(object().Visual());
	if(sa){
		MotionID m	= sa->ID_Cycle(anim);
		if (m) sa->PlayCycle(m,(BOOL)mix_in);
		else 
		{
			ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError ,"CGameObject : has not cycle %s",anim);
		}
	}
	else
	{
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : is not animated object");
	}
}

void CScriptGameObject::play_cycle(LPCSTR anim)
{
	play_cycle	(anim,true);
}

void CScriptGameObject::Hit(CScriptHit *tpLuaHit)
{
	CScriptHit		&tLuaHit = *tpLuaHit;
	NET_Packet		P;
	SHit			HS;
	HS.GenHeader(GE_HIT,object().ID());										//	object().u_EventGen(P,GE_HIT,object().ID());
	THROW2			(tLuaHit.m_tpDraftsman,"Where is hit initiator??!");	//	THROW2			(tLuaHit.m_tpDraftsman,"Where is hit initiator??!");
	HS.whoID  = u16(tLuaHit.m_tpDraftsman->ID());							//	P.w_u16			(u16(tLuaHit.m_tpDraftsman->ID()));
	HS.weaponID = 0;														//	P.w_u16			(0);
	HS.dir = tLuaHit.m_tDirection;											//	P.w_dir			(tLuaHit.m_tDirection);
	HS.power = tLuaHit.m_fPower;											//	P.w_float		(tLuaHit.m_fPower);
	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());		//	CKinematics		*V = smart_cast<CKinematics*>(object().Visual());
	VERIFY			(V);													//	VERIFY			(V);
	if (xr_strlen	(tLuaHit.m_caBoneName))									//	if (xr_strlen	(tLuaHit.m_caBoneName))
		HS.boneID = 		(V->LL_BoneID(tLuaHit.m_caBoneName));			//		P.w_s16		(V->LL_BoneID(tLuaHit.m_caBoneName));
	else																	//	else
		HS.boneID = 		(s16(0));										//		P.w_s16		(s16(0));
	HS.p_in_bone_space = Fvector().set(0,0,0);								//	P.w_vec3		(Fvector().set(0,0,0));
	HS.impulse = tLuaHit.m_fImpulse;										//	P.w_float		(tLuaHit.m_fImpulse);
	HS.hit_type = (ALife::EHitType)(tLuaHit.m_tHitType);					//	P.w_u16			(u16(tLuaHit.m_tHitType));
	HS.Write_Packet(P);						

	object().u_EventSend(P);
}


#pragma todo("Dima to Dima : find out why user defined conversion operators work incorrect")

CScriptGameObject::operator CObject*()
{
	return			(&object());
}

CScriptGameObject *CScriptGameObject::GetBestEnemy()
{
	const CCustomMonster	*monster = smart_cast<const CCustomMonster*>(&object());
	if (!monster)
		return				(0);

	if (monster->memory().enemy().selected())
		return				(monster->memory().enemy().selected()->lua_game_object());
	return					(0);
} 

const CDangerObject *CScriptGameObject::GetBestDanger()
{
	const CCustomMonster	*monster = smart_cast<const CCustomMonster*>(&object());
	if (!monster)
		return				(0);

	if (!monster->memory().danger().selected())
		return				(0);

	return					(monster->memory().danger().selected());
}

CScriptGameObject *CScriptGameObject::GetBestItem()
{
	const CCustomMonster	*monster = smart_cast<const CCustomMonster*>(&object());
	if (!monster)
		return				(0);

	if (monster->memory().item().selected())
		return				(monster->memory().item().selected()->lua_game_object());
	return					(0);
}

u32 CScriptGameObject::memory_time(const CScriptGameObject &lua_game_object)
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CScriptEntity : cannot access class member memory!");
		return				(0);
	}
	else
		return				(monster->memory().memory_time(&lua_game_object.object()));
}

Fvector CScriptGameObject::memory_position(const CScriptGameObject &lua_game_object)
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CScriptEntity : cannot access class member memory!");
		return				(Fvector().set(0.f,0.f,0.f));
	}
	else
		return				(monster->memory().memory_position(&lua_game_object.object()));
}

void CScriptGameObject::enable_memory_object	(CScriptGameObject *game_object, bool enable)
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster)
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member enable_memory_object!");
	else
		monster->memory().enable			(&game_object->object(),enable);
}

const xr_vector<CNotYetVisibleObject> &CScriptGameObject::not_yet_visible_objects() const
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member not_yet_visible_objects!");
		NODEFAULT;
	}
	return					(monster->memory().visual().not_yet_visible_objects());
}

float CScriptGameObject::visibility_threshold	() const
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member visibility_threshold!");
		NODEFAULT;
	}
	return					(monster->memory().visual().visibility_threshold());
}

void CScriptGameObject::enable_vision			(bool value)
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CVisualMemoryManager : cannot access class member enable_vision!");
		return;
	}
	monster->memory().visual().enable		(value);
}

bool CScriptGameObject::vision_enabled			() const
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CVisualMemoryManager : cannot access class member vision_enabled!");
		return								(false);
	}
	return									(monster->memory().visual().enabled());
}

void CScriptGameObject::set_sound_threshold		(float value)
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CSoundMemoryManager : cannot access class member set_sound_threshold!");
		return;
	}
	monster->memory().sound().set_threshold		(value);
}

void CScriptGameObject::restore_sound_threshold	()
{
	CCustomMonster			*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CSoundMemoryManager : cannot access class member restore_sound_threshold!");
		return;
	}
	monster->memory().sound().restore_threshold	();
}

void CScriptGameObject::SetStartDialog(LPCSTR dialog_id)
{
	CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object());
	if(!pDialogManager) return;
	pDialogManager->SetStartDialog(dialog_id);
}

LPCSTR CScriptGameObject::GetStartDialog()
{
	CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object());
	if(!pDialogManager) return "";
	return pDialogManager->GetStartDialog().c_str();
}
void CScriptGameObject::RestoreDefaultStartDialog()
{
	CAI_PhraseDialogManager* pDialogManager = smart_cast<CAI_PhraseDialogManager*>(&object());
	if(!pDialogManager) return;
	pDialogManager->RestoreDefaultStartDialog();
}

CScriptGameObject* CScriptGameObject::GetActorCar()
{
	CActor* actor = smart_cast<CActor*>(&object());
	if (actor)
	{
		CCar* car = actor->GetAttachedCar();
		return car ? car->lua_game_object() : NULL;
	}
	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "ScriptGameObject : attempt to call GetActorCar method for non-actor object");
	return NULL;
}

void CScriptGameObject::SetActorPosition			(Fvector pos)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		CCar* car = smart_cast<CCar*>(actor->Holder());
		if (car)
			car->DoExit();
		
		Fmatrix F = actor->XFORM();
		F.c = pos;
		actor->ForceTransform(F);
		
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorPosition method for non-actor object");

}

void CScriptGameObject::SetActorDirection		(float dir)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		actor->cam_Active()->Set(dir,0,0);
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorDirection method for non-actor object");
}

void CScriptGameObject::SetNpcPosition			(Fvector pos)
{
	CCustomMonster* obj = smart_cast<CCustomMonster*>(&object());
	if(obj){
		Fmatrix F = obj->XFORM();
		F.c = pos;
		obj->movement().detail().make_inactual();
		if (obj->animation_movement_controlled())
			obj->destroy_anim_mov_ctrl();
		obj->ForceTransform(F);
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorPosition method for non-CCustomMonster object");
}

void CScriptGameObject::SetActorDirectionVector		(Fvector dir)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		actor->cam_Active()->Set(dir.x,dir.y,dir.z);
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorDirectionVector method for non-actor object");
}

void CScriptGameObject::SetActorCamActive	(Fvector pos, Fvector dir, Fvector norm)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor)
		actor->cam_Active()->Set(pos,dir,norm);
	else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorCameraVectors method for non-actor object");
}

void CScriptGameObject::SetActorDirectionSlowly		(Fvector pos, float time)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		if (time>0.1f) //skyloader: time can't be <=0.f
			actor->SetDirectionSlowly(pos, time);
		else { //skyloader: set direction once
			// get yaw and pitch to target
			float cam_target_yaw, cam_target_pitch;
			Fvector	P,D,N;

			actor->cam_Active()->Get	(P,D,N);
			Fvector().sub(pos, P).getHP	(cam_target_yaw, cam_target_pitch);
			D.setHP				(cam_target_yaw, cam_target_pitch);
			actor->cam_Active()->Set	(P,D,N);
		}
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorDirectionSlowly method for non-actor object");
}

void CScriptGameObject::SetActorLegsVisible			(bool val)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		actor->setVisible(val);
		actor->SetDrawLegs(val);
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorLegsVisible method for non-actor object");
}

void CScriptGameObject::SetActorCam			(u8 val)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		switch (val)
		{
		case 1: actor->cam_Set(eacFirstEye); break;
		case 2: actor->cam_Set(eacLookAt); break;
		case 3: actor->cam_Set(eacFreeLook); break;
		}
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorCamSet method for non-actor object");
}

bool CScriptGameObject::IsFirstEyeCam			()
{
	CActor* actor = smart_cast<CActor*>(&object());
	return			(actor && actor->IsFirstEye());
}

void CScriptGameObject::SetHandsOnly			(bool val)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if(actor){
		actor->inventory().SetHandsOnly(val);
	}else
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"ScriptGameObject : attempt to call SetActorLegsVisible method for non-actor object");
}

bool CScriptGameObject::IsHandsOnly			()
{
	CActor* actor = smart_cast<CActor*>(&object());
	return			(actor && actor->inventory().IsHandsOnly());
}

void CScriptGameObject::SetIconState		(u8 state, bool show)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if (!actor)
	{
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject : attempt to call SetIconState method for non-actor object");
		return;
	}
	actor->SetIconState(EActorState(state), show);
}

bool CScriptGameObject::GetIconState(u8 state)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if (!actor)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CScriptGameObject : attempt to call GetIconState method for non-actor object");
		return false;
	}
	return actor->GetIconState(EActorState(state));
}

CHolderCustom* CScriptGameObject::get_current_holder()
{
	CActor* actor = smart_cast<CActor*>(&object());

	if(actor)
		return actor->Holder();
	else
		return NULL;
}

u16 CScriptGameObject::get_current_holder_id()
{
	CActor* actor = smart_cast<CActor*>(&object());

	if(actor)
		return actor->HolderID();
	else
		return NULL;
}


void CScriptGameObject::detach_actor_Vehicle	()
{
	CActor* actor = smart_cast<CActor*>(&object());

	if(!actor)
	{
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject : attempt to call detach_actor_Vehicle method for non-actor object");
		return;
	}

	CCar* car = smart_cast<CCar*>(actor->Holder());
	if(car)
		car->DoExit();
}


void CScriptGameObject::attach_actor_Vehicle	(u32 id)
{
	CActor* actor = smart_cast<CActor*>(&object());
	if (!actor)
	{
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CScriptGameObject : attempt to call attach_actor_Vehicle method for non-actor object");
		return;
	}

	CObject* O	= Level().Objects.net_Find	(id);
	if (!O)
	{
		Msg("! Error: No object to attach holder [%d]", id);
		return;
	}

	CHolderCustom*	holder = smart_cast<CHolderCustom*>(O);
	if (!holder)
	{
		Msg("! Error: Object[%d] is not holder", id);
		return;
	}

	CCar* car = smart_cast<CCar*>(holder);
	R_ASSERT2(car, "object must be a car");
	car->DoEnter();
	actor->attach_Vehicle(holder);
}

u16 CScriptGameObject::BoneNameToId(LPCSTR name)
{
	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());
	VERIFY			(V);
	return (V->LL_BoneID(name));
}

bool CScriptGameObject::GetBoneVisible(LPCSTR name)
{
	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());
	VERIFY			(V);
	return (TRUE==V->LL_GetBoneVisible(V->LL_BoneID(name)));
}

void CScriptGameObject::SetBoneVisible(LPCSTR name, bool val, bool bRecursive)
{
	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());
	VERIFY			(V);
	return (V->LL_SetBoneVisible(V->LL_BoneID(name), val, bRecursive));
}

bool CScriptGameObject::BoneExist(LPCSTR name)
{
	IKinematics		*V = smart_cast<IKinematics*>(object().Visual());
	VERIFY			(V);
	return (V->LL_BoneID(name) != BI_NONE);
}

void CScriptGameObject::set_ignore_monster_threshold	(float ignore_monster_threshold)
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member set_ignore_monster_threshold!");
		return;
	}
	clamp				(ignore_monster_threshold,0.f,1.f);
	stalker->memory().enemy().ignore_monster_threshold	(ignore_monster_threshold);
}

void CScriptGameObject::restore_ignore_monster_threshold	()
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member restore_ignore_monster_threshold!");
		return;
	}
	stalker->memory().enemy().restore_ignore_monster_threshold	();
}

float CScriptGameObject::ignore_monster_threshold		() const
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member ignore_monster_threshold!");
		return			(0.f);
	}
	return				(stalker->memory().enemy().ignore_monster_threshold());
}

void CScriptGameObject::set_max_ignore_monster_distance	(const float &max_ignore_monster_distance)
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member set_max_ignore_monster_distance!");
		return;
	}
	stalker->memory().enemy().max_ignore_monster_distance	(max_ignore_monster_distance);
}

void CScriptGameObject::restore_max_ignore_monster_distance	()
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member restore_max_ignore_monster_distance!");
		return;
	}
	stalker->memory().enemy().restore_max_ignore_monster_distance	();
}

float CScriptGameObject::max_ignore_monster_distance	() const
{
	CAI_Stalker			*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member max_ignore_monster_distance!");
		return			(0.f);
	}
	return				(stalker->memory().enemy().max_ignore_monster_distance());
}

CMountedTurret* CScriptGameObject::get_turret()
{
	CMountedTurret *turret = smart_cast<CMountedTurret*>(&object());
	R_ASSERT2(turret, make_string<const char*>("CMountedTurret: can't cast '%s'", object().cName()));
	return turret;
}

CCar* CScriptGameObject::get_car	()
{
	CCar		*car = smart_cast<CCar*>(&object());
	if (!car) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member get_car!");
		NODEFAULT;
	}
	return car;
}

/*CPda* CScriptGameObject::get_pda	()
{
	CPda		*pda = smart_cast<CPda*>(&object());
	if (!pda) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CGameObject : cannot access class member get_pda!");
		NODEFAULT;
	}
	return pda;
}*/

#ifdef LOG_PLANNER
void CScriptGameObject::debug_planner				(const script_planner *planner)
{
	CAI_Stalker		*stalker = smart_cast<CAI_Stalker*>(&object());
	if (!stalker) {
		ai().script_engine().script_log			(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member debug_planner!");
		return;
	}

	stalker->debug_planner	(planner);
}
#endif

u32 CScriptGameObject::location_on_path				(float distance, Fvector *location)
{
	if (!location) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : location_on_path -> specify destination location!");
		return								(u32(-1));
	}

	CCustomMonster							*monster = smart_cast<CCustomMonster*>(&object());
	if (!monster) {
		ai().script_engine().script_log		(ScriptStorage::eLuaMessageTypeError,"CAI_Stalker : cannot access class member location_on_path!");
		return								(u32(-1));
	}

	VERIFY									(location);
	return									(monster->movement().detail().location_on_path(monster,distance,*location));
}
