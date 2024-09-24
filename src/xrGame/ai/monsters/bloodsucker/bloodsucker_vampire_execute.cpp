#include "stdafx.h"
#include "../../../xrCore/_vector3d_ext.h"
#include "sound_player.h"
#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "ai_object_location.h"
#include "bloodsucker.h"
#include "bloodsucker_vampire_execute.h"

#include "../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../actor.h"
#include "../../../../xrEngine/CameraBase.h"

#include "../../../HudManager.h"

CustomBloodsuckerStateVampireExecute::CustomBloodsuckerStateVampireExecute(CustomBloodsucker* object) : inherited(object)
{
	m_pBloodsucker = smart_cast<CustomBloodsucker*>(object);
}

CustomBloodsuckerStateVampireExecute::~CustomBloodsuckerStateVampireExecute()
{

}

void CustomBloodsuckerStateVampireExecute::initialize()
{
	inherited::initialize();
	CActor* actor = nullptr;
	if (IsGameTypeSingle())
		this->m_pBloodsucker->install();
	else
	{
		actor = const_cast<CActor*>(smart_cast<const CActor*>(this->object->EnemyMan.get_enemy()));
		if (actor)
			this->m_pBloodsucker->install(actor);
		else
			return;
	}
	look_head();

	m_action = eActionPrepare;
	time_vampire_started = 0;

	this->m_pBloodsucker->set_visibility_state(CustomBloodsucker::full_visibility);

	this->m_pBloodsucker->m_hits_before_vampire = 0;
	this->m_pBloodsucker->m_sufficient_hits_before_vampire_random = -1 + (rand() % 3);

	if (IsGameTypeSingle())
	{
		HUD().SetRenderable(false);
		NET_Packet			P;
		Actor()->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, Actor()->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		Actor()->u_EventSend(P);

		Actor()->set_inventory_disabled(true);
	}
	else
	{
		this->m_pBloodsucker->sendToStartVampire(actor);
	}
	m_effector_activated = false;
}

void CustomBloodsuckerStateVampireExecute::execute()
{
	if (!this->m_pBloodsucker->is_turning() && !m_effector_activated) {
		this->m_pBloodsucker->ActivateVampireEffector();
		m_effector_activated = true;
	}

	look_head();

	switch (m_action) {
	case eActionPrepare:
		execute_vampire_prepare();
		m_action = eActionContinue;
		break;

	case eActionContinue:
		execute_vampire_continue();
		break;

	case eActionFire:
		execute_vampire_hit();
		m_action = eActionWaitTripleEnd;
		break;

	case eActionWaitTripleEnd:
		if (!this->object->com_man().ta_is_active()) {
			m_action = eActionCompleted;
		}

	case eActionCompleted:
		break;
	}

	this->m_pBloodsucker->dir().face_target(this->object->EnemyMan.get_enemy());

	Fvector const enemy_to_self = this->object->EnemyMan.get_enemy()->Position() - this->object->Position();
	float const dist_to_enemy = magnitude(enemy_to_self);
	float const vampire_dist = this->m_pBloodsucker->get_vampire_distance();

	if (angle_between_vectors(this->object->Direction(), enemy_to_self) < deg2rad(20.f) &&
		dist_to_enemy > vampire_dist)
	{
		this->object->set_action(ACT_RUN);
		this->object->anim().accel_activate(eAT_Aggressive);
		this->object->anim().accel_set_braking(false);

		u32 const target_vertex = this->object->EnemyMan.get_enemy()->ai_location().level_vertex_id();
		Fvector const target_pos = ai().level_graph().vertex_position(target_vertex);

		this->object->path().set_target_point(target_pos, target_vertex);
		this->object->path().set_rebuild_time(100);
		this->object->path().set_use_covers(false);
		this->object->path().set_distance_to_end(vampire_dist);
	}
	else
	{
		this->object->set_action(ACT_STAND_IDLE);
	}
}

void CustomBloodsuckerStateVampireExecute::show_hud()
{
	HUD().SetRenderable(true);
	NET_Packet			P;

	Actor()->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, Actor()->ID());
	P.w_u16(INV_STATE_BLOCK_ALL);
	P.w_u8(u8(false));
	Actor()->u_EventSend(P);
}

void CustomBloodsuckerStateVampireExecute::cleanup()
{
	if (IsGameTypeSingle())
		Actor()->set_inventory_disabled(false);
	else
		this->m_pBloodsucker->sendToStopVampire();
	if (this->object->com_man().ta_is_active())
		this->object->com_man().ta_deactivate();

	if (this->m_pBloodsucker->is_controlling())
		this->m_pBloodsucker->release();

	if (IsGameTypeSingle())
		show_hud();
}

void CustomBloodsuckerStateVampireExecute::finalize()
{
	inherited::finalize();
	cleanup();
}

void CustomBloodsuckerStateVampireExecute::critical_finalize()
{
	inherited::critical_finalize();
	cleanup();
}

bool CustomBloodsuckerStateVampireExecute::check_start_conditions()
{
	const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();

	if (!this->m_pBloodsucker->done_enough_hits_before_vampire())
		return false;

	u32 const vertex_id = ai().level_graph().check_position_in_direction(this->object->ai_location().level_vertex_id(),
		this->object->Position(),
		enemy->Position());
	if (!ai().level_graph().valid_vertex_id(vertex_id))
		return false;

	if (!this->object->MeleeChecker.can_start_melee(enemy))
		return false;

	if (!this->object->control().direction().is_face_target(enemy, PI_DIV_2))
		return false;

	if (!this->m_pBloodsucker->WantVampire())
		return false;

	if (!smart_cast<CActor const*>(enemy))
		return false;

	if (this->m_pBloodsucker->is_controlling())
		return false;

	const CActor* actor = smart_cast<const CActor*>(enemy);

	VERIFY(actor);

	if (actor->input_external_handler_installed())
		return false;

	return true;
}

bool CustomBloodsuckerStateVampireExecute::check_completion()
{
	return (m_action == eActionCompleted);
}

//////////////////////////////////////////////////////////////////////////

void CustomBloodsuckerStateVampireExecute::execute_vampire_prepare()
{
	this->object->com_man().ta_activate(this->m_pBloodsucker->anim_triple_vampire);
	time_vampire_started = Device.dwTimeGlobal;

	this->m_pBloodsucker->sound().play(CustomBloodsucker::eVampireGrasp);
}

void CustomBloodsuckerStateVampireExecute::execute_vampire_continue()
{
	const CEntityAlive* enemy = this->object->EnemyMan.get_enemy();

	if (!this->object->MeleeChecker.can_start_melee(enemy)) {
		this->object->com_man().ta_deactivate();
		m_action = eActionCompleted;
		return;
	}

	this->m_pBloodsucker->sound().play(CustomBloodsucker::eVampireSucking);

	if (time_vampire_started + SBloodsuckerStateVampireExecuteProperies::TimeHold < Device.dwTimeGlobal) {
		m_action = eActionFire;
	}
}

void CustomBloodsuckerStateVampireExecute::execute_vampire_hit()
{
	this->object->com_man().ta_pointbreak();
	this->object->sound().play(CustomBloodsucker::eVampireHit);
	this->m_pBloodsucker->SatisfyVampire();
}

//////////////////////////////////////////////////////////////////////////

void CustomBloodsuckerStateVampireExecute::look_head()
{
	IKinematics* pK = smart_cast<IKinematics*>(this->object->Visual());
	Fmatrix bone_transform;
	bone_transform = pK->LL_GetTransform(pK->LL_BoneID("bip01_head"));

	Fmatrix global_transform;
	global_transform.mul_43(this->object->XFORM(), bone_transform);

	this->m_pBloodsucker->look_point(global_transform.c);
}
