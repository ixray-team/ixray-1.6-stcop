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

CustomBloodsuckerStateVampireExecute::CustomBloodsuckerStateVampireExecute(CBloodsuckerBase* object) : inherited(object)
{
	pBloodsuckerBase = smart_cast<CBloodsuckerBase*>(object);

	m_action = {};
	time_vampire_started = {};

	m_effector_activated = {};
}

CustomBloodsuckerStateVampireExecute::~CustomBloodsuckerStateVampireExecute()
{

}

void CustomBloodsuckerStateVampireExecute::initialize()
{
	inherited::initialize();
	CActor* actor = nullptr;
	if (IsGameTypeSingle())
		pBloodsuckerBase->install();
	else
	{
		actor = const_cast<CActor*>(smart_cast<const CActor*>(object->EnemyMan.get_enemy()));
		if (actor)
			pBloodsuckerBase->install(actor);
		else
			return;
	}
	look_head();

	m_action = eActionPrepare;
	time_vampire_started = 0;

	pBloodsuckerBase->set_visibility_state(CBloodsuckerBase::full_visibility);

	pBloodsuckerBase->m_hits_before_vampire = 0;
	pBloodsuckerBase->m_sufficient_hits_before_vampire_random = -1 + (rand() % 3);

	if (IsGameTypeSingle())
	{
		HUD().SetRenderable(false);
		NET_Packet			P{};
		Actor()->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, Actor()->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		Actor()->u_EventSend(P);

		Actor()->set_inventory_disabled(true);
	}
	else
	{
		pBloodsuckerBase->sendToStartVampire(actor);
	}
	m_effector_activated = false;
}

void CustomBloodsuckerStateVampireExecute::execute()
{
	if (!pBloodsuckerBase->is_turning() && !m_effector_activated) {
		pBloodsuckerBase->ActivateVampireEffector();
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
		if (!object->com_man().ta_is_active()) {
			m_action = eActionCompleted;
		}

	case eActionCompleted:
		break;
	}

	pBloodsuckerBase->dir().face_target(object->EnemyMan.get_enemy());

	Fvector const enemy_to_self = object->EnemyMan.get_enemy()->Position() - object->Position();
	float const dist_to_enemy = magnitude(enemy_to_self);
	float const vampire_dist = pBloodsuckerBase->get_vampire_distance();

	if (angle_between_vectors(object->Direction(), enemy_to_self) < deg2rad(20.f) &&
		dist_to_enemy > vampire_dist)
	{
		object->set_action(ACT_RUN);
		object->anim().accel_activate(eAT_Aggressive);
		object->anim().accel_set_braking(false);

		u32 const target_vertex = object->EnemyMan.get_enemy()->ai_location().level_vertex_id();
		Fvector const target_pos = ai().level_graph().vertex_position(target_vertex);

		object->path().set_target_point(target_pos, target_vertex);
		object->path().set_rebuild_time(100);
		object->path().set_use_covers(false);
		object->path().set_distance_to_end(vampire_dist);
	}
	else
	{
		object->set_action(ACT_STAND_IDLE);
	}
}

void CustomBloodsuckerStateVampireExecute::show_hud()
{
	HUD().SetRenderable(true);
	NET_Packet			P{};

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
		pBloodsuckerBase->sendToStopVampire();
	if (object->com_man().ta_is_active())
		object->com_man().ta_deactivate();

	if (pBloodsuckerBase->is_controlling())
		pBloodsuckerBase->release();

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
	const CEntityAlive* enemy = object->EnemyMan.get_enemy();

	if (!pBloodsuckerBase->done_enough_hits_before_vampire())
		return false;

	u32 const vertex_id = ai().level_graph().check_position_in_direction(object->ai_location().level_vertex_id(),
		object->Position(),
		enemy->Position());
	if (!ai().level_graph().valid_vertex_id(vertex_id))
		return false;

	if (!object->MeleeChecker.can_start_melee(enemy))
		return false;

	if (!object->control().direction().is_face_target(enemy, PI_DIV_2))
		return false;

	if (!pBloodsuckerBase->WantVampire())
		return false;

	if (!smart_cast<CActor const*>(enemy))
		return false;

	if (pBloodsuckerBase->is_controlling())
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
	object->com_man().ta_activate(pBloodsuckerBase->anim_triple_vampire);
	time_vampire_started = Device.dwTimeGlobal;

	pBloodsuckerBase->sound().play(CBloodsuckerBase::eVampireGrasp);
}

void CustomBloodsuckerStateVampireExecute::execute_vampire_continue()
{
	const CEntityAlive* enemy = object->EnemyMan.get_enemy();

	if (!object->MeleeChecker.can_start_melee(enemy)) {
		object->com_man().ta_deactivate();
		m_action = eActionCompleted;
		return;
	}

	pBloodsuckerBase->sound().play(CBloodsuckerBase::eVampireSucking);
	
	if (time_vampire_started + EntityDefinitions::CBloodsuckerBase::TimeHold < Device.dwTimeGlobal) {
		m_action = eActionFire;
	}
}

void CustomBloodsuckerStateVampireExecute::execute_vampire_hit()
{
	object->com_man().ta_pointbreak();
	object->sound().play(CBloodsuckerBase::eVampireHit);
	pBloodsuckerBase->SatisfyVampire();
}

//////////////////////////////////////////////////////////////////////////

void CustomBloodsuckerStateVampireExecute::look_head()
{
	IKinematics* pK = smart_cast<IKinematics*>(object->Visual());
	Fmatrix bone_transform{};
	bone_transform = pK->LL_GetTransform(pK->LL_BoneID("bip01_head"));

	Fmatrix global_transform{};
	global_transform.mul_43(object->XFORM(), bone_transform);

	pBloodsuckerBase->look_point(global_transform.c);
}
