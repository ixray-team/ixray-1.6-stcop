#pragma once

//#include "../../../../xrRender/skeletoncustom.h"
#include "../../../actor.h"
#include "../../../../CameraBase.h"
#include "../../../../CustomHUD.h"
#include "../../../../../xrCore/_vector3d_ext.h"
#include "../../../hudmanager.h"
#include "../../../UIGameCustom.h"
#include "../../../UI/UIStatic.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>

#define CStateZombieChokeExecuteAbstract CStateZombieChokeExecute<_Object>

#define CHOKE_TIME_HOLD		4000
#define CHOKE_HIT_IMPULSE		40.f
#define CHOKE_MIN_DIST		1.5f
#define CHOKE_MAX_DIST		2.f
extern bool g_bDisableAllInput;

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::initialize()
{
	controlling_value = 1;

	inherited::initialize					();

	object->CControlledActor::install		();
	object->CControlledActor::set_min_speed	(3.f);
	object->CControlledActor::set_max_speed	(5.f);

	look_head				();

	m_action				= eActionPrepare;
	time_choke_started	= 0;

	psHUD_Flags.set(HUD_DRAW, FALSE);
	g_bDisableAllInput = true;
	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, true);
	HUD().GetGameUI()->AddCustomStatic("zombie_choke", true);

	m_effector_activated			= false;
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::execute()
{
	if (/*!object->CControlledActor::is_turning() &&*/ !m_effector_activated) {
		object->ActivateChokeEffector	();
		m_effector_activated			= true;
	}
	
	look_head							();

	switch (m_action) {
		case eActionPrepare:
			execute_choke_prepare();
			m_action = eActionContinue;
			break;

		case eActionContinue:
			execute_choke_continue();
			break;

		case eActionFire:
			execute_choke_hit();
			m_action = eActionWaitTripleEnd;
			break;

		case eActionWaitTripleEnd:
			if (!object->com_man().ta_is_active()) {
				m_action = eActionCompleted; 
			}

		case eActionCompleted:
			controlling_value = 0;
			break;
	}
	object->set_action						(ACT_STAND_IDLE);
	object->dir().face_target	(object->EnemyMan.get_enemy());	
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::show_hud()
{
	psHUD_Flags.set(HUD_DRAW, TRUE);
	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
	g_bDisableAllInput = false;
	HUD().GetGameUI()->RemoveCustomStatic("zombie_choke");
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::cleanup()
{	
	if ( object->com_man().ta_is_active() )
		object->com_man().ta_deactivate();

	if (object->CControlledActor::is_controlling())
		object->CControlledActor::release		();

	show_hud();
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::finalize()
{
	inherited::finalize();
	cleanup();
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::critical_finalize()
{
	inherited::critical_finalize();
	cleanup();
}

TEMPLATE_SPECIALIZATION
bool CStateZombieChokeExecuteAbstract::check_start_conditions()
{
	const CEntityAlive	*enemy = object->EnemyMan.get_enemy();

	if (enemy->CLS_ID != CLSID_OBJECT_ACTOR)							return false;
	
	// проверить дистанцию
	float dist		= object->MeleeChecker.distance_to_enemy(enemy);
	if ((dist > CHOKE_MAX_DIST) || (dist < CHOKE_MIN_DIST))					return false;

	if (object->CControlledActor::is_controlling())							return false;

	const CActor *m_actor = smart_cast<const CActor*>(enemy);
	VERIFY(m_actor);
	if (m_actor->input_external_handler_installed())						return false;

	if (controlling_value == 1)									return false;

	// проверить направление на врага
	if (!object->control().direction().is_face_target(enemy, PI_DIV_6))				return false;

	return true;
}

TEMPLATE_SPECIALIZATION
bool CStateZombieChokeExecuteAbstract::check_completion()
{
	return (m_action == eActionCompleted);
}

//////////////////////////////////////////////////////////////////////////

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::execute_choke_prepare()
{
	object->com_man().ta_activate		(object->anim_triple_choke);
	time_choke_started				= Device.dwTimeGlobal;
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::execute_choke_continue()
{
	if (object->Position().distance_to(Actor()->Position()) > 2.f) {
		object->com_man().ta_deactivate();
		m_action = eActionCompleted;
		return;
	}

	// проверить на грави удар
	if (time_choke_started + CHOKE_TIME_HOLD < Device.dwTimeGlobal) {
		m_action = eActionFire;
	}
}

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::execute_choke_hit()
{
	object->com_man().ta_pointbreak				();
	object->ChokeCompleted						();
}

//////////////////////////////////////////////////////////////////////////

TEMPLATE_SPECIALIZATION
void CStateZombieChokeExecuteAbstract::look_head()
{
	IKinematics *pK = smart_cast<IKinematics*>(object->Visual());
	Fmatrix bone_transform;
	bone_transform = pK->LL_GetTransform(pK->LL_BoneID("bip01_head"));	

	Fmatrix global_transform;
	global_transform.mul_43(object->XFORM(),bone_transform);

	object->CControlledActor::look_point	(global_transform.c);
}

#undef TEMPLATE_SPECIALIZATION
#undef CStateZombieChokeExecuteAbstract

