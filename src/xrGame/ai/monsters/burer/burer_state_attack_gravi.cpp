#include "stdafx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "sound_player.h"

#include "burer.h"
#include "burer_state_attack_gravi.h"

#include "../states/monster_state_attack_on_run.h"

CStateBurerAttackGravi::CStateBurerAttackGravi(CBaseMonster* object) : inherited(object)
{
	m_next_gravi_allowed_tick = 0;
	m_anim_end_tick = 0;
	pBurerBase = smart_cast<CBurerBase*>(object);

	m_action = {};
	m_time_gravi_started = {};
}

CStateBurerAttackGravi::~CStateBurerAttackGravi()
{

}

void CStateBurerAttackGravi::initialize()
{
	inherited::initialize();
	m_action = ACTION_GRAVI_STARTED;
	m_time_gravi_started = 0;
	m_anim_end_tick = 0;
	m_next_gravi_allowed_tick = current_time() + pBurerBase->m_gravi.cooldown;
	pBurerBase->set_force_gravi_attack(false);
	pBurerBase->set_script_capture(false);
}

void CStateBurerAttackGravi::execute()
{
	switch (m_action)
	{
	case ACTION_GRAVI_STARTED:
		ExecuteGraviStart();
		break;

	case ACTION_GRAVI_CONTINUE:
		ExecuteGraviContinue();
		break;

	case ACTION_GRAVI_FIRE:
		ExecuteGraviFire();
		m_action = ACTION_WAIT_ANIM_END;
		break;

	case ACTION_WAIT_ANIM_END:
		if (current_time() > m_anim_end_tick)
		{
			m_action = ACTION_COMPLETED;
		}

	case ACTION_COMPLETED:
		break;
	}

	pBurerBase->face_enemy();

	if (current_time() < m_anim_end_tick)
	{
		object->anim().set_override_animation(eAnimGraviFire);
	}

	object->set_action(ACT_STAND_IDLE);
}

void CStateBurerAttackGravi::finalize()
{
	inherited::finalize();
	object->set_script_capture(true);
}

void CStateBurerAttackGravi::critical_finalize()
{
	inherited::critical_finalize();

	pBurerBase->StopGraviPrepare();
	object->set_script_capture(false);
}

bool CStateBurerAttackGravi::check_start_conditions()
{
	// обработать объекты
	if (pBurerBase->get_force_gravi_attack()) return true;
	float dist = object->Position().distance_to(object->EnemyMan.get_enemy()->Position());
	if (current_time() < m_next_gravi_allowed_tick) return false;
	if (dist < pBurerBase->m_gravi.min_dist) return false;
	if (dist > pBurerBase->m_gravi.max_dist) return false;
	if (!object->EnemyMan.see_enemy_now()) return false;
	if (!object->control().direction().is_face_target(object->EnemyMan.get_enemy(), deg(45))) return false;

	return								true;
}

bool CStateBurerAttackGravi::check_completion()
{
	return								m_action == ACTION_COMPLETED;
}

//////////////////////////////////////////////////////////////////////////

void CStateBurerAttackGravi::ExecuteGraviStart()
{
	float const time = object->anim().get_animation_length(eAnimGraviFire, 0);
	m_anim_end_tick = current_time() + TTime(time * 1000);
	m_action = ACTION_GRAVI_CONTINUE;
	m_time_gravi_started = Device.dwTimeGlobal;
	pBurerBase->StartGraviPrepare();
}

void CStateBurerAttackGravi::ExecuteGraviContinue()
{
	// проверить на грави удар
	float dist = object->Position().distance_to
	(object->EnemyMan.get_enemy()->Position());

	float time_to_hold = (abs(dist - pBurerBase->m_gravi.min_dist) / pBurerBase->m_gravi.min_dist);
	clamp(time_to_hold, 0.f, 1.f);
	time_to_hold *= float(pBurerBase->m_gravi.time_to_hold);

	if (m_time_gravi_started + u32(time_to_hold) < Device.dwTimeGlobal)
	{
		m_action = ACTION_GRAVI_FIRE;
	}
}

void CStateBurerAttackGravi::ExecuteGraviFire()
{
	Fvector from_pos{};
	Fvector target_pos{};
	from_pos = object->Position();
	from_pos.y += 0.5f;
	target_pos = object->EnemyMan.get_enemy()->Position();
	target_pos.y += 0.5f;

	pBurerBase->m_gravi_object.activate(object->EnemyMan.get_enemy(), from_pos, target_pos);

	pBurerBase->StopGraviPrepare();
	object->sound().play(CBurerBase::eMonsterSoundGraviAttack);
}
