#include "StdAfx.h"
#include "controller.h"
#include "controller_state_attack_hide.h"

#include "ai_object_location.h"

#include "controller_animation.h"
#include "controller_direction.h"

#include "../../../ai_space.h"
#include "../monster_cover_manager.h"
#include "../../../cover_point.h"
#include "../../../level.h"
#include "../../../level_debug.h"

#include "sound_player.h"

CStateControlHide::CStateControlHide(CBaseMonster* obj) : inherited(obj)
{
	m_pController = smart_cast<CControllerBase*>(obj);
}

void CStateControlHide::initialize()
{
	inherited::initialize();

	m_cover_reached = false;
	select_target_point();
	this->object->path().prepare_builder();

}

void CStateControlHide::execute()
{
	if (m_state_fast_run) {
		if (target.position.distance_to(this->object->Position()) < 5.f) {
			m_state_fast_run = false;
			this->m_pController->set_mental_state(CControllerBase::eStateDanger);
		}
	}

	this->object->set_action(ACT_RUN);

	this->object->path().set_target_point(target.position, target.node);
	this->object->path().set_rebuild_time(0);
	this->object->path().set_distance_to_end(0.f);
	this->object->path().set_use_covers(false);

	this->m_pController->anim().accel_activate(eAT_Aggressive);
	this->m_pController->anim().accel_set_braking(false);

	this->m_pController->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, this->object->db().m_dwAttackSndDelay);

	if (this->object->HitMemory.get_last_hit_time() > this->object->EnemyMan.get_enemy_time_last_seen()) {
		Fvector pos;
		pos.mad(this->object->Position(), this->object->HitMemory.get_last_hit_dir(), 5.f);
		pos.y += 1.5f;
		this->m_pController->custom_dir().head_look_point(pos);
	}
	else
		this->m_pController->custom_dir().head_look_point(this->object->EnemyMan.get_enemy_position());

	this->m_pController->custom_anim().set_body_state(CControllerAnimation::eTorsoRun, CControllerAnimation::eLegsTypeRun);
}

bool CStateControlHide::check_start_conditions()
{
	return true;
}

void CStateControlHide::finalize()
{
	inherited::finalize();
	this->m_pController->set_mental_state(CControllerBase::eStateDanger);
}


void CStateControlHide::critical_finalize()
{
	inherited::finalize();
	this->m_pController->set_mental_state(CControllerBase::eStateDanger);
}


bool CStateControlHide::check_completion()
{
	return ((this->object->ai_location().level_vertex_id() == target.node) && !this->object->control().path_builder().is_moving_on_path());
}


void CStateControlHide::select_target_point()
{
#ifdef DEBUG
	DBG().level_info(this).clear();
#endif

	const CCoverPoint* point = this->object->CoverMan->find_cover(this->object->EnemyMan.get_enemy_position(), 10.f, 30.f);
	if (point) {
		target.node = point->level_vertex_id();
		target.position = point->position();
	}
	else {
		target.node = 0;
		target.position = ai().level_graph().vertex_position(target.node);
	}

	m_state_fast_run = (target.position.distance_to(this->object->Position()) > 20.f);
	if (m_state_fast_run && (Random.randI(100) < 50))
		this->m_pController->set_mental_state(CControllerBase::eStateIdle);
}
