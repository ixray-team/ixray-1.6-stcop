#include "StdAfx.h"
#include "controller.h"

#include "controller_state_attack_hide_lite.h"

#include "ai_object_location.h"

#include "controller_animation.h"
#include "controller_direction.h"

#include "../../../ai_space.h"
#include "../monster_cover_manager.h"
#include "../../../cover_point.h"
#include "../../../level.h"
#include "../../../level_debug.h"

#include "sound_player.h"

CStateControlHideLite::CStateControlHideLite(CBaseMonster* object) : inherited(object)
{
	pControllerBase = smart_cast<CControllerBase*>(object);

	target = {};
	m_time_finished = {};
}

CStateControlHideLite::~CStateControlHideLite()
{

}

void CStateControlHideLite::initialize()
{
	inherited::initialize();

	select_target_point();
	object->path().prepare_builder();
}

void CStateControlHideLite::execute()
{
	object->path().set_target_point(target.position, target.node);
	object->path().set_rebuild_time(0);
	object->path().set_distance_to_end(0.f);
	object->path().set_use_covers(true);

	object->anim().accel_activate(eAT_Aggressive);
	object->anim().accel_set_braking(false);

	object->sound().play(MonsterSound::eMonsterSoundAggressive, 0, 0, object->db().m_dwAttackSndDelay);
	pControllerBase->custom_dir().head_look_point(object->EnemyMan.get_enemy_position());

	pControllerBase->custom_anim().set_body_state(CControllerAnimation::eTorsoRun, CControllerAnimation::eLegsTypeRun);
}

bool CStateControlHideLite::check_start_conditions()
{
	return true;
}

void CStateControlHideLite::reinit()
{
	inherited::reinit();
	m_time_finished = 0;
}

void CStateControlHideLite::finalize()
{
	inherited::finalize();
	m_time_finished = Device.dwTimeGlobal;
}

bool CStateControlHideLite::check_completion()
{
	if ((object->ai_location().level_vertex_id() == target.node) &&
		!object->control().path_builder().is_moving_on_path()) return true;

	return (!object->EnemyMan.see_enemy_now());
}

void CStateControlHideLite::select_target_point()
{
#ifdef DEBUG
	DBG().level_info(this).clear();
#endif

	const CCoverPoint* point = object->CoverMan->find_cover(object->EnemyMan.get_enemy_position(), 10.f, 30.f);
	//VERIFY(point);
	if (point) {
		target.node = point->level_vertex_id();
		target.position = point->position();
	}
	else {
		target.node = 0;
		target.position = ai().level_graph().vertex_position(target.node);
	}
}
