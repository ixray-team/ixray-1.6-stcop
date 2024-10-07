#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "ai_object_location.h"

#include "restricted_object.h"

#include "../dog/dog.h"

#include "../states/monster_state_rest_sleep.h"
#include "../states/state_move_to_restrictor.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"
#include "../monster_home.h"
#include "../anomaly_detector.h"
#include "../states/monster_state_home_point_rest.h"
#include "../states/monster_state_smart_terrain_task.h"
#include "group_state_rest_idle.h"
#include "group_state_custom.h"

#include "group_state_rest.h"

CStateGroupRest::CStateGroupRest(CBaseMonster* object) : inherited(object)
{
	m_pDog = smart_cast<CDogBase*>(object);

	time_for_life = 0;
	time_for_sleep = 0;

	add_state(eStateRest_Sleep, new CStateMonsterRestSleep (object));
	add_state(eStateCustomMoveToRestrictor, new CStateMonsterMoveToRestrictor (object));
	add_state(eStateRest_MoveToHomePoint, new CStateMonsterRestMoveToHomePoint (object));
	add_state(eStateSmartTerrainTask, new CStateMonsterSmartTerrainTask (object));
	add_state(eStateRest_Idle, new CStateGroupRestIdle (object));
	add_state(eStateCustom, new CStateCustomGroup (object));
}

CStateGroupRest::~CStateGroupRest()
{
}

void CStateGroupRest::initialize()
{
	inherited::initialize();
	time_for_sleep = 0;
	time_for_life = time() + m_pDog->m_min_life_time + Random.randI(10) * m_pDog->m_min_life_time;
	object->anomaly_detector().activate();
}


void CStateGroupRest::finalize()
{
	inherited::finalize();

	object->anomaly_detector().deactivate();
}


void CStateGroupRest::critical_finalize()
{
	inherited::critical_finalize();

	object->anomaly_detector().deactivate();
}


void CStateGroupRest::execute()
{
	// check alife control

	bool captured_by_smart_terrain = false;

	if (prev_substate == eStateSmartTerrainTask) {
		if (!get_state(eStateSmartTerrainTask)->check_completion())
			captured_by_smart_terrain = true;
	}
	else if (get_state(eStateSmartTerrainTask)->check_start_conditions())
		captured_by_smart_terrain = true;

	if (captured_by_smart_terrain) select_state(eStateSmartTerrainTask);
	else {
		// check restrictions
		bool move_to_restrictor = false;

		if (prev_substate == eStateCustomMoveToRestrictor) {
			if (!get_state(eStateCustomMoveToRestrictor)->check_completion())
				move_to_restrictor = true;
		}
		else if (get_state(eStateCustomMoveToRestrictor)->check_start_conditions())
			move_to_restrictor = true;

		if (move_to_restrictor) select_state(eStateCustomMoveToRestrictor);
		else {
			// check home point
			bool move_to_home_point = false;

			if (prev_substate == eStateRest_MoveToHomePoint) {
				if (!get_state(eStateRest_MoveToHomePoint)->check_completion())
					move_to_home_point = true;
			}
			else if (get_state(eStateRest_MoveToHomePoint)->check_start_conditions())
				move_to_home_point = true;

			if (move_to_home_point) select_state(eStateRest_MoveToHomePoint);
			else {
				// check squad behaviour
				if (m_pDog->saved_state == eStateRest_Sleep)
				{
					switch (m_pDog->get_number_animation())
					{
					case u32(8):
						m_pDog->set_current_animation(13);
						break;
					case u32(14):
						m_pDog->set_current_animation(12);
						break;
					case u32(12):
						m_pDog->set_current_animation(7);
						m_pDog->saved_state = u32(-1);
						break;
					default:
						break;
					}
					if (m_pDog->b_state_check)
					{
						m_pDog->b_state_check = false;
						select_state(eStateCustom);
						get_state_current()->execute();
						prev_substate = current_substate;
						return;
					}
				}
				if (time() < time_for_sleep && m_pDog->saved_state == eStateRest_Sleep && m_pDog->get_number_animation() == u32(13))
				{
					select_state(eStateRest_Sleep);
					get_state_current()->execute();
					prev_substate = current_substate;
					return;
				}
				bool use_to_do = false;
				if (prev_substate == eStateRest_Sleep) {
					if (time() <= time_for_sleep) {
						use_to_do = true;
					}
					else {
						time_for_life = time() + m_pDog->m_min_life_time + Random.randI(10) * m_pDog->m_min_life_time;
						m_pDog->set_current_animation(14);
						select_state(eStateCustom);
						m_pDog->b_state_check = false;
						get_state_current()->execute();
						prev_substate = current_substate;
						return;
					}
				}
				if (!use_to_do) {
					if (time() > time_for_life && object->Home->at_min_home(object->Position())) {
						m_pDog->set_current_animation(8);
						select_state(eStateCustom);
						m_pDog->saved_state = eStateRest_Sleep;
						time_for_sleep = time() + m_pDog->m_min_sleep_time + Random.randI(5) * m_pDog->m_min_sleep_time;
						use_to_do = true;
						m_pDog->b_state_check = false;
						get_state_current()->execute();
						prev_substate = current_substate;
						return;
					}
					else {
						if (m_pDog->saved_state != eStateRest_Sleep && prev_substate == eStateCustom && m_pDog->get_number_animation() >= u32(8) && m_pDog->get_number_animation() < u32(12))
						{
							m_pDog->set_current_animation(m_pDog->get_number_animation() + u32(1));
							select_state(eStateCustom);
							m_pDog->b_state_check = false;
							get_state_current()->execute();
							prev_substate = current_substate;
							return;
						}
						if (m_pDog->b_state_check)
						{
							select_state(eStateCustom);
							m_pDog->b_state_check = false;
						}
						else {
							select_state(eStateRest_Idle);
						}
					}
				}
			}
		}
	}

	get_state_current()->execute();
	prev_substate = current_substate;
}