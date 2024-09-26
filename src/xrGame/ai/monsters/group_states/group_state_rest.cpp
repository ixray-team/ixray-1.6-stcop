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

CStateGroupRest::CStateGroupRest(CBaseMonster* obj) : inherited(obj)
{
	m_pDog = smart_cast<CustomDog*>(obj);

	this->add_state(eStateRest_Sleep, xr_new<CStateMonsterRestSleep>(obj));
	this->add_state(eStateCustomMoveToRestrictor, xr_new<CStateMonsterMoveToRestrictor>(obj));
	this->add_state(eStateRest_MoveToHomePoint, xr_new<CStateMonsterRestMoveToHomePoint>(obj));
	this->add_state(eStateSmartTerrainTask, xr_new<CStateMonsterSmartTerrainTask>(obj));
	this->add_state(eStateRest_Idle, xr_new<CStateGroupRestIdle>(obj));
	this->add_state(eStateCustom, xr_new<CStateCustomGroup>(obj));
}



CStateGroupRest::~CStateGroupRest()
{
}


void CStateGroupRest::initialize()
{
	inherited::initialize();
	time_for_sleep = 0;
	time_for_life = time() + this->m_pDog->m_min_life_time + Random.randI(10) * this->m_pDog->m_min_life_time;
	this->object->anomaly_detector().activate();
}


void CStateGroupRest::finalize()
{
	inherited::finalize();

	this->object->anomaly_detector().deactivate();
}


void CStateGroupRest::critical_finalize()
{
	inherited::critical_finalize();

	this->object->anomaly_detector().deactivate();
}


void CStateGroupRest::execute()
{
	// check alife control

	bool captured_by_smart_terrain = false;

	if (this->prev_substate == eStateSmartTerrainTask) {
		if (!this->get_state(eStateSmartTerrainTask)->check_completion())
			captured_by_smart_terrain = true;
	}
	else if (this->get_state(eStateSmartTerrainTask)->check_start_conditions())
		captured_by_smart_terrain = true;

	if (captured_by_smart_terrain) this->select_state(eStateSmartTerrainTask);
	else {
		// check restrictions
		bool move_to_restrictor = false;

		if (this->prev_substate == eStateCustomMoveToRestrictor) {
			if (!this->get_state(eStateCustomMoveToRestrictor)->check_completion())
				move_to_restrictor = true;
		}
		else if (this->get_state(eStateCustomMoveToRestrictor)->check_start_conditions())
			move_to_restrictor = true;

		if (move_to_restrictor) this->select_state(eStateCustomMoveToRestrictor);
		else {
			// check home point
			bool move_to_home_point = false;

			if (this->prev_substate == eStateRest_MoveToHomePoint) {
				if (!this->get_state(eStateRest_MoveToHomePoint)->check_completion())
					move_to_home_point = true;
			}
			else if (this->get_state(eStateRest_MoveToHomePoint)->check_start_conditions())
				move_to_home_point = true;

			if (move_to_home_point) this->select_state(eStateRest_MoveToHomePoint);
			else {
				// check squad behaviour
				if (this->m_pDog->saved_state == eStateRest_Sleep)
				{
					switch (this->m_pDog->get_number_animation())
					{
					case u32(8):
						this->m_pDog->set_current_animation(13);
						break;
					case u32(14):
						this->m_pDog->set_current_animation(12);
						break;
					case u32(12):
						this->m_pDog->set_current_animation(7);
						this->m_pDog->saved_state = u32(-1);
						break;
					default:
						break;
					}
					if (this->m_pDog->b_state_check)
					{
						this->m_pDog->b_state_check = false;
						this->select_state(eStateCustom);
						this->get_state_current()->execute();
						this->prev_substate = this->current_substate;
						return;
					}
				}
				if (time() < time_for_sleep && this->m_pDog->saved_state == eStateRest_Sleep && this->m_pDog->get_number_animation() == u32(13))
				{
					this->select_state(eStateRest_Sleep);
					this->get_state_current()->execute();
					this->prev_substate = this->current_substate;
					return;
				}
				bool use_to_do = false;
				if (this->prev_substate == eStateRest_Sleep) {
					if (time() <= time_for_sleep) {
						use_to_do = true;
					}
					else {
						time_for_life = time() + this->m_pDog->m_min_life_time + Random.randI(10) * this->m_pDog->m_min_life_time;
						this->m_pDog->set_current_animation(14);
						this->select_state(eStateCustom);
						this->m_pDog->b_state_check = false;
						this->get_state_current()->execute();
						this->prev_substate = this->current_substate;
						return;
					}
				}
				if (!use_to_do) {
					if (time() > time_for_life && this->object->Home->at_min_home(this->object->Position())) {
						this->m_pDog->set_current_animation(8);
						this->select_state(eStateCustom);
						this->m_pDog->saved_state = eStateRest_Sleep;
						time_for_sleep = time() + this->m_pDog->m_min_sleep_time + Random.randI(5) * this->m_pDog->m_min_sleep_time;
						use_to_do = true;
						this->m_pDog->b_state_check = false;
						this->get_state_current()->execute();
						this->prev_substate = this->current_substate;
						return;
					}
					else {
						if (this->m_pDog->saved_state != eStateRest_Sleep && this->prev_substate == eStateCustom && this->m_pDog->get_number_animation() >= u32(8) && this->m_pDog->get_number_animation() < u32(12))
						{
							this->m_pDog->set_current_animation(this->m_pDog->get_number_animation() + u32(1));
							this->select_state(eStateCustom);
							this->m_pDog->b_state_check = false;
							this->get_state_current()->execute();
							this->prev_substate = this->current_substate;
							return;
						}
						if (this->m_pDog->b_state_check)
						{
							this->select_state(eStateCustom);
							this->m_pDog->b_state_check = false;
						}
						else {
							this->select_state(eStateRest_Idle);
						}
					}
				}
			}
		}
	}

	this->get_state_current()->execute();
	this->prev_substate = this->current_substate;
}