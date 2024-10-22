#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "poltergeist_state_rest.h"

CPoltergeistStateRest::CPoltergeistStateRest(CBaseMonster* object) : inherited(object) 
{

}

CPoltergeistStateRest::~CPoltergeistStateRest()
{

}

void CPoltergeistStateRest::execute()
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

		if (move_to_restrictor)
			select_state(eStateCustomMoveToRestrictor);
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
			else					select_state(eStateRest_WalkGraphPoint);
		}
	}

	get_state_current()->execute();
	prev_substate = current_substate;
}

