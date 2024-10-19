#include "stdafx.h"
#include "pseudodog_psy.h"
#include "pseudodog_psy_state_manager.h"
#include "../../../actor.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"
#include "../control_animation_base.h"
#include "../../../ai_object_location.h"
#include "../../../sound_player.h"
#include "../../../level_graph.h"

#include "pseudodog_psy_state_psy_attack.h"

CStateManagerPsyDog::CStateManagerPsyDog(CPseudoDogBase *monster) : inherited(monster)
{

}

void CStateManagerPsyDog::execute()
{
	inherited::execute();
}

