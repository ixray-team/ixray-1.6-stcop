#include "stdafx.h"
#include "../../tushkano/tushkano.h" // base class
#include "beetle.h"
#include "beetle_state_manager.h"

#include "../../control_animation_base.h"
#include "../../control_direction_base.h"
#include "../../control_movement_base.h"
#include "../../control_path_builder_base.h"

#include "../../states/monster_state_rest.h"
#include "../../states/monster_state_eat.h"
#include "../../states/monster_state_attack.h"
#include "../../states/monster_state_panic.h"
#include "../../states/monster_state_hear_danger_sound.h"
#include "../../states/monster_state_hitted.h"
#include "../../states/monster_state_controlled.h"
#include "../../states/monster_state_help_sound.h"

#include "../../../../entitycondition.h"

CStateManagerBeetle::CStateManagerBeetle(CBeetle* obj) : inherited(obj) {

}

CStateManagerBeetle::~CStateManagerBeetle() {

}