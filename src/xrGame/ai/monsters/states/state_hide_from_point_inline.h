#pragma once

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#define DIST_TO_PATH_END		1.5f


void CStateMonsterHideFromPoint::initialize()
{
	inherited::initialize();

	this->object->path().prepare_builder();
}


void CStateMonsterHideFromPoint::execute()
{
	this->object->set_action									(data.action.action);
	this->object->anim().SetSpecParams						(data.action.spec_params);
	
	this->object->path().set_retreat_from_point	(data.point);
	this->object->path().set_generic_parameters	();

	if (data.accelerated) {
		this->object->anim().accel_activate	(EAccelType(data.accel_type));
		this->object->anim().accel_set_braking (data.braking);
	}

	if (data.action.sound_type != u32(-1)) {
		this->object->set_state_sound(data.action.sound_type, data.action.sound_delay == u32(-1));
	}
}


bool CStateMonsterHideFromPoint::check_completion()
{	
	if (data.action.time_out !=0) {
		if (this->time_state_started + data.action.time_out < Device.dwTimeGlobal)
			return true;
	}
		
	return false;
}

#undef DIST_TO_PATH_END
