#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "state_custom_action_look.h"

#include "sound_player.h"

CStateMonsterCustomActionLook::CStateMonsterCustomActionLook(CBaseMonster* obj) : inherited(obj, &data)
{
}


CStateMonsterCustomActionLook::~CStateMonsterCustomActionLook()
{
}



void CStateMonsterCustomActionLook::execute()
{
	object->set_action(data.action);
	object->anim().SetSpecParams(data.spec_params);
	object->dir().face_target(data.point);

	if (data.sound_type != u32(-1)) {
		if (data.sound_delay != u32(-1))
			object->sound().play(data.sound_type, 0, 0, data.sound_delay);
		else
			object->sound().play(data.sound_type);
	}

}


bool CStateMonsterCustomActionLook::check_completion()
{
	if (data.time_out) {
		if (time_state_started + data.time_out < time()) return true;
	}

	return false;
}
