#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "state_look_point.h"

#include "sound_player.h"

CStateMonsterLookToPoint::CStateMonsterLookToPoint(CBaseMonster* obj) : inherited(obj, &data)
{
}

CStateMonsterLookToPoint::~CStateMonsterLookToPoint()
{
}

void CStateMonsterLookToPoint::initialize()
{
	inherited::initialize();
}

void CStateMonsterLookToPoint::execute()
{
	object->anim().m_tAction = data.action.action;
	object->anim().SetSpecParams(data.action.spec_params);
	object->dir().face_target(data.point, data.face_delay);

	if (data.action.sound_type != u32(-1)) {
		if (data.action.sound_delay != u32(-1))
			object->sound().play(data.action.sound_type, 0, 0, data.action.sound_delay);
		else
			object->sound().play(data.action.sound_type);
	}

}


bool CStateMonsterLookToPoint::check_completion() {
	if (data.action.time_out != 0) {
		if (time_state_started + data.action.time_out < Device.dwTimeGlobal) {
			return true;
		}
	}
	else if (!object->control().direction().is_turning()) {
		return true;
	}
	return false;
}
