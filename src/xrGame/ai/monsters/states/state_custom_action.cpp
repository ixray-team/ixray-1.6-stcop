#include "StdAfx.h"
#include "state_custom_action.h"

#include "sound_player.h"

CStateMonsterCustomAction::CStateMonsterCustomAction(CBaseMonster* obj) : inherited(obj, &data)
{
}


CStateMonsterCustomAction::~CStateMonsterCustomAction()
{
}



void CStateMonsterCustomAction::execute()
{
	object->anim().m_tAction = data.action;
	object->anim().SetSpecParams(data.spec_params);

	if (data.sound_type != u32(-1)) {
		if (data.sound_delay != u32(-1))
			object->sound().play(data.sound_type, 0, 0, data.sound_delay);
		else
			object->sound().play(data.sound_type);
	}

}


bool CStateMonsterCustomAction::check_completion()
{
	if (data.time_out) {
		if (time_state_started + data.time_out < time()) return true;
	}

	return false;
}
