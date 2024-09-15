#pragma once


CStateMonsterCustomAction::CStateMonsterCustomAction(CBaseMonster *obj) : inherited(obj, &data)
{
}


CStateMonsterCustomAction::~CStateMonsterCustomAction()
{
}



void CStateMonsterCustomAction::execute()
{
	this->object->anim().m_tAction		= data.action;
	this->object->anim().SetSpecParams(data.spec_params);

	if (data.sound_type != u32(-1)) {
		if (data.sound_delay != u32(-1))
			this->object->sound().play(data.sound_type, 0,0,data.sound_delay);
		else 
			this->object->sound().play(data.sound_type);
	}

}


bool CStateMonsterCustomAction::check_completion()
{	
	if (data.time_out) {
		if (this->time_state_started + data.time_out < time()) return true;
	}
	
	return false;
}
