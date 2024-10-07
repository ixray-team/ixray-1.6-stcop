#include "StdAfx.h"

#include "../dog/dog.h"

#include "../states/state_custom_action.h"
#include "group_state_custom.h"

CStateCustomGroup::CStateCustomGroup(CBaseMonster* object) : inherited(object)
{
	m_pDog = smart_cast<CDogBase*>(object);

	add_state(eStateCustom, new CStateMonsterCustomAction(object));
}

CStateCustomGroup::~CStateCustomGroup()
{

}

void CStateCustomGroup::execute()
{
	// check alife control
	select_state(eStateCustom);
	m_pDog->start_animation();

	get_state_current()->execute();
	prev_substate = current_substate;
}

void CStateCustomGroup::setup_substates()
{
	state_ptr state = get_state_current();
	if (current_substate == eStateCustom) {
		SStateDataAction data;

		data.action = ACT_STAND_IDLE;
		data.time_out = 0;			// do not use time out
		switch (m_pDog->get_number_animation())
		{
		case u32(5):
			data.sound_type = MonsterSound::eMonsterSoundSteal;
			break;
		case u32(6):
			data.sound_type = MonsterSound::eMonsterSoundThreaten;
			break;
		default:
			data.sound_type = MonsterSound::eMonsterSoundIdle;
			break;
		}
		data.sound_delay = object->db().m_dwEatSndDelay;
		state->fill_data_with(&data, sizeof(SStateDataAction));

		return;
	}
}
