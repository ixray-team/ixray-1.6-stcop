#include "stdafx.h"

#include "burer.h"
#include "burer_state_attack_antiaim.h"

#include "../anti_aim_ability.h"

CStateBurerAntiAim::CStateBurerAntiAim(CBaseMonster* object) : inherited(object)
{
	m_allow_anti_aim = false;
	pBurerBase = smart_cast<CBurerBase*>(object);

	m_last_shield_started = {};
	m_next_particle_allowed = {};
	m_shield_start_anim_length_sec = {};
	m_started = {};
}

CStateBurerAntiAim::~CStateBurerAntiAim()
{

}

void   CStateBurerAntiAim::initialize()
{
	inherited::initialize();
	m_allow_anti_aim = true;
	object->control().activate(ControlCom::eAntiAim);
	m_allow_anti_aim = false;

	VERIFY(object->get_anti_aim()->is_active());
}

void   CStateBurerAntiAim::execute()
{
	pBurerBase->face_enemy();
	object->set_action(ACT_STAND_IDLE);
}

void   CStateBurerAntiAim::finalize()
{
	inherited::finalize();
}

void   CStateBurerAntiAim::critical_finalize()
{
	inherited::critical_finalize();
}

bool   CStateBurerAntiAim::check_start_conditions()
{
	return									object->get_anti_aim()->check_start_condition();
}

bool   CStateBurerAntiAim::check_completion()
{
	if (!object->get_anti_aim()->is_active())
	{
		return								true;
	}

	return									false;
}
