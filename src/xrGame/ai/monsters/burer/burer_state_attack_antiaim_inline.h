#pragma once

#include "../anti_aim_ability.h"

CStateBurerAntiAim::CStateBurerAntiAim (CBaseMonster *obj) : inherited(obj)
{
	m_allow_anti_aim					=	false;
}

void   CStateBurerAntiAim::initialize()
{
	inherited::initialize					();
	m_allow_anti_aim					=	true;
	this->object->control().activate				(ControlCom::eAntiAim);
	m_allow_anti_aim					=	false;

	VERIFY									(this->object->get_anti_aim()->is_active() );
}

void   CStateBurerAntiAim::execute()
{
	this->object->face_enemy						();
	this->object->set_action						(ACT_STAND_IDLE);
}

void   CStateBurerAntiAim::finalize()
{
	inherited::finalize();
}

void   CStateBurerAntiAim::critical_finalize()
{
	inherited::critical_finalize			();
}

bool   CStateBurerAntiAim::check_start_conditions()
{
	return									this->object->get_anti_aim()->check_start_condition();
}

bool   CStateBurerAntiAim::check_completion()
{
	if ( !this->object->get_anti_aim()->is_active() )
	{
		return								true;
	}

	return									false;
}
