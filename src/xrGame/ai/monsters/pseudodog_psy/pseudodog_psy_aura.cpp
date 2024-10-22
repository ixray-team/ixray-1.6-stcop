#include "stdafx.h"
#include "../pseudodog/pseudodog.h"
#include "../pseudodog_phantom/pseudodog_phantom.h"
#include "pseudodog_psy_aura.h"
#include "pseudodog_psy.h"
#include "../../../actor.h"
#include "../../../ActorEffector.h"
#include "../../../actor_memory.h"
#include "../../../visual_memory_manager.h"
#include "../../../level.h"

CPPEffectorPsyDogAura::CPPEffectorPsyDogAura(const SPPInfo &ppi, u32 time_to_fade)
: inherited(ppi)
{
	m_time_to_fade			= time_to_fade;
	m_effector_state		= eStateFadeIn;
	m_time_state_started	= Device.dwTimeGlobal;

}

CPPEffectorPsyDogAura::~CPPEffectorPsyDogAura()
{

}

void CPPEffectorPsyDogAura::switch_off()
{
	m_effector_state		= eStateFadeOut;		
	m_time_state_started	= Device.dwTimeGlobal;
}

BOOL CPPEffectorPsyDogAura::update()
{
	if (m_effector_state == eStatePermanent) {
		m_factor = 1.f;
	} else {
		m_factor = float(Device.dwTimeGlobal - m_time_state_started) / float(m_time_to_fade);
		if (m_effector_state == eStateFadeOut) m_factor = 1 - m_factor;

		if (m_factor > 1) {
			m_effector_state	= eStatePermanent;
			m_factor			= 1.f;
		} else if (m_factor < 0) {
			return FALSE;
		}
	}
	return TRUE;
}


