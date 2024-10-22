#include "StdAfx.h"
#include "controller_psy_aura_effector.h"
#include "controller_psy_aura.h"
#include "controller.h"
#include "../../../actor.h"
#include "../../../level.h"
#include "../../../CameraEffector.h"
#include "../../../ActorEffector.h"

CPPEffectorControllerAura::CPPEffectorControllerAura(const SPPInfo& ppi, u32 time_to_fade, const ref_sound& snd_left, const ref_sound& snd_right)
	: inherited(ppi)
{
	m_time_to_fade = time_to_fade;
	m_effector_state = eStateFadeIn;
	m_time_state_started = Device.dwTimeGlobal;

	m_snd_left.clone(snd_left, st_Effect, sg_SourceType);
	m_snd_right.clone(snd_right, st_Effect, sg_SourceType);

	m_snd_left.play_at_pos(Actor(), Fvector().set(-1.f, 0.f, 1.f), sm_Looped | sm_2D);
	m_snd_right.play_at_pos(Actor(), Fvector().set(-1.f, 0.f, 1.f), sm_Looped | sm_2D);

}

CPPEffectorControllerAura::~CPPEffectorControllerAura()
{

}

void CPPEffectorControllerAura::switch_off()
{
	m_effector_state = eStateFadeOut;
	m_time_state_started = Device.dwTimeGlobal;
}

BOOL CPPEffectorControllerAura::update()
{
	// update factor
	if (m_effector_state == eStatePermanent) {
		m_factor = 1.f;
	}
	else {
		m_factor = float(Device.dwTimeGlobal - m_time_state_started) / float(m_time_to_fade);
		if (m_effector_state == eStateFadeOut) m_factor = 1 - m_factor;

		if (m_factor > 1) {
			m_effector_state = eStatePermanent;
			m_factor = 1.f;
		}
		else if (m_factor < 0) {
			if (m_snd_left._feedback())		m_snd_left.stop();
			if (m_snd_right._feedback())	m_snd_right.stop();

			return FALSE;
		}
	}

	// start new or play again?
	if (!m_snd_left._feedback() && !m_snd_right._feedback()) {
		m_snd_left.play_at_pos(Actor(), Fvector().set(-1.f, 0.f, 1.f), sm_Looped | sm_2D);
		m_snd_right.play_at_pos(Actor(), Fvector().set(-1.f, 0.f, 1.f), sm_Looped | sm_2D);
	}

	if (m_snd_left._feedback())		m_snd_left.set_volume(m_factor);
	if (m_snd_right._feedback())	m_snd_right.set_volume(m_factor);

	return TRUE;
}