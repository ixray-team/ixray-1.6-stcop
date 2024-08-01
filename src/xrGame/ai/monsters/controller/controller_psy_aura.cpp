#include "stdafx.h"
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
	m_snd_right.play_at_pos(Actor(), Fvector().set(0.f, -1.f, 1.f), sm_Looped | sm_2D);
}

CPPEffectorControllerAura::~CPPEffectorControllerAura()
{
	m_snd_left.destroy();
	m_snd_right.destroy();
}

void CPPEffectorControllerAura::switch_off()
{
	m_effector_state = eStateFadeOut;
	m_time_state_started = Device.dwTimeGlobal;
}

void CPPEffectorControllerAura::stop_snds()
{
	m_time_to_fade = 0;

	if (m_snd_left._feedback())
		m_snd_left.stop();

	if (m_snd_right._feedback())
		m_snd_right.stop();
}

BOOL CPPEffectorControllerAura::update()
{	// update factor
	if (m_time_to_fade != 0.0f)
	{
		if (m_effector_state == eStatePermanent)
		{
			m_factor = 1.f;
		}
		else
		{
			m_factor = float(Device.dwTimeGlobal - m_time_state_started) / float(m_time_to_fade);
			if (m_effector_state == eStateFadeOut) m_factor = 1 - m_factor;

			if (m_factor > 1)
			{
				m_effector_state = eStatePermanent;
				m_factor = 1.f;
			}
			else if (m_factor < 0)
			{
				if (m_snd_left._feedback())		m_snd_left.stop();
				if (m_snd_right._feedback())	m_snd_right.stop();

				return FALSE;
			}
		}

		// start new or play again?
		if (!m_snd_left._feedback() && !m_snd_right._feedback())
		{
			m_snd_left.play_at_pos(Actor(), Fvector().set(-1.f, 0.f, 1.f), sm_Looped | sm_2D);
			m_snd_right.play_at_pos(Actor(), Fvector().set(0.f, -1.f, 1.f), sm_Looped | sm_2D);
		}

		if (m_snd_left._feedback())
			m_snd_left.set_volume(m_factor);

		if (m_snd_right._feedback())
			m_snd_right.set_volume(m_factor);

		return TRUE;
	}
	else
		return FALSE;
}

CControllerAura::~CControllerAura() {}

void CControllerAura::load(LPCSTR section)
{
	inherited::load(pSettings->r_string(section, "aura_effector"));

	aura_sound.left.create(pSettings->r_string(section, "PsyAura_SoundLeftPath"), st_Effect, sg_SourceType);
	aura_sound.right.create(pSettings->r_string(section, "PsyAura_SoundRightPath"), st_Effect, sg_SourceType);

	aura_radius_min = READ_IF_EXISTS(pSettings, r_float, section, "PsyAura_Radius_min", 5.f);
	aura_radius_max = READ_IF_EXISTS(pSettings, r_float, section, "PsyAura_Radius_max", 90.f);
	aura_radius_max_y = READ_IF_EXISTS(pSettings, r_float, section, "PsyAura_Radius_max_y", 3.f);
	aura_damage = READ_IF_EXISTS(pSettings, r_float, section, "PsyAura_Damage", 0.02f);

	aura_regular_effector_sect = pSettings->r_string(section, "aura_regular_effector_sect");
	aura_hit_effector_sect = pSettings->r_string(section, "aura_hit_effector_sect");
	aura_effector_max_factor = READ_IF_EXISTS(pSettings, r_float, section, "aura_effector_max_factor", 0.30f);

	m_time_fake_aura = 0;

	Ivector2 fakeAuraDefaultDuration{ 5000, 13000 };
	Ivector2 fakeAuraDefaultDelay{ 5000, 10000 };
	m_time_fake_aura_duration = READ_IF_EXISTS(pSettings, r_ivector2, section, "PsyAura_Fake_Duration", fakeAuraDefaultDuration);
	m_time_fake_aura_delay = READ_IF_EXISTS(pSettings, r_ivector2, section, "PsyAura_Fake_Delay", fakeAuraDefaultDelay);
	m_time_started = 0;
	m_hit_state = eNone;

	m_pmt_hit_delay = READ_IF_EXISTS(pSettings, r_u32, section, "PsyAura_HitDelay", 1000);
	m_pmt_pp_hit_delay = READ_IF_EXISTS(pSettings, r_u32, section, "PsyAura_PPHitDelay", 300);

	current_effector_strength = 0.f;
}

void CControllerAura::update_schedule()
{
	if (!m_object->g_Alive())
		return;

	float dist_to_actor = Actor()->Position().distance_to(m_object->Position());
	float height_distance = m_object->Position().y - Actor()->Position().y;

	if ((dist_to_actor < aura_radius_max || dist_to_actor < aura_radius_min) && (abs(height_distance) < aura_radius_max_y))
	{
		//if actor is in regular effect range
		if ((dist_to_actor < aura_radius_max))
		{
			// first time? 
			if (m_time_fake_aura == 0)
			{
				m_time_fake_aura = time() + Random.randI(m_time_fake_aura_delay.x, m_time_fake_aura_delay.y);

				if (m_effector)
				{
					m_effector->switch_off();
					m_effector = nullptr;
				}
			}
			else
			{
				if (m_effector)
				{
					// check to stop
					if (m_time_fake_aura < time())
					{
						m_effector->switch_off();
						m_effector = nullptr;

						m_time_fake_aura = time() + Random.randI(m_time_fake_aura_delay.x, m_time_fake_aura_delay.y);
					}
				}
				else
				{
					// check to start
					if (m_time_fake_aura < time())
					{

						m_effector = new CPPEffectorControllerAura(m_state, m_time_fake_aura_duration.x, aura_sound.left, aura_sound.right);
						Actor()->Cameras().AddPPEffector(m_effector);
						m_time_fake_aura = time() + Random.randI(m_time_fake_aura_duration.x, m_time_fake_aura_duration.y);
					}
				}
			}
		}

		// if actor is close enough to trigger "close" effects (and aura damage is not 0)
		if (dist_to_actor < aura_radius_min && !fis_zero(aura_damage))
		{
			if (m_hit_state == eNone)
			{
				m_hit_state = eEffectoring;
				m_time_started = time();
			}
		}
		else
		{
			m_hit_state = eNone;
		}
	}
	//if actor is out of aura range, then remove effectors
	else
	{
		current_effector_strength = 0.f;
		if (m_effector)
		{
			m_effector->switch_off();
			m_effector = nullptr;
		}

		m_hit_state = eNone;
	}

	// Separately add camera shaking effector
	if (aura_effector_max_factor > 0.f)
	{
		auto ce = Actor()->Cameras().GetCamEffector((ECamEffectorType)effControllerAura2);
		if (active())
		{
			if (!ce)
			{
				AddEffector(Actor(), effControllerAura2, aura_regular_effector_sect, GET_KOEFF_FUNC(this, &CControllerAura::get_effector_strength));
				// calculate effector strength based on distance at the moment it started (to prevent camera jumps)
				if (aura_effector_max_factor > 0.f)
				{
					current_effector_strength = aura_effector_max_factor * (1.f - (dist_to_actor - aura_radius_min) / (aura_radius_max - aura_radius_min));
				}
			}
		}
		else
		{
			if (ce)
				RemoveEffector(Actor(), effControllerAura2);
		}
	}
}

void CControllerAura::update_frame()
{
	if (m_hit_state == eNone)
		return;

	//Add effector that happens when actor gets very close and do damage to actor
	switch (m_hit_state)
	{
	case eEffectoring:
		if (m_time_started + m_pmt_hit_delay < time())
		{
			// launch effector
			AddEffector(Actor(), effControllerAura, aura_hit_effector_sect);
			m_hit_state = eHit;
			m_time_started = time();
		}
		break;
	case eHit:
		if (m_time_started + m_pmt_pp_hit_delay < time())
		{
			m_object->Hit_Psy(Actor(), aura_damage);
			m_hit_state = eEffectoring;
		}
		break;
	}
}

float CControllerAura::get_effector_strength()
{
	// make smooth transition out of the effect
	u32 effectorFadeTime = m_time_fake_aura_duration.x / 2;
	if (active() && time() > (m_time_fake_aura - effectorFadeTime))
		return time() < m_time_fake_aura ? current_effector_strength * (m_time_fake_aura - time()) / effectorFadeTime : 0.f;

	return current_effector_strength;
}

void CControllerAura::on_death()
{
	if (m_effector)
	{
		CEffectorCam* ce = Actor()->Cameras().GetCamEffector((ECamEffectorType)effControllerAura2);
		if (ce)
			RemoveEffector(Actor(), effControllerAura2);

		m_effector->switch_off();
		m_effector = nullptr;
		m_hit_state = eNone;
	}
}

void CControllerAura::on_destroy()
{
	if (m_effector)
	{
		CEffectorCam* ce = Actor()->Cameras().GetCamEffector((ECamEffectorType)effControllerAura2);
		if (ce)
			RemoveEffector(Actor(), effControllerAura2);

		m_effector->stop_snds();

		m_effector = nullptr;

		m_hit_state = eNone;
	}
}