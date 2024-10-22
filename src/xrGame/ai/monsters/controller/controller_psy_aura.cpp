#include "stdafx.h"
#include "controller_psy_aura_effector.h"
#include "controller_psy_aura.h"
#include "controller.h"
#include "../../../actor.h"
#include "../../../level.h"
#include "../../../CameraEffector.h"
#include "../../../ActorEffector.h"

CControllerAura::CControllerAura(CControllerBase* object) : m_object(object) 
{
	m_object = {};
	m_time_last_update = {};

	aura_sound = {};
	aura_radius = {};

	m_time_fake_aura = {};

	m_time_fake_aura_duration = {};
	m_time_fake_aura_delay = {};
	m_fake_max_add_dist = {};
	m_fake_min_add_dist = {};

	m_time_started = {};
}

void CControllerAura::update_schedule()
{
	if (!m_object->g_Alive()) return;

	float dist_to_actor		= Actor()->Position().distance_to(m_object->Position());

	if ((dist_to_actor > aura_radius + 
		EntityDefinitions::CControllerBase::FAKE_MIN_ADD_DIST) && (dist_to_actor < aura_radius + EntityDefinitions::CControllerBase::FAKE_MAX_ADD_DIST))
	{
		
		// first time? 
		if (m_time_fake_aura == 0) {
			m_time_fake_aura = time() + 5000 + Random.randI(EntityDefinitions::CControllerBase::FAKE_AURA_DELAY);
			
			if (active()) {
				m_effector->switch_off	();
				m_effector				= 0;
			}
		} else {
			if (active()) {
				// check to stop
				if (m_time_fake_aura < time())  {
					m_effector->switch_off	();
					m_effector				= 0;
					m_time_fake_aura		= time() + 5000 + Random.randI(EntityDefinitions::CControllerBase::FAKE_AURA_DELAY);
				}
			} else {
				// check to start
				if (m_time_fake_aura < time())  {
					m_effector = new CPPEffectorControllerAura	(m_state, 5000, aura_sound.left, aura_sound.right);
					Actor()->Cameras().AddPPEffector				(m_effector);

					m_time_fake_aura		= time() + 5000 + Random.randI(EntityDefinitions::CControllerBase::FAKE_AURA_DURATION);
				}
			}
		}
	} else {
		m_time_fake_aura = 0;

		bool need_be_active		= (dist_to_actor < aura_radius);

		if (active()) 
		{
			if (!need_be_active) 
			{
				m_effector->switch_off	();
				m_effector			=	0;
			}
		} 
		else 
		{
			if ( need_be_active )
			{
				// create effector
				m_effector = new CPPEffectorControllerAura	(m_state, 5000, aura_sound.left, aura_sound.right);
				Actor()->Cameras().AddPPEffector				(m_effector);
				m_time_started		=	time();
			}
		}
	}

	if (active()) {
		CEffectorCam* ce = Actor()->Cameras().GetCamEffector((ECamEffectorType)effControllerAura2);
		if(!ce) AddEffector(Actor(), effControllerAura2, "effector_controller_aura2", 0.15f);
	}else{
		CEffectorCam* ce = Actor()->Cameras().GetCamEffector((ECamEffectorType)effControllerAura2);
		if(ce)
			RemoveEffector(Actor(), effControllerAura2);
	}
}

void CControllerAura::update_frame()
{
}

void CControllerAura::on_death()
{
	if (active()) {
		m_effector->switch_off	();
		m_effector				= 0;
	}
}

void CControllerAura::load(LPCSTR section)
{
	inherited::load				(pSettings->r_string(section,"aura_effector"));
	
	aura_sound.left.create		(pSettings->r_string(section,"PsyAura_SoundLeftPath"),st_Effect,sg_SourceType);
	aura_sound.right.create		(pSettings->r_string(section,"PsyAura_SoundRightPath"),st_Effect,sg_SourceType);

	aura_radius					= READ_IF_EXISTS(pSettings,r_float,section,"PsyAura_Radius", 40.f);

	m_time_fake_aura			= 0;

	m_time_fake_aura_duration	= READ_IF_EXISTS(pSettings,r_u32,section,"PsyAura_Fake_Duration", 3000);
	m_time_fake_aura_delay		= READ_IF_EXISTS(pSettings,r_u32,section,"PsyAura_Fake_Delay", 8000);
	m_fake_max_add_dist			= READ_IF_EXISTS(pSettings,r_float,section,"PsyAura_Fake_MaxAddDist", 90.f);
	m_fake_min_add_dist			= READ_IF_EXISTS(pSettings,r_float,section,"PsyAura_Fake_MinAddDist", 20.f);

	m_time_started				= 0;
}


