#pragma once

CStateBurerShield::CStateBurerShield (CBaseMonster*obj) : inherited(obj)
{
	m_last_shield_started			=	0;
	m_shield_start_anim_length_sec	=	0;
	m_next_particle_allowed			=	0;
}

void   CStateBurerShield::initialize()
{
	inherited::initialize				();
	this->object->set_script_capture			(false);
	m_last_shield_started			=	current_time();
	m_next_particle_allowed			=	0;
	m_started						=	false;

	MotionID	motion;
	this->object->anim().get_animation_info	(eAnimShieldStart, 0, motion, m_shield_start_anim_length_sec);
}

void   CStateBurerShield::execute()
{
	if ( !m_started ) // && current_time() > m_last_shield_started + TTime(m_shield_start_anim_length_sec*1000) )
	{
		m_started					=	true;
		this->object->ActivateShield			();
	}

	if ( m_started && 
		 this->object->m_shield_keep_particle != 0 && 
		 current_time() > m_next_particle_allowed )
	{
		this->object->CParticlesPlayer::StartParticles	(this->object->m_shield_keep_particle, 
													 Fvector().set(0,1,0), 
													 this->object->ID(), 
													 -1, 
													 true);

		m_next_particle_allowed		=	current_time() + this->object->m_shield_keep_particle_period;
	}

	this->object->face_enemy					();
	this->object->set_action					(ACT_STAND_IDLE);

	this->object->anim().set_override_animation	(m_started ? eAnimShieldContinue : eAnimShieldStart);
}

void   CStateBurerShield::finalize()
{
	inherited::finalize();
	this->object->DeactivateShield			();
	this->object->set_script_capture			(true);
}

void   CStateBurerShield::critical_finalize()
{
	inherited::critical_finalize		();
	this->object->DeactivateShield			();
	this->object->set_script_capture			(false);
}

bool   CStateBurerShield::check_start_conditions()
{
	if ( current_time() < m_last_shield_started + this->object->m_shield_time + this->object->m_shield_cooldown )
		return							false;
	
	if ( !this->object->EnemyMan.enemy_see_me_now() )
		return							false;

	return								true;
}

bool   CStateBurerShield::check_completion()
{
	if ( current_time() > m_last_shield_started + this->object->m_shield_time )
		return							true;

	CEntityAlive const* enemy		=	this->object->EnemyMan.get_enemy();
	if ( !enemy )
		return							true;

	if ( enemy == Actor() )
	{
		if ( actor_is_reloading_weapon () )
			return						true;
	}

	if ( !this->object->EnemyMan.get_enemy() )
		return							true;

	return								false;
}
