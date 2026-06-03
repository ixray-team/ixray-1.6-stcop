#include "stdafx.h"

#include "src/xrSound/ai_sounds.h"
#include "ParticlesObject.h"
#include "PolterAbility.h"
#include "PolterInterface.h"
#include "Level.h"
#include "src/xrGame/ai/monsters/basemonster/base_monster.h"

#include "alife_space.h"


CPolterSpecialAbility::CPolterSpecialAbility(IPolterInterface* polter)
{
	m_object					= polter;

	m_particles_object			= nullptr;
	m_particles_object_electro	= nullptr;
}


CPolterSpecialAbility::~CPolterSpecialAbility()
{
	Particles::Details::Destroy(m_particles_object);
	Particles::Details::Destroy(m_particles_object_electro);
}

void CPolterSpecialAbility::load(str_c section)
{
	m_particles_hidden					= pSettings->r_string(section,"Particles_Hidden");
	m_particles_damage					= pSettings->r_string(section,"Particles_Damage");
	m_particles_death					= pSettings->r_string(section,"Particles_Death");
	m_particles_idle					= pSettings->r_string(section,"Particles_Idle");

	m_sound_base.create					(pSettings->r_string(section,"Sound_Idle"), st_Effect, SOUND_TYPE_MONSTER_TALKING);

	m_last_hit_frame					= 0;
}

void CPolterSpecialAbility::update_schedule()
{
	if (m_object->GetMonster()->g_Alive()) {
		if (!m_sound_base.is_playing())
		{
			m_sound_base.play_at_pos(m_object->GetMonster(), m_object->GetMonster()->Position());
		}
		else
		{
			m_sound_base.set_position(m_object->GetMonster()->Position());
		}
	}
}

void CPolterSpecialAbility::on_hide()
{
	VERIFY(m_particles_object == 0);
	if (!m_object->GetMonster()->g_Alive())
		return;

 	m_particles_object			= m_object->GetMonster()->PlayParticles	(m_particles_hidden, m_object->GetMonster()->Position(),Fvector().set(0.0f,0.1f,0.0f), false);
 	m_particles_object_electro	= m_object->GetMonster()->PlayParticles	(m_particles_idle, m_object->GetMonster()->Position(),Fvector().set(0.0f,0.1f,0.0f), false);
}

void CPolterSpecialAbility::on_show()
{
	if (m_particles_object)			Particles::Details::Destroy(m_particles_object);
	if (m_particles_object_electro) Particles::Details::Destroy(m_particles_object_electro);
}

void CPolterSpecialAbility::update_frame()
{
	if (m_particles_object)			m_particles_object->SetXFORM		(m_object->GetMonster()->XFORM());
	if (m_particles_object_electro)	m_particles_object_electro->SetXFORM(m_object->GetMonster()->XFORM());
}

void CPolterSpecialAbility::on_die()
{
	Fvector particles_position	= m_object->GetCurrentPosition();
	particles_position.y		+= m_object->GetTargetHeight();

	m_object->GetMonster()->PlayParticles			(m_particles_death, particles_position, Fvector().set(0.0f,1.0f,0.0f), true, false);

	Particles::Details::Destroy		(m_particles_object_electro);
	Particles::Details::Destroy		(m_particles_object);
}

void CPolterSpecialAbility::on_hit(SHit* pHDS)
{
	if (m_object->GetMonster()->g_Alive() && (pHDS->hit_type == ALife::eHitTypeFireWound) && (Device.dwFrame != m_last_hit_frame)) {
		if(BI_NONE != pHDS->bone()) {

			//��������� ���������� ���������
			IKinematics* V = smart_cast<IKinematics*>(m_object->GetMonster()->Visual());

			Fvector start_pos = pHDS->bone_space_position();
			Fmatrix& m_bone = V->LL_GetBoneInstance(pHDS->bone()).mTransform;
			m_bone.transform_tiny	(start_pos);
			m_object->GetMonster()->XFORM().transform_tiny	(start_pos);

			m_object->GetMonster()->PlayParticles(m_particles_damage, start_pos, Fvector().set(0.f,1.f,0.f));
		}
	} 

	m_last_hit_frame = Device.dwFrame;
}

