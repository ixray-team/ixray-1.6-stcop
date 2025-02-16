#include "stdafx.h"
#include "pch_script.h"
#include "HairsZone.h"
#include "hudmanager.h"
#include "level.h"
#include "PhysicsShellHolder.h"
#include "entity_alive.h"
#include "PHMovementControl.h"
#include "CharacterPhysicsSupport.h"
#include "../xr_collide_form.h"

bool CHairsZone::BlowoutState()
{
	bool result = inherited::BlowoutState();

	if(!result)
		UpdateBlowout();

	return result;
}

void CHairsZone::CheckForAwaking()
{
	for(OBJECT_INFO_VEC_IT it = m_ObjectInfoMap.begin(); m_ObjectInfoMap.end() != it; ++it) 
	{
		CObject* pObject = (*it).object;

		if (!pObject)
			continue;

		SwitchZoneState(eZoneStateAwaking);
	}
}

void CHairsZone::Load(LPCSTR section) 
{
	inherited::Load(section);

	m_min_speed_to_react		= pSettings->r_float(section, "min_speed_to_react");
}

void CHairsZone::UpdateBlowout()
{
	if (m_dwBlowoutExplosionTime >= (u32)m_iPreviousStateTime &&
		m_dwBlowoutExplosionTime<(u32)m_iStateTime)
	{
		AffectObjects();
		BornArtefact();
	}
}

float CHairsZone::CalcHitPower(float velocity, CPhysicsShellHolder* object)
{
	const Fbox&	self_bb = CFORM()->getBBox();

	const Fbox&	hitted_bb = object->character_physics_support()->movement()->Box();

	Fvector	self_r;
	self_bb.getradius(self_r);
	Fvector	hitted_r;
	hitted_bb.getradius(hitted_r);

	float max_y = Position().y;
	float min_y = Position().y - (self_r.y * 2);

	float hitted_top_point = object->Position().y + hitted_r.y * 2;

	if (Position().y < min_y) // objects is not hitting the anomaly
		return 0.f;

	float distnce_from_top = max_y - hitted_top_point;
	float distance_k = distnce_from_top / (max_y - min_y);
	float velocity_k = velocity / m_min_speed_to_react;

	float hit_power = m_fMaxPower * distance_k * velocity_k;

	//Msg("max %f min %f hitted_top_point %f dist %f D K %f V k %f power %f", max_y, min_y, hitted_top_point, distnce_from_top, distance_k, velocity_k, hit_power);

	return hit_power;
}

void CHairsZone::Affect(SZoneObjectInfo* O) 
{
	if (O->zone_ignore)
		return;

	CPhysicsShellHolder* phys_shell_holder = smart_cast<CPhysicsShellHolder*>(O->object);

	if (!phys_shell_holder)
		return;

	if (!phys_shell_holder->character_physics_support())
	{
		PlayHitParticles(phys_shell_holder);

		return;
	}

	float sp = phys_shell_holder->character_physics_support()->movement()->GetVelocityActual();
	if (sp < m_min_speed_to_react)
		return;

	Fvector P;
	XFORM().transform_tiny(P,CFORM()->getSphere().P);

	if (phys_shell_holder->Position().y > P.y) // don't interfear with object if its walking on top of it
		return;

	float power = CalcHitPower(sp, phys_shell_holder);
	float impulse = m_fHitImpulseScale * power * phys_shell_holder->GetMass();

	if(power > 0.01f) 
	{
		Fvector hit_dir;
		hit_dir.set(::Random.randF(-.5f, .5f),
			::Random.randF(.0f, 1.f),
			::Random.randF(-.5f, .5f));
		hit_dir.normalize();

		m_dwDeltaTime = 0;

		//статистика по объекту
		O->total_damage += power;
		O->hit_num++;

		Fvector position_in_bone_space;

		position_in_bone_space.set(0.f, 0.f, 0.f);

		CreateHit(phys_shell_holder->ID(), ID(), hit_dir, power, 0, position_in_bone_space, impulse, m_eHitTypeBlowout);

		PlayHit(phys_shell_holder);
	}
}

void CHairsZone::PlayHit(CGameObject* hitted_object)
{
	if (m_dwBlowoutParticlesTime >= (u32)m_iPreviousStateTime &&
		m_dwBlowoutParticlesTime<(u32)m_iStateTime)
		PlayBlowoutParticles();

	if (m_dwBlowoutLightTime >= (u32)m_iPreviousStateTime &&
		m_dwBlowoutLightTime<(u32)m_iStateTime)
		StartBlowoutLight();

	if (m_dwBlowoutSoundTime >= (u32)m_iPreviousStateTime &&
		m_dwBlowoutSoundTime<(u32)m_iStateTime)
		m_blowout_sound.play_at_pos(0, Position());

	if (m_zone_flags.test(eBlowoutWind) && m_dwBlowoutWindTimeStart >= (u32)m_iPreviousStateTime &&
		m_dwBlowoutWindTimeStart<(u32)m_iStateTime)
		StartWind();

	UpdateWind();

	PlayHitParticles(hitted_object);
}

bool CHairsZone::ShouldIgnoreObject(CGameObject* pObject)
{
	return inherited::ShouldIgnoreObject(pObject);
}
