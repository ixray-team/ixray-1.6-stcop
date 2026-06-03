#include "stdafx.h"
#include "MagnetZone.h"

#include "../xrPhysics/PhysicsShell.h"
#include "entity_alive.h"
#include "PhysicsShellHolder.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"

void CMagnetZone::Load(str_c section)
{
	inherited::Load(section);

	m_fThrowInImpulse = pSettings->r_float(section, "throw_in_impulse");//800.f;
	m_fThrowInImpulseAlive = pSettings->r_float(section, "throw_in_impulse_alive");//800.f;
	m_fThrowInAtten = pSettings->r_float(section, "throw_in_atten");
}

void CMagnetZone::Affect(SZoneObjectInfo* O)
{
	//Msg("CMagnetZone: Try affect [%s]", O->object->Name());
	CPhysicsShellHolder* GO = smart_cast<CPhysicsShellHolder*>(O->object);
	if (!GO) return;


	//////////////////////////////////////////////////////////////////////////
	//	���������� ����� �� ����������� � ������ ����

	Fvector					throw_in_dir;
	Fvector					zone_center;
	ThrowInCenter(zone_center);
	Fvector					go_center;
	GO->Center(go_center);
	throw_in_dir.sub(zone_center, go_center);

	float dist = throw_in_dir.magnitude();
	float dist_to_radius = dist / Radius();

	if (!fis_zero(dist))
		throw_in_dir.mul(1.f / dist);
	else
		throw_in_dir.set(0.f, 1.f, 0.f);

	bool CanApplyPhisImpulse = GO->Local() == TRUE;

	AffectPull(GO, throw_in_dir, dist);
}

void CMagnetZone::AffectPull(CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist)
{
	CEntityAlive* EA = smart_cast<CEntityAlive*>(GO);
	if (EA && EA->g_Alive())
	{
		AffectPullAlife(EA, throw_in_dir, dist);
	}
	else if (GO && GO->PPhysicsShell())
	{
		AffectPullDead(GO, throw_in_dir, dist);
	}
}

void CMagnetZone::AffectPullAlife(CEntityAlive* EA, const Fvector& throw_in_dir, float dist)
{
	float rel_power = RelativePower(dist, Radius());
	float throw_power = m_fThrowInImpulseAlive * rel_power * rel_power * rel_power * rel_power * rel_power;

	Fvector vel;
	vel.set(throw_in_dir);
	vel.mul(throw_power);
	EA->character_physics_support()->movement()->AddControlVel(vel);
}

void CMagnetZone::AffectPullDead(CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist)
{
	GO->PPhysicsShell()->applyImpulse(throw_in_dir, dist * m_fThrowInImpulse * GO->GetMass() / 100.f);
}

void CMagnetZone::ThrowInCenter(Fvector& C)
{
	Center(C);
}

bool CMagnetZone::BlowoutState()
{
	bool state = CAnomalyZone::BlowoutState();

	AffectObjects();

	return state;
}
