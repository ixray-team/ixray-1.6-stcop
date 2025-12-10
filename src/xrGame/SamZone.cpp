#include "stdafx.h"
#include "SamZone.h"

#include "../xrSound/ai_sounds.h"
#include "ai_space.h"
#include "MissileSam.h"
#include "../xrScripts/script_engine.h"
#include "xrMessages.h"

void CSamZone::shedule_Update(u32 time_delta)
{
	inherited::shedule_Update(time_delta);
	/*if(!getCurrentRocket())
	{
		CRocketLauncher::SpawnRocket(m_missile_section, this);
	}*/
}

bool CSamZone::net_Spawn(CSE_Abstract* DC)
{
	auto result = inherited::net_Spawn(DC);
	CRocketLauncher::SpawnRocket(m_missile_section, this);
	return result;
}

void CSamZone::Load(LPCSTR section)
{
	m_missile_section = pSettings->r_string(section, "missile_section");
	//CRocketLauncher::SpawnRocket(m_missile_section, this);
	m_layered_sounds.LoadSound(section, "snd_shoot_rocket", "sndRocket", false, ESoundTypes::SOUND_TYPE_WEAPON_SHOOTING);
}

void CSamZone::LaunchMissile(CGameObject* target)
{
	auto CurrentMissile = smart_cast<CMissileSam*>(getCurrentRocket());
	if(CurrentMissile)
	{
		CurrentMissile->SetTarget(target);

		Fmatrix xform;
		xform.identity();
		Fvector dir;
		Fvector vel;
		dir.sub(target->Position(), Position()).normalize_safe();
		vel.mul(dir, CRocketLauncher::m_fLaunchSpeed);
		xform.k.set(dir);
		Fvector::generate_orthonormal_basis(xform.k, xform.j, xform.i);
		xform.c = Position();
		VERIFY2(_valid(xform), "CSamZone::LaunchMissile. Invalid xform");

		LaunchRocket(xform, vel, zero_vel);

		NET_Packet P;
		u_EventGen(P, GE_LAUNCH_ROCKET, ID());
		P.w_u16(static_cast<u16>(getCurrentRocket()->ID()));
		u_EventSend(P);

		dropCurrentRocket();

		m_layered_sounds.PlaySound("sndRocket", xform.c, this, false);

	} else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSamZone: No missile avalable! Probably, too small delay between launches.");
	}
	CRocketLauncher::SpawnRocket(m_missile_section, this);
}

void CSamZone::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent(P, type);
	u16 id;
	switch (type) {
	case GE_OWNERSHIP_TAKE:
	{
		P.r_u16(id);
		CRocketLauncher::AttachRocket(id, this);
	} break;
	case GE_OWNERSHIP_REJECT:
	case GE_LAUNCH_ROCKET:
	{
		bool bLaunch = (type == GE_LAUNCH_ROCKET);
		P.r_u16(id);
		CRocketLauncher::DetachRocket(id, bLaunch);
	} break;
	}
}
