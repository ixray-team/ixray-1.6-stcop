#include "stdafx.h"
#include "torridZone.h"
#include "../xrEngine/objectanimator.h"
#include "xrServer_Objects_ALife_Monsters.h"

CTorridZone::CTorridZone()
{
	m_animator			= new CObjectAnimator();
}

CTorridZone::~CTorridZone()
{
	xr_delete			(m_animator);
}

BOOL CTorridZone::net_Spawn(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC))
		return			(FALSE);

	CSE_Abstract		*abstract=(CSE_Abstract*)(DC);
	CSE_ALifeTorridZone	*zone	= smart_cast<CSE_ALifeTorridZone*>(abstract);
	VERIFY				(zone);

	m_animator->Load	(zone->get_motion());
	m_animator->Play	(true);

	shedule.t_min = shedule.t_max = 1;
	Engine.Sheduler.Register(this, TRUE);

	return				(TRUE);
}

void CTorridZone::net_Destroy	()
{
	inherited::net_Destroy	();
	Engine.Sheduler.Unregister(this);
}

void CTorridZone::UpdateWorkload(u32 dt)
{
	inherited::UpdateWorkload	(dt);
	m_animator->Update			(float(dt)/1000.f);
	XFORM().set					(m_animator->XFORM());
	OnMove						();
}

void CTorridZone::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	if(m_idle_sound._feedback())		m_idle_sound.set_position		(XFORM().c);
	if(m_blowout_sound._feedback())		m_blowout_sound.set_position	(XFORM().c);
	if(m_hit_sound._feedback())			m_hit_sound.set_position		(XFORM().c);
	if(m_entrance_sound._feedback())	m_entrance_sound.set_position	(XFORM().c);
}

bool CTorridZone::Enable()
{
	bool res = inherited::Enable();
	if(res)
	{
		m_animator->Stop	();
		m_animator->Play	(true);
	}
	return res;
}

bool CTorridZone::Disable()
{
	bool res = inherited::Disable();
	if(res)
		m_animator->Stop	();
	
	return res;
}

// Lain: added
bool   CTorridZone::light_in_slow_mode ()
{
	return true;
}

BOOL   CTorridZone::AlwaysTheCrow ()
{
	return true;
}