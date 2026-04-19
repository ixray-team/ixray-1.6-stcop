#include "StdAfx.h"
#include "TorridZone.h"
#include "../xrEngine/ObjectAnimator.h"
#include "xrServer_Objects_ALife_Monsters.h"

CTorridZone::CTorridZone()
{
	m_animator			= new CObjectAnimator();
}

CTorridZone::~CTorridZone()
{
	xr_delete			(m_animator);
}

bool CTorridZone::net_Spawn(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC))
		return			(false);

	CSE_Abstract		*abstract=(CSE_Abstract*)(DC);
	CSE_ALifeTorridZone	*zone	= smart_cast<CSE_ALifeTorridZone*>(abstract);
	VERIFY				(zone);

	m_animator->Load	(zone->get_motion());
	m_animator->Play	(true);

	return				(true);
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

	UpdateSoundsPosition(m_idle_sounds_variants, XFORM().c);
	UpdateSoundsPosition(m_blowout_sounds_variants, XFORM().c);
	UpdateSoundsPosition(m_hit_sounds_variants, XFORM().c);
	UpdateSoundsPosition(m_entrance_sounds_variants, XFORM().c);
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
bool CTorridZone::light_in_slow_mode ()
{
	return true;
}

bool CTorridZone::AlwaysTheCrow ()
{
	return true;
}