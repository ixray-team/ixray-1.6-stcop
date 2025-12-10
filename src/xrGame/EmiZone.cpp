#include "stdafx.h"
#include "EmiZone.h"

#include "entity_alive.h"

void CEmiZone::Load(LPCSTR section)
{
	inherited::Load(section);
}

void CEmiZone::enter_Zone(SZoneObjectInfo& io)
{
	inherited::enter_Zone(io);
	auto Object = smart_cast<CEntityAlive*>(io.object);
	if (!Object)
	{
		return;
	}
	Object->SetInEmi(this);
}

void CEmiZone::exit_Zone(SZoneObjectInfo& io)
{
	inherited::exit_Zone(io);
	auto Object = smart_cast<CEntityAlive*>(io.object);
	if (!Object)
	{
		return;
	}
	Object->SetOutEmi(this);
}
