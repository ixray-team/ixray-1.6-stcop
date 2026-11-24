#include "StdAfx.h"
#include "AnomalyZoneRegistry.h"
#include "AnomalyZone.h"
#include "ZoneCampfire.h"

namespace AnomalyZoneRegistryStorage
{
xrCriticalSection Lock;
xr_vector<CAnomalyZone*> Zones;
}

namespace AnomalyZoneRegistry
{
void Add(CAnomalyZone* Zone)
{
	if (!Zone || smart_cast<CZoneCampfire*>(Zone))
	{
		return;
	}
	xrCriticalSectionGuard Guard(AnomalyZoneRegistryStorage::Lock);
	xr_vector<CAnomalyZone*>& Zones = AnomalyZoneRegistryStorage::Zones;
	if (std::find(Zones.begin(), Zones.end(), Zone) == Zones.end())
	{
		Zones.push_back(Zone);
	}
}

void Remove(CAnomalyZone* Zone)
{
	xrCriticalSectionGuard Guard(AnomalyZoneRegistryStorage::Lock);
	xr_vector<CAnomalyZone*>& Zones = AnomalyZoneRegistryStorage::Zones;
	const auto Found = std::find(Zones.begin(), Zones.end(), Zone);
	if (Found != Zones.end())
	{
		Zones.erase(Found);
	}
}

SNearbyHazards Query(const Fvector& Position, float Radius)
{
	SNearbyHazards Result;
	xrCriticalSectionGuard Guard(AnomalyZoneRegistryStorage::Lock);
	for (CAnomalyZone* Zone : AnomalyZoneRegistryStorage::Zones)
	{
		if (!Zone->IsEnabled())
		{
			continue;
		}
		const float Reach = Radius + Zone->Radius();
		if (Zone->Position().distance_to_sqr(Position) > Reach * Reach)
		{
			continue;
		}
		if (Zone->GetHitType() == ALife::eHitTypeRadiation)
		{
			Result.Radiation = true;
		}
		else
		{
			Result.Anomaly = true;
		}
		if (Result.Radiation && Result.Anomaly)
		{
			break;
		}
	}
	return Result;
}
}
