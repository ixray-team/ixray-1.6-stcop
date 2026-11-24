#pragma once

class CAnomalyZone;

namespace AnomalyZoneRegistry
{
struct SNearbyHazards
{
	bool Anomaly = false;
	bool Radiation = false;
};

void Add(CAnomalyZone* Zone);
void Remove(CAnomalyZone* Zone);
SNearbyHazards Query(const Fvector& Position, float Radius);
}
