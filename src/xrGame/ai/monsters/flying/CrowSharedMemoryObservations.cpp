#include "StdAfx.h"
#include "CrowSharedMemory.h"
#include "../../../entity_alive.h"
#include "../../../../xrCore/Collision/xr_area.h"
#include "../../../../xrServerEntities/clsid_game.h"

const CCrowSharedMemory::SObservation* CCrowSharedMemory::Observe(const Fvector& Point, float Radius)
{
	InvalidationFrame = u32(-1);
	Maintain();
	const float Cell = Config.CellSize;
	const int X = int(floorf(Point.x / Cell)), Y = int(floorf(Point.y / Cell)), Z = int(floorf(Point.z / Cell));
	const u32 Now = Device.dwTimeGlobal;
	auto Entry = Observations.find({X, Y, Z});
	if (Entry != Observations.end())
	{
		Entry->second.LastUse = Now;
		if (u32(Now - Entry->second.Created) < Config.ObservationRefreshMs && Entry->second.Radius >= Radius) return &Entry->second;
	}
	if (ObservationFrame != Device.dwFrame)
	{
		ObservationFrame = Device.dwFrame;
		ObservationsRemaining = Config.ObservationQueriesPerFrame;
	}
	if (!ObservationsRemaining) return Entry == Observations.end() || Entry->second.Radius < Radius ? nullptr : &Entry->second;
	--ObservationsRemaining;
	if (Entry == Observations.end())
	{
		if (Config.MaxCachedObservations && Observations.size() >= Config.MaxCachedObservations)
		{
			auto Oldest = std::max_element(Observations.begin(), Observations.end(), [&](const auto& A, const auto& B)
				{ return u32(Now - A.second.LastUse) < u32(Now - B.second.LastUse); });
			Observations.erase(Oldest);
		}
		SObservation Value;
		Value.X = X; Value.Y = Y; Value.Z = Z; Value.Radius = Radius;
		Entry = Observations.emplace(SCellKey{X, Y, Z}, std::move(Value)).first;
	}
	Entry->second.Radius = std::max(Radius, Entry->second.Radius);
	Entry->second.Created = Entry->second.LastUse = Now;
	Entry->second.Threats.clear(); Entry->second.Food.clear();
	Fvector Centre = {(float(X) + .5f) * Cell, (float(Y) + .5f) * Cell, (float(Z) + .5f) * Cell};
	xr_vector<ISpatialShared> Neighbours;
	// The extra half diagonal covers every requesting position inside this cell.
	g_SpatialSpace->q_sphere(Neighbours, 0, ESPATIAL_TYPE::COLLIDEABLE, Centre, Entry->second.Radius + Cell * .8660254f);
	for (const auto& Spatial : Neighbours)
	{
		if (!Spatial) continue;
		auto* Entity = smart_cast<CEntityAlive*>(Spatial->dcast_CObject());
		if (!Entity || Entity->getDestroy() || Entity->CLS_ID == CLSID_AI_SCAVENGER_CROW) continue;
		(Entity->g_Alive() ? Entry->second.Threats : Entry->second.Food).push_back(Entity->ID());
	}
	return &Entry->second;
}
