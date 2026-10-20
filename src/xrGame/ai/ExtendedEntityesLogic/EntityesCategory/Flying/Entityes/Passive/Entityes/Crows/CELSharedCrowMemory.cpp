#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "ai/ExtendedEntityesLogic/CoreComponent/CELNativeEntityes.h"
#include "entity_alive.h"
#include "../xrServerEntities/clsid_game.h"
#include "Level.h"

// Возвращает единственный общий экземпляр службы.
CELSharedCrowMemory& CELSharedCrowMemory::Instance()
{
	static CELSharedCrowMemory Memory;
	return Memory;
}

// Упорядочивает координатные ключи для поиска в общем реестре.
bool CELSharedCrowMemory::SCellKey::operator<(const SCellKey& Other) const
{
	if (Column != Other.Column)
	{
		return Column < Other.Column;
	}
	if (Height != Other.Height)
	{
		return Height < Other.Height;
	}
	return Row < Other.Row;
}

// Регистрирует владельца в общем обслуживании ворон.
void CELSharedCrowMemory::Register(u16 BirdIdentifier)
{
	if (!HasSettings)
	{
		LoadSettings();
	}
	if (std::find(Birds.begin(), Birds.end(), BirdIdentifier) == Birds.end())
	{
		Birds.push_back(BirdIdentifier);
	}
}
// Снимает регистрацию вороны и освобождает её назначения.
void CELSharedCrowMemory::Unregister(u16 BirdIdentifier)
{
	ReleaseFood(BirdIdentifier);
	Birds.erase(std::remove(Birds.begin(), Birds.end(), BirdIdentifier), Birds.end());
	if (Birds.empty())
	{
		Clear();
	}
}

// Обновляет личные наблюдения владельца с учётом доступного бюджета.
const CELSharedCrowMemory::SObservation* CELSharedCrowMemory::Observe(const Fvector& Point, float Radius)
{
	if (!HasSettings)
	{
		LoadSettings();
	}
	const SCellKey Key = {iFloor(Point.x / Settings.CellSize), iFloor(Point.y / Settings.CellSize), iFloor(Point.z / Settings.CellSize)};
	xr_map<SCellKey, SObservation>::iterator Found = Observations.find(Key);
	if (Found == Observations.end())
	{
		if (Settings.ObservationLimit && Observations.size() >= Settings.ObservationLimit)
		{
			return nullptr;
		}
		SObservation Observation;
		Observation.Center.set((float(Key.Column) + .5f) * Settings.CellSize, (float(Key.Height) + .5f) * Settings.CellSize, (float(Key.Row) + .5f) * Settings.CellSize);
		Found = Observations.emplace(Key, std::move(Observation)).first;
	}
	SObservation& Observation = Found->second;
	Observation.LastUse = Device.dwTimeGlobal;
	if (!Observation.Created || Device.dwTimeGlobal - Observation.Created >= Settings.ObservationRefresh || Observation.Radius < Radius)
	{
		Observation.Pending = true;
		Observation.Radius = std::max(Observation.Radius, Radius);
	}
	return Observation.Created ? &Observation : nullptr;
}

// Порционно обновляет общие наблюдения и назначения ворон.
void CELSharedCrowMemory::BeginFrame()
{
	if (Birds.empty())
	{
		return;
	}
	Maintain();
	UpdateTrees();
	xr_map<SCellKey, SObservation>::iterator ObservationIterator;
	xr_vector<ISpatialShared> Neighbors;
	xr_vector<ISpatialShared>::const_iterator Neighbor;
	CObject* Object;
	CEntityAlive* Entity;
	u32 QueryCount = 0;
	u32 Visited;
	ObservationIterator = HasObservationCursor ? Observations.upper_bound(LastObservation) : Observations.begin();
	SObservation* PendingObservation;
	for (Visited = 0; Visited < std::min(u32(Observations.size()), Settings.ScanEntries) && QueryCount < Settings.ObservationQueries; ++Visited)
	{
		if (ObservationIterator == Observations.end())
		{
			ObservationIterator = Observations.begin();
		}
		LastObservation = ObservationIterator->first;
		HasObservationCursor = true;
		PendingObservation = &ObservationIterator->second;
		++ObservationIterator;
		if (!PendingObservation->Pending)
		{
			continue;
		}
		if (!ExtendedEntityes::PerceptionBudget().TrySpatial())
		{
			break;
		}
		++QueryCount;
		g_SpatialSpace->q_sphere(Neighbors, 0, ESPATIAL_TYPE::COLLIDEABLE, PendingObservation->Center, PendingObservation->Radius + Settings.CellSize * .8660254f);
		PendingObservation->Food.clear();
		PendingObservation->Threats.clear();
		for (Neighbor = Neighbors.begin(); Neighbor != Neighbors.end(); ++Neighbor)
		{
			Object = *Neighbor ? (*Neighbor)->dcast_CObject() : nullptr;
			Entity = Object ? Object->cast_entity_alive() : nullptr;
			if (!Entity || Entity->getDestroy() || Entity->CLS_ID == ExtendedEntityesClasses::Crow || Entity->CLS_ID == CLSID_AI_CROW)
			{
				continue;
			}
			(Entity->g_Alive() ? PendingObservation->Threats : PendingObservation->Food).push_back(Entity->ID());
		}
		PendingObservation->Created = Device.dwTimeGlobal;
		PendingObservation->Pending = false;
	}
	xr_map<u16, SCorpse>::iterator CorpseIterator;
	u32 ProbeCount = 0;
	CorpseIterator = Corpses.upper_bound(LastCorpse);
	xr_map<u16, SCorpse>::iterator PendingCorpse;
	for (Visited = 0; Visited < std::min(u32(Corpses.size()), Settings.ScanEntries) && ProbeCount < Settings.PerchProbes; ++Visited)
	{
		if (CorpseIterator == Corpses.end())
		{
			CorpseIterator = Corpses.begin();
		}
		PendingCorpse = CorpseIterator++;
		LastCorpse = PendingCorpse->first;
		if (!PendingCorpse->second.Pending || s32(Device.dwTimeGlobal - PendingCorpse->second.RetryAfter) < 0)
		{
			continue;
		}
		++ProbeCount;
		BuildPerch(PendingCorpse->first, PendingCorpse->second);
	}
}

// Удаляет устаревшие сведения и обслуживает общие назначения ворон.
void CELSharedCrowMemory::Maintain()
{
	if (s32(Device.dwTimeGlobal - NextMaintenance) < 0)
	{
		return;
	}
	NextMaintenance = Device.dwTimeGlobal + Settings.MaintenanceInterval;
	xr_map<SCellKey, SObservation>::iterator Observation = Observations.begin();
	while (Observation != Observations.end())
	{
		if (Device.dwTimeGlobal - Observation->second.LastUse > Settings.ObservationIdle)
		{
			Observation = Observations.erase(Observation);
		}
		else
		{
			++Observation;
		}
	}
	xr_map<u16, SCorpse>::iterator Corpse = Corpses.begin();
	while (Corpse != Corpses.end())
	{
		if (Device.dwTimeGlobal - Corpse->second.LastUse > Settings.CorpseIdle || !ExtendedEntityes::FindEntity(Corpse->first))
		{
			Corpse = Corpses.erase(Corpse);
		}
		else
		{
			++Corpse;
		}
	}
}

// Освобождает занятое владельцем место у пищи.
void CELSharedCrowMemory::ReleaseFood(u16 BirdIdentifier)
{
	Claims.erase(std::remove_if(Claims.begin(), Claims.end(), [BirdIdentifier](const SClaim& Claim)
								{ return Claim.BirdIdentifier == BirdIdentifier; }),
				 Claims.end());
}

// Резервирует место посадки с учётом дистанции до других ворон.
bool CELSharedCrowMemory::ClaimPerch(u16 BirdIdentifier, const Fvector& Contact, float Spacing)
{
	xr_vector<SClaim>::const_iterator Claim;
	float Required;
	for (Claim = Claims.begin(); Claim != Claims.end(); ++Claim)
	{
		if (Claim->BirdIdentifier == BirdIdentifier)
		{
			continue;
		}
		Required = std::max(Spacing, Claim->Spacing);
		if (Claim->Contact.distance_to_sqr(Contact) < Required * Required)
		{
			return false;
		}
	}
	ReleaseFood(BirdIdentifier);
	Claims.push_back({BirdIdentifier, u16(-1), Contact, Spacing});
	return true;
}

// Проверяет, остаётся ли за владельцем назначение к выбранной пище.
bool CELSharedCrowMemory::HasFoodClaim(u16 BirdIdentifier, u16 CorpseIdentifier)
{
	const xr_map<u16, SCorpse>::iterator Corpse = Corpses.find(CorpseIdentifier);
	CEntityAlive* Entity = ExtendedEntityes::FindEntity(CorpseIdentifier);
	if (Corpse == Corpses.end() || !Entity || Entity->g_Alive())
	{
		return false;
	}
	Fvector Center;
	Entity->Center(Center);
	if (Corpse->second.Center.distance_to_sqr(Center) > Settings.CorpseMoveDistance * Settings.CorpseMoveDistance ||
		Corpse->second.Forward.distance_to_sqr(Entity->XFORM().k) > .0004f)
	{
		return false;
	}
	Corpse->second.LastUse = Device.dwTimeGlobal;
	return std::any_of(Claims.begin(), Claims.end(), [BirdIdentifier, CorpseIdentifier](const SClaim& Claim)
					   { return Claim.BirdIdentifier == BirdIdentifier && Claim.CorpseIdentifier == CorpseIdentifier; });
}

// Занимает свободное место кормления у выбранного тела.
bool CELSharedCrowMemory::ClaimFood(u16 BirdIdentifier, u16 CorpseIdentifier, const Fvector& Contact, u32 SlotLimit)
{
	xr_vector<SClaim>::iterator Claim;
	SClaim* OwnClaim = nullptr;
	u32 OtherUsers = 0;
	for (Claim = Claims.begin(); Claim != Claims.end(); ++Claim)
	{
		if (Claim->BirdIdentifier == BirdIdentifier)
		{
			OwnClaim = &*Claim;
			continue;
		}
		if (Claim->CorpseIdentifier == CorpseIdentifier)
		{
			++OtherUsers;
			if (Claim->Contact.distance_to_sqr(Contact) < Claim->Spacing * Claim->Spacing)
			{
				return false;
			}
		}
	}
	if (OtherUsers >= SlotLimit)
	{
		return false;
	}
	if (OwnClaim)
	{
		*OwnClaim = {BirdIdentifier, CorpseIdentifier, Contact};
	}
	else
	{
		Claims.push_back({BirdIdentifier, CorpseIdentifier, Contact});
	}
	return true;
}

// Возвращает число ворон, назначенных к выбранной пище.
u32 CELSharedCrowMemory::FoodUsers(u16 CorpseIdentifier) const
{
	return u32(std::count_if(Claims.begin(), Claims.end(), [CorpseIdentifier](const SClaim& Claim)
							 { return Claim.CorpseIdentifier == CorpseIdentifier; }));
}

// Помечает устаревшими сведения об изменившемся или исчезнувшем объекте.
void CELSharedCrowMemory::Invalidate(u16 ObjectIdentifier)
{
	Corpses.erase(ObjectIdentifier);
	Claims.erase(std::remove_if(Claims.begin(), Claims.end(), [ObjectIdentifier](const SClaim& Claim)
								{ return Claim.BirdIdentifier == ObjectIdentifier || Claim.CorpseIdentifier == ObjectIdentifier; }),
				 Claims.end());
	// Observation entries retain IDs only; consumers resolve and validate each candidate.
}
// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedCrowMemory::Clear()
{
	Observations.clear();
	Corpses.clear();
	Claims.clear();
	Birds.clear();
	Trees.clear();
	NextTreeRefresh = 0;
	CachedTreeCount = 0;
	NextMaintenance = 0;
	HasSettings = false;
	Settings = SSettings();
	HasObservationCursor = false;
	LastCorpse = u16(-1);
}

// Порционно обновляет сведения о деревьях для поиска укрытий.
void CELSharedCrowMemory::UpdateTrees()
{
	const size_t Count = Device.m_trees_poses_pm.size() + Device.m_trees_poses_st.size();
	if (Count == CachedTreeCount && s32(Device.dwTimeGlobal - NextTreeRefresh) < 0)
	{
		return;
	}
	Trees.clear();
	const xr_vector<Fmatrix*>* Collections[] = {&Device.m_trees_poses_pm, &Device.m_trees_poses_st};
	u32 CollectionIndex;
	xr_vector<Fmatrix*>::const_iterator Transform;
	int Column;
	int Row;
	for (CollectionIndex = 0; CollectionIndex < 2; ++CollectionIndex)
	{
		for (Transform = Collections[CollectionIndex]->begin(); Transform != Collections[CollectionIndex]->end(); ++Transform)
		{
			if (!*Transform || !_valid((*Transform)->c))
			{
				continue;
			}
			Column = iFloor((*Transform)->c.x / Settings.CellSize);
			Row = iFloor((*Transform)->c.z / Settings.CellSize);
			Trees[(u64(u32(Column)) << 32) | u32(Row)].push_back((*Transform)->c);
		}
	}
	CachedTreeCount = Count;
	NextTreeRefresh = Device.dwTimeGlobal + Settings.ObservationIdle;
}
// Находит подходящее известное дерево рядом с точкой.
bool CELSharedCrowMemory::NearestTree(const Fvector& Point, float Radius, Fvector& TreePosition) const
{
	int Column;
	int Row;
	xr_map<u64, xr_vector<Fvector>>::const_iterator Cell;
	xr_vector<Fvector>::const_iterator Tree;
	float BestDistance = Radius * Radius;
	float Distance;
	bool Found = false;
	for (Column = iFloor((Point.x - Radius) / Settings.CellSize); Column <= iFloor((Point.x + Radius) / Settings.CellSize); ++Column)
	{
		for (Row = iFloor((Point.z - Radius) / Settings.CellSize); Row <= iFloor((Point.z + Radius) / Settings.CellSize); ++Row)
		{
			Cell = Trees.find((u64(u32(Column)) << 32) | u32(Row));
			if (Cell == Trees.end())
			{
				continue;
			}
			for (Tree = Cell->second.begin(); Tree != Cell->second.end(); ++Tree)
			{
				Distance = (Tree->x - Point.x) * (Tree->x - Point.x) + (Tree->z - Point.z) * (Tree->z - Point.z);
				if (Distance < BestDistance)
				{
					BestDistance = Distance;
					TreePosition = *Tree;
					Found = true;
				}
			}
		}
	}
	return Found;
}

// Читает общие ограничения наблюдений и назначений ворон.
void CELSharedCrowMemory::LoadSettings()
{
	HasSettings = true;
	string_path Path;
	FS.update_path(Path, "$game_config$", "creatures\\crow_shared_memory.ltx");
	if (!FS.exist(Path))
	{
		return;
	}
	CInifile Configuration(Path, TRUE, TRUE, FALSE);
	if (!Configuration.section_exist("crow_shared_memory"))
	{
		return;
	}
	Settings.CorpseLimit = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "corpse_limit", Settings.CorpseLimit);
	clamp(Settings.CorpseLimit, 0u, 65535u);
	Settings.ObservationLimit = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "observation_limit", Settings.ObservationLimit);
	clamp(Settings.ObservationLimit, 0u, 65535u);
	Settings.PerchCount = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "perches_per_corpse", Settings.PerchCount);
	clamp(Settings.PerchCount, 0u, 128u);
	Settings.CorpseRefresh = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "corpse_refresh_ms", Settings.CorpseRefresh);
	clamp(Settings.CorpseRefresh, 0u, 3600000u);
	Settings.ObservationRefresh = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "observation_refresh_ms", Settings.ObservationRefresh);
	clamp(Settings.ObservationRefresh, 16u, 60000u);
	Settings.PerchAttempts = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "perch_search_attempts", Settings.PerchAttempts);
	clamp(Settings.PerchAttempts, 1u, 1024u);
	Settings.PerchRetry = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "perch_retry_ms", Settings.PerchRetry);
	clamp(Settings.PerchRetry, 250u, 60000u);
	Settings.CorpseIdle = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "corpse_idle_ms", Settings.CorpseIdle);
	clamp(Settings.CorpseIdle, 250u, 3600000u);
	Settings.ObservationIdle = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "observation_idle_ms", Settings.ObservationIdle);
	clamp(Settings.ObservationIdle, 250u, 3600000u);
	Settings.MaintenanceInterval = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "maintenance_ms", Settings.MaintenanceInterval);
	clamp(Settings.MaintenanceInterval, 16u, 10000u);
	Settings.PerchProbes = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "perch_probes_per_frame", Settings.PerchProbes);
	clamp(Settings.PerchProbes, 1u, 64u);
	Settings.ObservationQueries = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "observation_queries_per_frame", Settings.ObservationQueries);
	clamp(Settings.ObservationQueries, 1u, 64u);

	Settings.ScanEntries = (&Configuration)->read_if_exists<u32>("crow_shared_memory", "cache_scan_entries_per_frame", Settings.ScanEntries);
	clamp(Settings.ScanEntries, 1u, 4096u);
	Settings.CellSize = (&Configuration)->read_if_exists<float>("crow_shared_memory", "observation_cell_size", 16.f);
	Settings.CorpseMoveDistance = (&Configuration)->read_if_exists<float>("crow_shared_memory", "corpse_move_distance", .05f);
	if (!_valid(Settings.CellSize))
	{
		Settings.CellSize = 16.f;
	}
	if (!_valid(Settings.CorpseMoveDistance))
	{
		Settings.CorpseMoveDistance = .05f;
	}
	clamp(Settings.CellSize, 2.f, 128.f);
	clamp(Settings.CorpseMoveDistance, .005f, 1.f);
}
