#include "StdAfx.h"
#include "CrowSharedMemory.h"
#include "../../../Level.h"

CCrowSharedMemory& CCrowSharedMemory::Get()
{
	static CCrowSharedMemory Memory;
	return Memory;
}

void CCrowSharedMemory::Update()
{
	Maintain();
}

void CCrowSharedMemory::RegisterBird(u32 Bird)
{
	if (!ConfigLoaded)
	{
		ConfigLoaded = true;
		ReloadConfig();
	}
	if (std::find(Birds.begin(), Birds.end(), Bird) == Birds.end()) Birds.push_back(Bird);
}

void CCrowSharedMemory::UnregisterBird(u32 Bird)
{
	ReleaseFood(Bird);
	Birds.erase(std::remove(Birds.begin(), Birds.end(), Bird), Birds.end());
	if (Birds.empty())
	{
		xr_map<u32, SCorpse>().swap(Corpses);
		xr_vector<SFoodClaim>().swap(Claims);
		xr_map<SCellKey, SObservation>().swap(Observations);
		xr_vector<u32>().swap(Birds);
		xr_hash_set<u32>().swap(InvalidatedObjects);
		InvalidationFrame = u32(-1);
		MaintenanceTime = 0;
		ProbeFrame = ObservationFrame = u32(-1);
	}
}

void CCrowSharedMemory::InvalidateObject(u32 Object)
{
	// net_Relcase broadcasts the same deletion to every crow. Process it once
	// until a cache query can introduce fresh information or an ID is reused.
	if (InvalidationFrame != Device.dwFrame)
	{
		InvalidatedObjects.clear();
		InvalidationFrame = Device.dwFrame;
	}
	if (!InvalidatedObjects.insert(Object).second) return;
	for (auto& Pair : Observations)
	{
		auto& Observation = Pair.second;
		Observation.Threats.erase(std::remove(Observation.Threats.begin(), Observation.Threats.end(), Object), Observation.Threats.end());
		Observation.Food.erase(std::remove(Observation.Food.begin(), Observation.Food.end(), Object), Observation.Food.end());
	}
	Corpses.erase(Object);
	Claims.erase(std::remove_if(Claims.begin(), Claims.end(),
		[=](const SFoodClaim& Claim) { return Claim.Bird == Object || Claim.Corpse == Object; }), Claims.end());
}

void CCrowSharedMemory::ReleaseFood(u32 Bird)
{
	for (const auto& Claim : Claims)
	{
		// Only a place actually used by this bird needs replacement on departure.
		if (Claim.Bird == Bird && Claim.HasPerch) RejectPerch(Claim.Corpse, Claim.Contact);
	}
	Claims.erase(std::remove_if(Claims.begin(), Claims.end(),
		[=](const SFoodClaim& Claim) { return Claim.Bird == Bird; }), Claims.end());
}

void CCrowSharedMemory::ClaimFood(u32 Bird, u32 Corpse, const Fvector* Contact)
{
	InvalidationFrame = u32(-1);
	ReleaseFood(Bird);
	SFoodClaim Claim;
	Claim.Bird = Bird;
	Claim.Corpse = Corpse;
	Claim.HasPerch = Contact != nullptr;
	Claim.Contact = Contact ? *Contact : Fvector().set(0,0,0);
	Claims.push_back(Claim);
}

u32 CCrowSharedMemory::FoodUsers(u32 Corpse) const
{
	return u32(std::count_if(Claims.begin(), Claims.end(),
		[=](const SFoodClaim& Claim) { return Claim.Corpse == Corpse; }));
}

void CCrowSharedMemory::Maintain()
{
	const u32 Now = Device.dwTimeGlobal;
	if (u32(Now - MaintenanceTime) < Config.MaintenanceMs) return;
	MaintenanceTime = Now;
	for (auto Entry = Corpses.begin(); Entry != Corpses.end();)
	{
		// Deletion is also delivered by net_Relcase; this handles missed notifications.
		auto* Object = Level().Objects.net_Find(Entry->first);
		if (!Object || Object->getDestroy() || u32(Now - Entry->second.LastUse) > Config.CorpseIdleMs)
			Entry = Corpses.erase(Entry);
		else ++Entry;
	}
	for (auto Entry = Observations.begin(); Entry != Observations.end();)
	{
		if (u32(Now - Entry->second.LastUse) > Config.ObservationIdleMs) Entry = Observations.erase(Entry);
		else ++Entry;
	}
}
