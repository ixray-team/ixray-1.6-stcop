#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"
class CBaseMonster;
class CObject;
struct SELPerceptionProfile;

// Game-phase service. Shared claims coordinate work; evidence remains private to each observer.
class CELSharedDeaths
{
public:
	static void Record(CBaseMonster& Victim, CObject* Killer);
	static bool Witness(CBaseMonster& Observer, const SELPerceptionProfile& Profile, SELObservation& Enemy);
	static void Observe(CBaseMonster& Observer, const SELPerceptionProfile& Profile);
	static bool Remember(CBaseMonster& Observer, const SELObservation& Evidence, const SELPerceptionProfile& Profile);
	static const CELSharedMemory::Records& KnownEvidence(u16 OwnerIdentifier);
	static bool Claim(CBaseMonster& Observer, const SELPerceptionProfile& Profile, SELObservation& Evidence);
	static void Release(u16 OwnerIdentifier, bool Completed);
	static void UpdateCombat(CBaseMonster& Observer, const SELPerceptionProfile& Profile, bool SeesEnemy);
	static void AppendObstacles(u16 OwnerIdentifier, xr_vector<SELNavigationObstacle>& Obstacles);
	static bool Contains(u16 OwnerIdentifier, const Fvector& Position);
	static bool Crosses(u16 OwnerIdentifier, const Fvector& Start, const Fvector& End);
	static void RemoveOwner(u16 OwnerIdentifier);
	static void RemoveObject(u16 ObjectIdentifier);
	static void Clear();
};
