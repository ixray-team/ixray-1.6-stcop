#pragma once
#include "../Interfaces/IELNavigation.h"
class CBaseMonster;
struct SELPerceptionProfile;

class CELSharedAnomalies final
{
public:
	static void Observe(CBaseMonster& OwnerMonster, const SELPerceptionProfile& Profile);
	static bool Contains(u16 OwnerIdentifier, const Fvector& Position);
	static bool Crosses(u16 OwnerIdentifier, const Fvector& StartPosition, const Fvector& EndPosition);
	static bool Crosses(const xr_vector<SELNavigationObstacle>& Fields, const Fvector& StartPosition, const Fvector& EndPosition);
	static void Snapshot(u16 OwnerIdentifier, xr_vector<SELNavigationObstacle>& Result);
	static void Remove(u16 OwnerIdentifier);
	static void Clear();
};
