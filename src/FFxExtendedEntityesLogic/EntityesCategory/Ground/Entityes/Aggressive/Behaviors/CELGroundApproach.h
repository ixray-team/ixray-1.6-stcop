#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
class IELAdapter;
class CEntityAlive;

class CELGroundApproach
{
public:
	void Reset();
	void Select(IELAdapter& Adapter, const SELPerceptionProfile& Profile, const Fvector& KnownPosition, bool HasVision, CEntityAlive* Target, bool ForceDirect = false);
	const Fvector& Position() const { return GoalPosition; }
	u32 Vertex() const { return GoalVertex; }
	bool Runs() const { return IsRunning; }
	const char* Description() const;
	u32 Rank() const { return GroupRank; }
	u32 Count() const { return GroupCount; }

private:

	void ChooseStrategy(IELAdapter& Adapter, const SELPerceptionProfile& Profile, float Distance, bool HasVision, u32 NearbyCount, const Fvector& KnownPosition);
	Fvector GoalPosition = {0.f, 0.f, 0.f};
	Fvector LastKnownPosition = {0.f, 0.f, 0.f};
	u32 GoalVertex = u32(-1);
	EELApproachStrategy Strategy = EELApproachStrategy::Direct;
	u32 ReconsiderAt = 0;
	bool IsRunning = false;
	u32 GroupRank = 0;
	u32 GroupCount = 1;
	bool IsObserved = false;
	Fvector AllyPosition = {0.f, 0.f, 0.f};
	bool HasAlly = false;
	Fvector ObservedVelocity = {0.f, 0.f, 0.f};
	u32 LastSampleAt = 0;
	u16 ObservedTarget = u16(-1);
	bool IsDrivingToAllies = false;
	bool IsIntercepting = false;
	bool HasConcealedPosition = false;
};
