#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
class IELAdapter;

class CELGroundApproach
{
public:
	void Reset();
	void Select(IELAdapter& Adapter, const SELPerceptionProfile& Profile, const Fvector& KnownPosition, bool HasVision);
	const Fvector& Position() const { return GoalPosition; }
	u32 Vertex() const { return GoalVertex; }
	bool Runs() const { return IsRunning; }
	const char* Description() const;

private:
	static u32 GatherPeers(IELAdapter& Adapter, const Fvector& KnownPosition, Fvector& Separation, float CrowdDistance);
	void ChooseStrategy(IELAdapter& Adapter, const SELPerceptionProfile& Profile, float Distance, bool HasVision, u32 NearbyCount, const Fvector& KnownPosition);
	Fvector GoalPosition = {0.f, 0.f, 0.f};
	Fvector LastKnownPosition = {0.f, 0.f, 0.f};
	u32 GoalVertex = u32(-1);
	EELApproachStrategy Strategy = EELApproachStrategy::Direct;
	u32 Phase = 0;
	u32 ReconsiderAt = 0;
	bool IsRunning = false;
};
