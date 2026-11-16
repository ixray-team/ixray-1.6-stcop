#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundInspection.h"

class CELGroundSearchBehavior final : public CELGroundBehaviorBase
{
public:
	CELGroundSearchBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	void Exit() override;
	void Enter() override;
	void Update(float DeltaTime) override;

private:
	void ResolveAnchor();
	CELGroundInspection Inspection;
	u32 SearchUntil = 0;
	u32 TravelUntil = 0;
	bool HasReachedAnchor = false;
	bool HasCheckedForward = false;
	u32 AnchorObservationAt = 0;
	u32 TotalUntil = 0;
	Fvector SearchAnchor = {0.f, 0.f, 0.f};
	u32 NextPoint = 0;
	u32 SearchPhase = 0;
	Fvector SearchPosition = {0.f, 0.f, 0.f};
	u32 SearchVertex = u32(-1);
};
