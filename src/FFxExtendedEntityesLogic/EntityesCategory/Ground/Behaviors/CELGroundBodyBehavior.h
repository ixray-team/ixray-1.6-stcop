#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundInspection.h"

class CELGroundBodyBehavior final : public CELGroundBehaviorBase
{
public:
	CELGroundBodyBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	void Enter() override;
	void Update(float DeltaTime) override;
	void Exit() override;

private:
	CELGroundInspection Inspection;
	u32 InspectUntil = 0;
	bool HasReachedBody = false;
};
