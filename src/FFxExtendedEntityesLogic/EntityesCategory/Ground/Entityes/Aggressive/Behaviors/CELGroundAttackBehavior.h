#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundApproach.h"

class CELGroundAttackBehavior final : public CELGroundBehaviorBase
{
public:
	using CELGroundBehaviorBase::CELGroundBehaviorBase;
	void Enter() override;
	void Update(float DeltaTime) override;

private:
	void UpdateDeadline();
	CELGroundApproach Approach;
	u32 ApproachDeadline = 0;
	u16 DeadlineTarget = u16(-1);
	bool HasRouteEstimate = false;
};
