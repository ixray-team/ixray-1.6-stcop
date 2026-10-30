#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundApproach.h"

class CELGroundAttackBehavior final : public CELGroundBehaviorBase
{
public:
	using CELGroundBehaviorBase::CELGroundBehaviorBase;
	void Enter() override;
	void Exit() override;
	const char* TacticalMovementReason() const { return TacticalMoveReason; }
	bool ExecutingTacticalMove() const { return HasExecutedTacticalMove; }
	void SetTacticalGoal(const Fvector* Goal)
	{
		HasTacticalGoal = Goal != nullptr;
		if (Goal)
		{
			TacticalGoal = *Goal;
		}
	}
	void SetForceDirect(bool Value) { ForceDirect = Value; }
	void Update(float DeltaTime) override;

private:
	void UpdateDeadline();
	CELGroundApproach Approach;
	u32 ApproachDeadline = 0;
	u32 ApproachWindowStartedAt = 0;
	u16 DeadlineTarget = u16(-1);
	bool HasRouteEstimate = false;
	bool HadVision = false;
	bool HasTacticalGoal = false;
	bool ForceDirect = false;
	bool HasExecutedTacticalMove = false;
	const char* TacticalMoveReason = "нет запроса уворота";
	Fvector TacticalGoal = {0.f, 0.f, 0.f};
};
