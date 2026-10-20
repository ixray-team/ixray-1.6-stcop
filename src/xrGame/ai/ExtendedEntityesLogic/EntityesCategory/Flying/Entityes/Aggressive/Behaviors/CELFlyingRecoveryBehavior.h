#pragma once
#include "../../../Behaviors/CELFlyingBehaviorBase.h"

class CELFlyingRecoveryBehavior final : public CELFlyingBehaviorBase
{
public:
	using CELFlyingBehaviorBase::CELFlyingBehaviorBase;
	void Enter() override;
	void Update(float DeltaTime) override;
	EELRecovery Recover(const SELBehaviorValidation& Failure) override;
	bool ReadyToResume() const;
	const char* ModeDescription() const override { return "ВОССТАНОВЛЕНИЕ ПОЛЁТА"; }

private:
	void ChooseGoal() override;
	bool CanStreamRoute() const override { return false; }
	bool CentersUnderCover() const override { return true; }
	bool CanPlanCurve() const override { return !HasDeparture; }
	void OnRouteActivated() override;
	u32 Attempt = 0;
	bool HasDeparture = false;
};
