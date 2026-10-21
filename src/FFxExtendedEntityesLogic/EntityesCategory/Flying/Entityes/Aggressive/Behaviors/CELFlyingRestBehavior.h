#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"

class CELFlyingAttackAnimation;

class CELFlyingRestBehavior final : public CELFlyingBehaviorBase
{
public:
	CELFlyingRestBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, CELFlyingAttackAnimation& FlightAnimation);
	void Enter() override;
	void Update(float DeltaTime) override;
	SELBehaviorValidation Validate() const override;
	EELRecovery Recover(const SELBehaviorValidation& Failure) override;
	void Exit() override;
	bool IsResting() const;

private:
	enum class EPhase : u8
	{
		FindSupport,
		Approach,
		Touchdown,
		Resting
	};
	bool FindSupport();
	void UpdateRest();

	void ChooseGoal() override;
	void OnRouteActivated() override;
	bool CanPlanCurve() const override;
	bool CanStreamRoute() const override { return false; }
	CELFlyingAttackAnimation& Animation;
	Fvector Contact = {0.f, 0.f, 0.f};
	Fvector Facing = {0.f, 0.f, 1.f};
	EPhase Phase = EPhase::FindSupport;
	bool HasActiveLeg = false;
	u32 RestUntil = 0;
	u32 NextSupportCheck = 0;
};
