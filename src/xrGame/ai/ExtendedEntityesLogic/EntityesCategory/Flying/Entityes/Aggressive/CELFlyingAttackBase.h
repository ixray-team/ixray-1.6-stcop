#pragma once
#include "../../CELFlying.h"
#include "CELFlyingAttackAnimation.h"
#include "Behaviors/CELFlyingAttackBehavior.h"
#include "Behaviors/CELFlyingRecoveryBehavior.h"
#include "Behaviors/CELFlyingRestBehavior.h"

class CELFlyingAttackBase : public CELFlying
{
public:
	CELFlyingAttackBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELFlyingAttackBase() override;
	EELControl Update(float DeltaTime) override;

protected:
	const SELObservation* SelectTarget() override;
	bool IsAttacking() const override;
	CELFlyingBehaviorBase* ChooseBehavior(const SELObservation* Observation, float DeltaTime);
	void HandleFailure(CELFlyingBehaviorBase* RequestedBehavior);
	CELFlyingAttackAnimation Animation;
	CELFlyingAttackBehavior AttackBehavior;
	CELFlyingRecoveryBehavior RecoveryBehavior;
	CELFlyingRestBehavior RestBehavior;
	float AirborneTime = 0.f;
	u32 NextRestAttempt = 0;
	u32 RetryAfter = 0;
};
