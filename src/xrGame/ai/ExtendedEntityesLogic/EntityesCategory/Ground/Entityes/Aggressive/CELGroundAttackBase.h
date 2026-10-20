#pragma once
#include "../../CELGround.h"
#include "Behaviors/CELGroundAttackBehavior.h"
#include "Behaviors/CELGroundSearchBehavior.h"
class CELGroundAttackBase : public CELGround
{
public:
	CELGroundAttackBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELGroundAttackBase() override;
	void Detach() override;

protected:
	EELControl UpdateTarget(const SELObservation& Observation, float DeltaTime) override;
	CELGroundAttackBehavior AttackBehavior;
	CELGroundSearchBehavior SearchBehavior;
	u32 SuppressedUntil = 0;
	u32 SuppressedObservation = 0;
	u16 SuppressedTarget = u16(-1);
	u16 SearchedTarget = u16(-1);
	u32 SearchedObservation = 0;
};
