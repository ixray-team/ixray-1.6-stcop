#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGround.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundCombatTactics.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundAttackBehavior.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundSearchBehavior.h"
class CELGroundAttackBase : public CELGround
{
public:
	CELGroundAttackBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELGroundAttackBase() override;
	void Detach() override;

protected:
	void FillGroundDebug(SELDebugRecord& Record) const override;
	EELControl UpdateTarget(const SELObservation& Observation, float DeltaTime) override;
	CELGroundCombatTactics Tactics;
	CELGroundAttackBehavior AttackBehavior;
	CELGroundSearchBehavior SearchBehavior;
	u32 NextAttackTrace = 0;
	u32 SuppressedUntil = 0;
	u32 SuppressedObservation = 0;
	u16 SuppressedTarget = u16(-1);
	u16 SearchedTarget = u16(-1);
	u32 SearchedObservation = 0;
};
