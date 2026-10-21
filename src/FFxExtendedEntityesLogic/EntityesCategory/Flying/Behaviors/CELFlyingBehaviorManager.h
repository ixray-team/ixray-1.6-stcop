#pragma once
#include "FFxExtendedEntityesLogic/CoreComponent/CELSharedBehaviorManager.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELLogic.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingCruiseBehavior.h"
class CELFlying;

class CELFlyingBehaviorManager final : public CELSharedBehaviorManager
{
public:
	CELFlyingBehaviorManager(CELFlying& OwnerLogic, IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement);
	~CELFlyingBehaviorManager() override;
	EELControl UpdateFlight(float DeltaTime, CELFlyingBehaviorBase* DesiredBehavior = nullptr);
	void SelectFlight(CELFlyingBehaviorBase& Behavior);
	void FrameUpdate(float DeltaTime);
	void AfterMovement();
	void Clear();
	Fvector Goal() const;
	SELBehaviorValidation FlightValidation() const;
	const char* FlightMode() const;
	u32 PendingRouteAge() const;
	u32 LastRouteReadyMilliseconds() const;

private:
	CELFlying& Owner;
	CELFlyingMovement& Movement;
	CELFlyingCruiseBehavior CruiseBehavior;
	CELFlyingBehaviorBase* ActiveFlight = nullptr;
};
