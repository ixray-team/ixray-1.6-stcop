#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELBehavior.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingMovement.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"
class IELAdapter;

class CELCrowLandingBehavior final : public IELBehavior
{
public:
	CELCrowLandingBehavior(IELAdapter& OwnerAdapter, CELFlyingMovement& FlightMovement, const SELFlyingSettings& FlightSettings, const SELCrowSettings& CrowSettings);
	void Configure(const Fvector& Point, const Fvector& Facing);
	void Enter() override;
	void Update(float DeltaTime) override;
	SELBehaviorValidation Validate() const override;
	EELRecovery Recover(const SELBehaviorValidation& Failure) override;
	void Exit() override;
	bool HasLanded() const { return Phase == 2; }
	const Fvector& Goal() const { return Contact; }

private:
	bool Request();
	IELAdapter& Adapter;
	CELFlyingMovement& Movement;
	const SELFlyingSettings& FlightSettings;
	const SELCrowSettings& Settings;
	CELFlyingNavigation Navigation;
	SELNavigationResult Prepared;
	Fvector Contact = {0.f, 0.f, 0.f};
	Fvector Facing = {0.f, 0.f, 1.f};
	Fvector ProgressPosition = {0.f, 0.f, 0.f};
	u32 StartedAt = 0;
	u32 ProgressAt = 0;
	u32 Phase = 0;
	bool HasActiveRoute = false;
	SELBehaviorValidation Validation;
};
