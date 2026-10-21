#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"

enum class EELCrowFlightMode : u8
{
	Roam,
	Escape,
	Circle,
	Investigate
};

class CELCrowCruiseBehavior final : public CELFlyingBehaviorBase
{
public:
	CELCrowCruiseBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, const SELCrowSettings& CrowSettings, SELCrowState& CrowState);
	void Enter() override;
	void Configure(EELCrowFlightMode NewMode, const Fvector& Anchor, bool IsTakingOff = false);

private:
	void ChooseGoal() override;
	void OnRouteActivated() override;
	const SELCrowSettings& CrowSettings;
	SELCrowState& State;
	EELCrowFlightMode Mode = EELCrowFlightMode::Roam;
	Fvector Home = {0.f, 0.f, 0.f};
	Fvector Anchor = {0.f, 0.f, 0.f};
	Fvector Destination = {0.f, 0.f, 0.f};
	float CircleAngle = 0.f;
	bool HasDestination = false;
	bool NeedsTakeoff = false;
	u32 NextHeightChange = 0;
};
