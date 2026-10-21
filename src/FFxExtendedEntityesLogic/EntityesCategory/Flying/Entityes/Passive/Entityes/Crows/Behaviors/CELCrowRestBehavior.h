#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"
class IELAdapter;
class CELCrowAnimation;
class CELFlyingMovement;

class CELCrowRestBehavior : public IELBehavior
{
public:
	CELCrowRestBehavior(IELAdapter& OwnerAdapter, CELFlyingMovement& FlightMovement, CELCrowAnimation& Animation, const SELCrowSettings& CrowSettings, SELCrowState& CrowState);
	void Enter() override;
	void Update(float DeltaTime) override;
	void FrameUpdate(float DeltaTime);
	SELBehaviorValidation Validate() const override;
	EELRecovery Recover(const SELBehaviorValidation& Failure) override;
	void Exit() override;

protected:
	bool PrepareStep();
	bool QueueStep();
	void PollStep();
	void CancelStep();
	IELAdapter& Adapter;
	CELFlyingMovement& Movement;
	CELCrowAnimation& Animation;
	const SELCrowSettings& Settings;
	SELCrowState& State;
	Fvector StepStart = {0.f, 0.f, 0.f};
	Fvector StepGoal = {0.f, 0.f, 0.f};
	float StepProgress = 1.f;
	float StepDuration = 1.f;
	u32 NextStep = 0;
	u32 StartedAt = 0;
	SELNavigationIdentity StepIdentity;
	u32 StepGeneration = 0;
	bool HasPendingStep = false;
	SELBehaviorValidation Validation;
};
