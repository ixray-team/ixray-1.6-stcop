#pragma once
#include "../../../Interfaces/IELBehavior.h"
#include "../../../Perceptions/CELSharedMemory.h"
#include "../CELFlyingMovement.h"
#include "../Navigation/CELFlyingNavigation.h"
#include "../Navigation/CELFlyingContinuity.h"
class IELAdapter;

class CELFlyingBehaviorBase : public IELBehavior
{
public:
	CELFlyingBehaviorBase(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement);
	void Enter() override;
	void Update(float DeltaTime) override;
	SELBehaviorValidation Validate() const override;
	EELRecovery Recover(const SELBehaviorValidation& Validation) override;
	void Exit() override;
	virtual void FrameUpdate(float DeltaTime);
	virtual void AfterMovement();
	void SetObservation(const SELObservation* TargetObservation);
	const Fvector& Goal() const { return GoalPosition; }
	u32 PendingRouteAge() const;
	u32 LastReadyMilliseconds() const { return LastRouteReadyMilliseconds; }
	virtual const char* ModeDescription() const { return "ВОЗДУШНАЯ СХЕМА"; }

protected:
	virtual void ChooseGoal() = 0;
	virtual const char* FlightDescription() const { return "Полёт"; }
	virtual bool CanStreamRoute() const { return true; }
	virtual bool CanPlanCurve() const { return true; }
	virtual void OnRouteActivated();
	virtual void OnRouteRejected() {}
	virtual void ConfigureCurveRequest(SELNavigationRequest& Request) {}
	virtual bool CanActivatePreparedRoute() { return true; }
	virtual bool ActivateReplacementRoute(SELNavigationResult& Route) { return false; }
	bool RequestCurve();
	void CancelRouteRequest();
	virtual bool CentersUnderCover() const { return true; }
	virtual bool LimitsGoalUnderCover() const { return true; }
	void PrepareNextRoute();
	void ActivatePreparedRoute();
	void MaintainContinuity();
	IELAdapter& Adapter;
	const SELFlyingSettings& Settings;
	CELFlyingMovement& Movement;
	CELFlyingNavigation Navigation;
	CELFlyingContinuity Continuity;
	SELObservation Observation;
	SELNavigationResult PreparedRoute;
	Fvector GoalPosition = {0.f, 0.f, 0.f};
	Fvector GoalDirection = {0.f, 0.f, 1.f};
	Fvector ProgressPosition = {0.f, 0.f, 0.f};
	Fvector PendingContact = {0.f, 0.f, 0.f};
	u32 PreparedAt = 0;
	u32 ProgressTime = 0;
	u32 RetryAfter = 0;
	u32 RecoveryCount = 0;
	u32 RequestedAt = 0;
	u32 LastRouteReadyMilliseconds = 0;
	bool IsRecovering = false;
	bool HasTarget = false;
	bool HasPreparedRoute = false;
	bool HasPendingContact = false;
	SELBehaviorValidation Validation;
};
