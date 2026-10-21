#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttack.h"

class CELFlyingAttackAnimation;

enum class EELAttackPhase : u8
{
	Climb,
	Approach,
	Dive,
	Exit
};

class CELFlyingAttackBehavior final : public CELFlyingBehaviorBase
{
public:
	CELFlyingAttackBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, CELFlyingAttackAnimation& FlightAnimation);
	void Exit() override;
	void Enter() override;
	EELRecovery Recover(const SELBehaviorValidation& Failure) override;
	void SetObservation(const SELObservation* TargetObservation);
	void FrameUpdate(float DeltaTime) override;
	void AfterMovement() override;
	void Update(float DeltaTime) override;
	SELBehaviorValidation Validate() const override;
	const char* ModeDescription() const override { return "АТАКА"; }
	bool IsAttacking() const { return HasTarget && HasVisibleTarget && ActivePhase == EELAttackPhase::Dive && Movement.HasRoute() && !Movement.IsContinuityFlight() && !HasHitPass; }
	bool HasCommittedTarget() const { return HasTarget && (Movement.HasRoute() || Navigation.IsPending() || HasPreparedRoute); }
	u16 CommittedTargetIdentifier() const { return Observation.TargetIdentifier; }

private:
	void ChooseGoal() override;
	void OnRouteActivated() override;
	void OnRouteRejected() override;
	bool CanStreamRoute() const override { return !IsCorrectingDive; }
	bool CanPlanCurve() const override { return FailedPlans < Settings.RecoverySteps; }
	bool CanActivatePreparedRoute() override;
	bool ActivateReplacementRoute(SELNavigationResult& Route) override;
	void ConfigureCurveRequest(SELNavigationRequest& Request) override;
	void RefreshTarget();
	void SelectEntryPhase();
	void CorrectDive();
	const char* FlightDescription() const override;
	void ChooseDeparture(const Fvector& Origin);
	void ChooseApproach(const Fvector& Origin);
	void ChooseContact(const Fvector& Origin);
	CELFlyingAttackAnimation& Animation;
	CELFlyingAttack Attack;
	EELAttackPhase Phase = EELAttackPhase::Climb;
	EELAttackPhase PlannedPhase = EELAttackPhase::Climb;
	EELAttackPhase ActivePhase = EELAttackPhase::Climb;
	Fvector PreviousContact = {0.f, 0.f, 0.f};
	Fmatrix PreviousTargetTransform;
	bool HasContactSample = false;
	Fvector AimPosition = {0.f, 0.f, 0.f};
	Fvector TargetVelocity = {0.f, 0.f, 0.f};
	Fvector PlannedTargetPosition = {0.f, 0.f, 0.f};
	Fvector ActiveTargetPosition = {0.f, 0.f, 0.f};
	Fvector PlannedContact = {0.f, 0.f, 0.f};
	Fvector ContactDirection = {0.f, 0.f, 1.f};
	Fvector ActiveContact = {0.f, 0.f, 0.f};
	Fvector CorrectionStart = {0.f, 0.f, 0.f};
	Fvector CorrectionDirection = {0.f, 0.f, 1.f};
	u32 TargetSampleAt = 0;
	u32 LastVisibleAt = 0;
	u32 LastCorrectionAt = 0;
	u32 FailedPlans = 0;
	float PassRadius = 0.f;
	bool NeedsDifferentEntry = false;
	bool HasVisibleTarget = false;
	bool HasHitPass = false;
	bool IsDivePass = true;
	bool PrefersDivePass = true;
	bool PlannedDivePass = true;
	bool ActiveDivePass = true;
	bool IsCorrectingDive = false;
	u32 CorrectionAttempts = 0;
};
