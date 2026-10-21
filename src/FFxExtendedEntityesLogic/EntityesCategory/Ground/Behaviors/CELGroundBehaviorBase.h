#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELBehavior.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundMovement.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundLocalSearch.h"
class IELAdapter;

class CELGroundBehaviorBase : public IELBehavior
{
public:
	CELGroundBehaviorBase(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	void SetObservation(const SELObservation& Observation);
	void Enter() override;
	void Exit() override;
	SELBehaviorValidation Validate() const override;
	EELRecovery Recover(const SELBehaviorValidation& Validation) override;

protected:
	bool Move(const Fvector& GoalPosition, u32 GoalVertex, bool IsRunning);
	void TrackProgress();
	void TakePreparedRoute();
	void AdvanceRoute();
	void PrepareContinuation(const Fvector& GoalPosition, u32 GoalVertex);
	bool FollowRoute(bool IsRunning);
	IELAdapter& Adapter;
	const SELPerceptionProfile& Profile;
	CELGroundMovement Movement;
	CELGroundNavigation Navigation;
	CELGroundLocalSearch LocalSearch;
	SELNavigationResult Route;
	SELObservation Observation;
	Fvector RequestedGoal = {0.f, 0.f, 0.f};
	bool HasRequestedGoal = false;
	Fvector ProgressPosition = {0.f, 0.f, 0.f};
	u32 ProgressTime = 0;
	u32 WaitingSince = 0;
	u32 StartedAt = 0;
	u32 ActionUntil = 0;
	u32 LastObservationTime = 0;
	u32 RouteCursor = 0;
	u32 PlanGeneration = 0;
	u32 RecoveryCount = 0;
	u32 RetryAfter = 0;
	bool HasMoveCommand = false;
	SELBehaviorValidation Validation;
};
