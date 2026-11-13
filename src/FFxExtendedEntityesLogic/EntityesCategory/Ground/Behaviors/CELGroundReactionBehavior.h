#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundInspection.h"

class CELGroundReactionBehavior final : public CELGroundBehaviorBase
{
public:
	CELGroundReactionBehavior(IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	void ConfigureObstacle(const Fvector& EnemyPosition, u32 WaitUntil);
	void Exit() override;
	void Configure(const Fvector& SoundPosition, u32 ReactionUntil, bool ShouldFlee);
	void ConfigureWatching(const Fvector& EnemyPosition, const Fvector& EnemyForward, u32 WatchUntil, bool Visible = true, const Fvector* Eye = nullptr);
	void SetEvasionGoal(const Fvector* Goal);
	bool EvasionBlocked() const { return EvasionHardBlocked; }
	void Enter() override;
	void Update(float DeltaTime) override;
	bool IsCornered() const;

private:
	bool SelectPoint();
	bool SelectWatchPoint();
	bool SelectObstaclePoint();
	CELGroundInspection Inspection;
	bool IsObstaclePatrol = false;
	bool IsInspectingPoint = false;
	Fvector EvasionGoal = {0.f, 0.f, 0.f};
	Fvector PreviousWatchPoint = {0.f, 0.f, 0.f};
	Fvector OlderWatchPoint = {0.f, 0.f, 0.f};
	u32 PreviousWatchAt = 0;
	u32 OlderWatchAt = 0;
	float WatchSide = 1.f;
	bool WatchVisible = false;
	bool HasEvasionGoal = false;
	bool EvasionHardBlocked = false;
	float WatchEyeHeight = 0.f;
	Fvector SourcePosition = {0.f, 0.f, 0.f};
	Fvector PlannedSourcePosition = {0.f, 0.f, 0.f};
	Fvector ReactionPosition = {0.f, 0.f, 0.f};
	u32 ReactionVertex = u32(-1);
	u32 Until = 0;
	u32 EscapeBlockedSince = 0;
	u32 NextPoint = 0;
	u32 SearchPhase = 0;
	u32 PointStartedAt = 0;
	Fvector WatchedForward = {0.f, 0.f, 1.f};
	bool IsWatching = false;
	bool IsFleeing = false;
	bool HasPoint = false;
};
