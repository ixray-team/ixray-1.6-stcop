#pragma once
#include "CELGroundBehaviorBase.h"

class CELGroundReactionBehavior final : public CELGroundBehaviorBase
{
public:
	using CELGroundBehaviorBase::CELGroundBehaviorBase;
	void Configure(const Fvector& SoundPosition, u32 ReactionUntil, bool ShouldFlee);
	void Enter() override;
	void Update(float DeltaTime) override;

private:
	bool SelectPoint();
	Fvector SourcePosition = {0.f, 0.f, 0.f};
	Fvector PlannedSourcePosition = {0.f, 0.f, 0.f};
	Fvector ReactionPosition = {0.f, 0.f, 0.f};
	u32 ReactionVertex = u32(-1);
	u32 Until = 0;
	u32 NextPoint = 0;
	u32 SearchPhase = 0;
	u32 PointStartedAt = 0;
	bool IsFleeing = false;
	bool HasPoint = false;
};
