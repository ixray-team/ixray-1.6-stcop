#pragma once
#include "CELFlyingBehaviorBase.h"

class CELFlyingCruiseBehavior final : public CELFlyingBehaviorBase
{
public:
	using CELFlyingBehaviorBase::CELFlyingBehaviorBase;
	void Enter() override;
	void Update(float DeltaTime) override;
	void SetAttention(const Fvector& Position, bool IsEscape);
	void ClearAttention();
	const char* ModeDescription() const override { return HasAttention ? (IsEscaping ? "ПАНИКА" : "ИНТЕРЕС К ЗВУКУ") : "БЛУЖДАНИЕ"; }

private:
	const char* FlightDescription() const override;
	void ConfigureCurveRequest(SELNavigationRequest& Request) override;
	void OnRouteActivated() override;
	void ChooseManeuver(const Fvector& Origin);
	bool BuildLoop(const Fvector& Origin, float Radius);
	void BuildVariation(const Fvector& Origin, float Radius);
	void ChooseGoal() override;
	bool LimitsGoalUnderCover() const override { return !HasAttention || IsEscaping; }
	void ChooseWander(const Fvector& Origin);
	void ChooseAttention(const Fvector& Origin);
	void ApplyHeight(const Fvector& Origin);
	void ConstrainGoal();
	float WanderHeading = 0.f;
	Fvector AttentionPosition = {0.f, 0.f, 0.f};
	bool HasAttention = false;
	bool IsEscaping = false;
	EELFlightManeuver PlannedManeuver = EELFlightManeuver::Glide;
	EELFlightManeuver ActiveManeuver = EELFlightManeuver::Glide;
	xr_vector<Fvector> ManeuverPoints;
	float InvertedBodyRadius = 0.f;
	float ManeuverCurveDistance = 0.f;
};
