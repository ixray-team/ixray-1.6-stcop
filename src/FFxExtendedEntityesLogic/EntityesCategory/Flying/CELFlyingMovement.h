#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELMovement.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
class IELAdapter;

class CELFlyingMovement final : public IELMovement
{
public:
	CELFlyingMovement(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings);
	~CELFlyingMovement() override;
	bool MoveTo(const Fvector& TargetPosition, u32 TargetVertex, bool IsRunning) override;
	void Stop() override;
	void SetRoute(SELNavigationResult Route, bool IsContinuity = false);
	u32 NextPlanGeneration() { return ++PlanGeneration; }
	bool IsContinuityFlight() const { return HasContinuityRoute; }
	bool IsStalled() const;
	bool AppendRouteTail(const SELNavigationResult& Route);
	void CloseRouteStream(const SELNavigationIdentity& Identity);
	float RemainingDistance() const;
	bool IsAwaitingTail() const { return ActiveRoute.IsPartial; }
	void RetainRoutePrefix(float Distance);
	bool PreviewHandoff(float Distance, Fvector& Position, Fvector& Direction) const;
	bool CanSpliceAt(const Fvector& Position) const;
	bool SpliceRoute(SELNavigationResult Route);
	void SetContactTarget(u16 Identifier) { ContactTarget = Identifier; }
	void SetContactPosition(const Fvector& Position, const Fvector& Direction)
	{
		ContactPosition = Position;
		FlightContactDirection = Direction;
	}
	void AcknowledgeObstacle() { HasDynamicObstacle = false; }
	const char* StopReason() const { return LastStopReason; }
	bool IsLandingFlight() const { return IsLanding; }
	bool WasBlocked() const { return HasDynamicObstacle; }
	void AppendObstacles(xr_vector<SELNavigationObstacle>& Obstacles) const;
	const SELFlyingSettings& GetSettings() const { return Settings; }
	void Update(float DeltaTime);
	float UnusedTime() const { return RemainingTime; }
	void SetSpeedScale(float Scale);
	void SetConfined(bool Enabled) { IsConfined = Enabled; }
	bool InConfinedSpace() const { return IsConfined; }
	float PlanningSpeed() const { return Settings.Speed * SpeedScale * (IsConfined ? .4f : 1.f); }
	void SetLanding(bool Enabled) { IsLanding = Enabled; }
	bool HasRoute() const;
	float Progress() const;
	float GetSpeed() const { return Velocity.magnitude(); }
	float TransitionBodyRadius() const;
	Fvector Direction() const;
	const Fvector& EndPosition() const;
	const Fvector& EndDirection() const;

private:
	void PublishObstacleProbe();
	void Orient(const Fvector& Direction, const Fvector& NewPosition, float DeltaTime);
	IELAdapter& Adapter;
	const SELFlyingSettings& Settings;
	SELNavigationResult ActiveRoute;
	Fvector Velocity = {0.f, 0.f, 0.f};
	Fvector ContactPosition = {0.f, 0.f, 0.f};
	Fvector FlightContactDirection = {0.f, 0.f, 1.f};
	Fvector FinalDirection = {0.f, 0.f, 1.f};
	u32 Segment = 1;
	u32 PlanGeneration = 0;
	u32 SlowSince = 0;
	bool HasContinuityRoute = false;
	float RemainingTime = 0.f;
	float Speed = 0.f;
	float SpeedScale = 1.f;
	bool IsConfined = false;
	bool IsLanding = false;
	bool IsBraking = false;
	bool HasDynamicObstacle = false;
	u16 ContactTarget = u16(-1);
	u32 ObstacleUntil = 0;
	const char* LastStopReason = "Маршрут ещё не назначен";
	SELNavigationObstacle LastObstacle;
};
