#pragma once

enum class EELFlightManeuver : u8
{
	Glide,
	Arch,
	Wave,
	Weave,
	Spiral,
	Loop,
	Count
};

struct SELFlyingSettings
{
	bool CanWander = true;
	float Speed = 8.f;
	float Acceleration = 8.f;
	float TurningSpeedRatio = .6f;
	float OrientationResponse = 12.f;
	float TurnRate = 2.5f;
	float ClimbSpeedRatio = .75f;
	float DiveSpeedRatio = 1.25f;
	float Altitude = 10.f;
	float WanderHeightMinimum = 5.f;
	float WanderHeightMaximum = 16.f;
	float WanderDistanceMinimum = 12.f;
	float WanderDistanceMaximum = 40.f;
	float WanderTurnAngle = PI_DIV_2;
	u32 WanderManeuvers = (1u << u32(EELFlightManeuver::Count)) - 1;
	float ManeuverRadiusMinimum = 4.f;
	float ManeuverRadiusMaximum = 7.f;
	float OrbitRadius = 14.f;
	float CollisionRadius = .35f;
	float CollisionCenterHeight = .4f;
	float AttackDistance = .35f;
	float AttackTargetScale = 1.3f;
	float AttackDamage = .15f;
	float AttackImpulse = 10.f;
	u32 AttackCooldown = 2500;
	u32 AttackIndicatorTime = 200;
	float BoundaryMargin = 2.f;
	float CurveRatio = .65f;
	float CurveMinimum = 1.f;
	float CurveMaximum = 30.f;
	float ProgressDistance = .5f;
	u32 ProgressTimeout = 8000;
	u32 RouteWaitTimeout = 10000;
	u32 RouteRetryDelay = 250;
	float SearchCellSize = 4.f;
	u32 SearchNodeLimit = 128;
	u32 SearchTimeout = 8000;
	u32 RecoverySteps = 6;
	float RecoveryForward = 3.f;
	float RecoveryClimb = 2.f;
	float RecoveryStep = 1.f;
	float AttackApproachRatio = .6f;
	float AttackApproachHeightRatio = .6f;
	float AttackPulloutPitch = .2f;
	float AttackReplanDistance = .5f;
	float AttackLeadTime = 2.f;
	float AttackDiveProbability = .4f;
	float AttackArcSideRatio = .65f;
	float GroundClearance = .1f;
	float LandingSpeedMinimum = .35f;
	float MaximumBank = .6f;
	float InterruptLookahead = .75f;
	float ObstacleLookahead = .75f;
	u32 ObstacleMemory = 2000;
	float RestInterval = 120.f;
	float RestDuration = 6.f;
	u32 RestRetryDelay = 15000;
	u32 RestSupportInterval = 500;
	float LandingApproachDistance = 3.f;
	float LandingHeight = 1.5f;
	float LandingMinimumNormalY = .70710678f;
	float LandingHeightTolerance = .2f;
	shared_str AttackAnimation;
	shared_str FlightAnimation;
	shared_str IdleAnimation;
};

class CELFlyingSettings final
{
public:
	static const SELFlyingSettings& Read(const char* Section);
	static void Clear();
};
