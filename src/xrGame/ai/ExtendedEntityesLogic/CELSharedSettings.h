#pragma once
#include "Perceptions/CELSharedPerceptionProfiles.h"

struct SELSharedWorkSettings
{
	u32 PerceptionOperations = 384;
	u32 PerceptionRays = 16;
	u32 SpatialQueries = 2;
	u32 SharingSessions = 12;
	u32 SharingTransfers = 32;
	u32 TargetAssignments = 64;
	u32 NavigationSteps = 256;
	u32 NavigationCaptureNodes = 72;
	u32 NavigationSliceMicroseconds = 1500;
	u32 FlightGeometryRays = 350;
	u32 FlightDynamicChecks = 4;
	u32 NavigationTaskLimit = 256;
	u32 NavigationGraphNodeLimit = 16384;
	u32 GroundCaptureTaskLimit = 128;
	u32 GroundCaptureNodeLimit = 4096;
	u32 GroundCaptureTimeout = 20000;
	float GroundRouteReplanDistance = 2.f;
	float GroundWaypointLookahead = 4.f;
	u32 GroundRouteRetryDelay = 250;
};

class CELSharedSettings final
{
public:
	static void Initialize();
	static const SELPerceptionProfile& Profile(const char* Section);
	static bool IsEnabled();
	static const SELSharedWorkSettings& Work();
	static void SetEnabled(int OverrideValue);
	static int GetOverride();
	static u32 Revision();
	static bool CanShareWithFriends();
	static bool CanDistributeTargets();
	static bool CanPlayCalls();
	static void Clear();

private:
	CELSharedSettings() = delete;
};
