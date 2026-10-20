#pragma once
#include "../../../Interfaces/IELNavigation.h"

struct SELFlyingCorridor
{
	SELNavigationIdentity Identity;
	Fvector Start = {0.f, 0.f, 0.f};
	Fvector End = {0.f, 0.f, 0.f};
	float Radius = 0.f;
	float CenterHeight = 0.f;
	u16 ContactTarget = u16(-1);
	bool IsLanding = false;
};
class CELSharedFlyingObstacles final
{
public:
	static void Publish(const SELFlyingCorridor& Corridor);
	static bool IsBlocked(const SELNavigationIdentity& Identity, SELNavigationObstacle& Obstacle, float& ClearDistance);
	static void Cancel(const SELNavigationIdentity& Identity);
	static void BeginFrame();
	static void Clear();
	static u32 OutstandingRequests();

private:
	struct SState;
	static SState& State();
	CELSharedFlyingObstacles() = delete;
};
