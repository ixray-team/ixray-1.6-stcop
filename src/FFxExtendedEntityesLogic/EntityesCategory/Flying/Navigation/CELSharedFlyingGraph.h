#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"

// Captures checked air corridors in the game phase; no live geometry reaches the worker.
class CELSharedFlyingGraph final
{
public:
	static bool Submit(SELNavigationRequest Request, float Radius, float CenterHeight, float CellSize, u32 NodeLimit, u32 Timeout);
	static EELGeometryResult Take(const SELNavigationIdentity& Identity, SELNavigationRequest& Request);
	static void Cancel(const SELNavigationIdentity& Identity);
	static bool BeginFrame();
	static bool HasFrameBudget();
	static void Clear();
	static u32 OutstandingRequests();

private:
	struct SState;
	static SState& State();
	CELSharedFlyingGraph() = delete;
};
