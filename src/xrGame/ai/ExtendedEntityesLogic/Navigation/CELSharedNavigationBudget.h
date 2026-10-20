#pragma once
#include "../Interfaces/IELBudget.h"

class CELSharedNavigationBudget final : public IELBudget
{
public:
	void BeginFrame(u32 FrameIdentifier) override;
	bool TryConsume(u32 OperationCount = 1) override;
	bool TryCaptureNode();
	bool TryGeometryRays(u32 RayCount);
	bool TryEmergencyGeometryRays(u32 RayCount);
	u32 GeometryRaysRemaining() const { return RemainingGeometryRays; }
	u32 EmergencyRaysRemaining() const { return RemainingEmergencyRays; }

private:
	u32 LastFrame = u32(-1);
	u32 RemainingOperations = 0;
	u32 RemainingCaptureNodes = 0;
	u32 RemainingGeometryRays = 0;
	u32 RemainingEmergencyRays = 0;
};
