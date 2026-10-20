#pragma once
#include "Interfaces/IELBudget.h"

class CELSharedPerceptionBudget final : public IELBudget
{
public:
	void BeginFrame(u32 FrameIdentifier) override;
	bool TryConsume(u32 OperationCount = 1) override;
	bool TryRay();
	bool TrySpatial();
	bool TrySpatial(u16 OwnerIdentifier);
	bool TrySharing();
	bool TryTransfer();
	void Clear();

private:
	struct SSpatialWaiter
	{
		u16 Identifier;
		u32 Generation;
		u32 RequestedAt;
	};
	xr_vector<SSpatialWaiter> SpatialWaiters;
	u32 LastFrame = u32(-1);
	u32 RemainingOperations = 0;
	u32 RemainingRays = 0;
	u32 RemainingSpatial = 0;
	u32 RemainingSessions = 0;
	u32 RemainingTransfers = 0;
};
