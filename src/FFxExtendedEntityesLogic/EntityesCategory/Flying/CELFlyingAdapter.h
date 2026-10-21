#pragma once
#include "FFxExtendedEntityesLogic/CELBaseAdapter.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"

class CELFlyingAdapter final : public CELBaseAdapter
{
public:
	using CELBaseAdapter::CELBaseAdapter;
	bool CanSee(const CEntityAlive& Candidate) const override;
	bool CanReach(const Fvector& TargetPosition, u32 TargetVertex) const override;
	void ReleaseControl() override;

private:
	struct SSightResult
	{
		u32 CheckedAt = 0;
		u32 TargetGeneration = 0;
		bool Visible = false;
	};
	mutable xr_map<u16, SSightResult> SightResults;
};
