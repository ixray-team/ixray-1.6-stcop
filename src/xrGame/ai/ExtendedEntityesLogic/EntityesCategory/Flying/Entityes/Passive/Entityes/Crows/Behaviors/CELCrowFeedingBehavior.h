#pragma once
#include "CELCrowRestBehavior.h"

class CELCrowFeedingBehavior final : public CELCrowRestBehavior
{
public:
	using CELCrowRestBehavior::CELCrowRestBehavior;
	void Enter() override;
	void Update(float DeltaTime) override;
	SELBehaviorValidation Validate() const override;

private:
	bool FeedingContact(Fvector& Contact);
	u32 NextPeck = 0;
};
