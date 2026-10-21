#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"

class CELNavigationBase : public IELNavigation
{
public:
	~CELNavigationBase() override;
	bool Submit(SELNavigationRequest Request) override;
	bool Poll(SELNavigationResult& Result) override;
	void Cancel() override;
	bool IsPending() const override { return HasPendingRequest; }

protected:
	SELNavigationIdentity PendingIdentity;
	bool HasPendingRequest = false;
};
