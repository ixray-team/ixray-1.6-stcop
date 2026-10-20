#pragma once
#include "../../../Navigation/CELNavigationBase.h"
class IELAdapter;

class CELGroundNavigation final : public CELNavigationBase
{
public:
	explicit CELGroundNavigation(IELAdapter& OwnerAdapter);
	~CELGroundNavigation() override;
	bool Prepare(const Fvector& GoalPosition, u32 GoalVertex, u32 PlanGeneration);
	bool Poll(SELNavigationResult& Result) override;
	void Cancel() override;
	void Reset();

private:
	IELAdapter& Adapter;
	bool HasCapture = false;
};
