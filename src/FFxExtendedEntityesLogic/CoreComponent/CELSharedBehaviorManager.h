#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELBehavior.h"

class CELSharedBehaviorManager
{
public:
	virtual ~CELSharedBehaviorManager();
	void Select(IELBehavior* Behavior);
	void Update(float DeltaTime);
	void Clear();
	bool HasActiveBehavior() const { return ActiveBehavior != nullptr; }
	bool IsActive(const IELBehavior& Behavior) const { return ActiveBehavior == &Behavior; }
	const SELBehaviorValidation& GetValidation() const { return Validation; }

private:
	IELBehavior* ActiveBehavior = nullptr;
	SELBehaviorValidation Validation;
};
