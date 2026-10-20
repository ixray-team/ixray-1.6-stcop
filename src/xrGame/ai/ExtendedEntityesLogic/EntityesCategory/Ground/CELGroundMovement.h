#pragma once
#include "../../Interfaces/IELMovement.h"
class IELAdapter;

class CELGroundMovement final : public IELMovement
{
public:
	explicit CELGroundMovement(IELAdapter& OwnerAdapter);
	bool MoveTo(const Fvector& TargetPosition, u32 TargetVertex, bool IsRunning) override;
	void Stop() override;

private:
	IELAdapter& Adapter;
};
