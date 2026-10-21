#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlying.h"

class CELFlyingNonAttackingBase : public CELFlying
{
public:
	CELFlyingNonAttackingBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELFlyingNonAttackingBase() override;
	EELControl Update(float DeltaTime) override;
};
