#pragma once
#include "../../CELBaseAdapter.h"

class CELGroundAdapter final : public CELBaseAdapter
{
public:
	explicit CELGroundAdapter(CBaseMonster& OwnerMonster);
	bool CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const override;

private:
	float JumpHeight = 0.f;
};
