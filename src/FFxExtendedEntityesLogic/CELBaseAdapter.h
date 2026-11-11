#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"

class CELBaseAdapter : public IELAdapter
{
public:
	explicit CELBaseAdapter(CBaseMonster& OwnerMonster);
	CBaseMonster& Monster() const override;
	bool CanControl() const override;
	bool IsEnemy(const CEntityAlive& Candidate) const override;
	bool CanSee(const CEntityAlive& Candidate) const override;
	bool CanReach(const Fvector& TargetPosition, u32 TargetVertex) const override;
	bool CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const override;
	void SetFieldEscapePermission(bool Enabled) override { HasFieldEscapePermission = Enabled; }
	void BeginMovementControl() override;
	void EndMovementControl() override;
	void ReleaseControl() override;

protected:
	CBaseMonster& OwnerMonster;
	bool HasMovementControl = false;
	bool HasFieldEscapePermission = false;
};
