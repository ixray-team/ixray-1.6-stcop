#pragma once

class CBaseMonster;
class CEntityAlive;

class IELAdapter
{
public:
	virtual ~IELAdapter() = default;
	virtual CBaseMonster& Monster() const = 0;
	virtual bool CanControl() const = 0;
	virtual bool IsEnemy(const CEntityAlive& Candidate) const = 0;
	virtual bool CanSee(const CEntityAlive& Candidate) const = 0;
	virtual bool CanReach(const Fvector& TargetPosition, u32 TargetVertex) const = 0;
	virtual bool CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const = 0;
	virtual void ReleaseControl() = 0;
};
