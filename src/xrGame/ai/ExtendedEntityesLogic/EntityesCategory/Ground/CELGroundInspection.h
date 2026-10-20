#pragma once
class CBaseMonster;

class CELGroundInspection
{
public:
	explicit CELGroundInspection(CBaseMonster& OwnerMonster);
	bool Update(bool Enabled);
	void Clear();

private:
	CBaseMonster& Owner;
	u32 Until = 0;
	u32 NextInspection = 0;
	bool HasOverride = false;
};
