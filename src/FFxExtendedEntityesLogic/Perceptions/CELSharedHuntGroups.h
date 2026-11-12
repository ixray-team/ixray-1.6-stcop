#pragma once
class CBaseMonster;
struct SELHuntGroup
{
	u32 Count = 1;
	u32 Rank = 0;
	Fvector Separation = {0.f, 0.f, 0.f};
	Fvector AllyPosition = {0.f, 0.f, 0.f};
	bool HasAlly = false;
	u32 AllyAttackStartedAt = 0;
	bool HasBlockingPosition = false;
	Fvector BlockingPosition = {0.f, 0.f, 0.f};
};

class CELSharedHuntGroups
{
public:
	static void CaptureCompanions(CBaseMonster& Owner, float Radius, xr_map<u16, u32>& Members);
	static u32 CountCompanions(CBaseMonster& Owner, float Radius);
	static SELHuntGroup Observe(CBaseMonster& Owner, u16 TargetIdentifier, float Radius);
	static void ReportPosition(CBaseMonster& Owner, const Fvector& Position, u16 TargetIdentifier, bool IsBlocking);
	static void ReportStamina(CBaseMonster& Owner, float Stamina);
	static void Clear();
};
