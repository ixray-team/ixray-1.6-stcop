#pragma once
#include "xrEngine/AI/alife_space.h"
class CBaseMonster;
class CObject;
class CEntityAlive;

enum class EELSightResult : u8
{
	Visible,
	Occluded,
	WaitingForBudget
};

class CELSharedSight final
{
public:
	static EELSightResult CheckClose(CBaseMonster& OwnerMonster, CObject& Candidate);
	static bool HasCloseContact(CBaseMonster& OwnerMonster, const CEntityAlive& Candidate);
	static void Remove(ALife::_OBJECT_ID Identifier);
	static void Clear();
	static void KeepEvasionTarget(CBaseMonster& Owner, CEntityAlive& Target, u32 Until);
	static void UpdateEvasionTarget(CBaseMonster& Owner);
	static bool HasEvasionTarget(CBaseMonster& Owner, const CEntityAlive& Target);
	static EELSightResult Check(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, bool UseWeather = true);
	static EELSightResult CheckFrom(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, const Fvector& StartPosition, bool UseWeather = true);
	static bool Visible(CBaseMonster& OwnerMonster, CObject& Candidate, float Angle, float Radius, bool UseWeather = true);
};
