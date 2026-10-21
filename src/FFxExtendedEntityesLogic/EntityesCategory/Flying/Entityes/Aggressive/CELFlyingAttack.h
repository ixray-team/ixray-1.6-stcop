#pragma once
class IELAdapter;
class CBaseMonster;
struct SELFlyingSettings;

class CELFlyingAttack final
{
public:
	static Fvector LocalContactPoint(const CBaseMonster& OwnerMonster);
	static Fvector RootAtContact(const CBaseMonster& OwnerMonster, const Fvector& TargetPoint, const Fvector& Direction);
	bool TryHit(IELAdapter& Adapter, const SELFlyingSettings& Settings, u16 TargetIdentifier, const Fvector& PreviousContact, const Fmatrix& PreviousTargetTransform);
	bool IsAttacking() const;
	float CooldownRemaining() const;
	const char* LastContactResult() const { return ContactResult; }

private:
	const char* ContactResult = "Контакт не проверялся";
	u32 NextAttack = 0;
	u32 AttackUntil = 0;
};
