#pragma once

#include "alife_space.h"

class CBurer;
class CPhysicsShellHolder;

// Gunslinger combat policy. Configuration is read once by CBurer::Load.
class CBurerCombat
{
public:
	bool Enabled = false;
	float SuperDistance = 15.f;
	float StaminaHit = 1000.f;
	float StaminaFloor = .05f;
	float StaminaHealth = .5f;
	float HealthHit = -1.f;
	float HealthImpulse = 10.f;
	ALife::EHitType HealthHitType = ALife::eHitTypeWound;
	float HideSkipProbability = 1.f;
	float ConditionMin = .05f;
	float ConditionMax = .1f;
	float AntiAimDistance = 7.f;
	float ShieldDistance = 3.f;
	float RiskProbability = .02f;
	u32 MinGrenadeTimer = 1000;
	u32 CriticalGrenadeTimer = 1000;
	u32 ForceFireDelay = 1000;
	float GraviImpulse = 1000.f;
	float RiskImpulse = 1000.f;
	float FireAllProbability = .7f;
	u32 AimDelay = 500;
	u32 AimRandomDelay = 1000;
	u32 KnifeWindow = 500;

	struct SContext
	{
		bool ActorEnemy = false;
		bool Visible = false;
		bool Facing = false;
		bool Dangerous = false;
		bool Knife = false;
		bool Consumable = false;
		bool Inventory = false;
		bool Sniper = false;
		bool Explosive = false;
		bool RecentExplosion = false;
		bool Reloading = false;
		bool Empty = false;
		bool Firing = false;
		bool Aiming = false;
		bool Throwable = false;
		bool PreparedThrow = false;
		bool HasItem = false;
		bool UnderAim = false;
		bool UnderWideAim = false;
		bool UnderExactAim = false;
		bool Moving = false;
		float ReadyIn = 0.f;
		float Distance = 0.f;
		u32 Grenades = 0;
		u32 GrenadeTimer = u32(-1);
	} Context;

private:
	u32 ContextTime = u32(-1);
	u32 AimUntil = 0;
	u32 AimDuration = 0;
	u32 RiskNext = 0;
	u32 TeleDecisionNext = 0;
	bool WasAimed = false;
	bool StopTele = false;
	xr_vector<ISpatialShared> Nearby;

public:
	void Load();
	void Reset();
	void Refresh(CBurer& Burer);
	void BeginTele()
	{
		TeleDecisionNext = 0;
		StopTele = false;
	}
	bool CloseProtection() const;
	bool TeleAllowed() const;
	bool CanTeleObject(CPhysicsShellHolder& Object) const;
	void CorrectReadiness(CBurer& Burer, bool& HealthLoss, bool& AntiAim, bool& Gravi, bool& Tele, bool& Shield, bool PreviousShield, bool PreviousTele);
	bool HoldShield(CBurer& Burer);
	bool NeedStopTele(CBurer& Burer, u32 Started);
	bool CanFireAll(u32 Started, u32 End) const;
	void StaminaAttack(CBurer& Burer);
};
