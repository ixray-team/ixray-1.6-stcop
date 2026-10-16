#pragma once

class CActor;

enum class EBurerHit : u8
{
	Gravi,
	Drop,
	Risk,
	Stamina
};

// Actor-owned reactions are shared by all attacking burers.
class CActorBurerEffects
{
	u32 CameraId = 0;
	EBurerHit LastHit = EBurerHit::Gravi;
	u32 KnifeUntil = 0;
	u32 RestoreUntil = 0;
	float StaminaFloor = 0.f;
	float HealthThreshold = 1.f;
	bool RestoreBlocked = false;
	bool Available[4] = {};
	void CameraRemoved();

public:
	void Load();
	bool Active(CActor& Actor) const;
	bool CanAntiAim(CActor& Actor) const;
	bool Play(CActor& Actor, EBurerHit Hit, bool Blood = false);
	void PrepareKnife(u32 Duration);
	bool KnifeWindow() const;
	void RestoreStamina(float Floor, float Health, bool Blocked);
	void Update(CActor& Actor);
	void Reset(CActor& Actor);
};
