#pragma once

enum class EELApproachStrategy : u8
{
	Direct,
	Flank,
	Wide,
	Cautious,
	Lunge,
	Charge,
	Encircle,
	Pressure,
	Retreat,
	Count
};

constexpr u32 ELApproachStrategyMask(EELApproachStrategy Strategy)
{
	return 1u << u32(Strategy);
}

enum class EELNoiseType : u8
{
	Shot,
	Explosion,
	ActorSteps,
	MonsterSteps,
	Jump,
	Melee,
	Attack,
	Injury,
	Death,
	BulletHit,
	Breaking,
	Collision,
	Talking,
	Eating,
	Idle,
	Ambient,
	Pickup,
	Drop,
	Hide,
	Take,
	Use,
	EmptyClick,
	Reload,
	Other,
	Light,
	Count
};

constexpr u32 ELNoiseTypeMask(EELNoiseType Type)
{
	return 1u << u32(Type);
}

struct SELNoiseProfile
{
	bool IsEnabled = false;
	bool CanAttract = false;
	bool CanFrighten = false;
	float Radius = 0.f;
	float PanicChance = 0.f;
	float InterestStep = .25f;
	float InterestThreshold = .65f;
	float FearStep = .25f;
	float FearThreshold = .65f;
};

struct SELPerceptionProfile
{
	bool CanUseBehaviors = true;
	bool IsPassive = false;
	bool CanShare = true;
	bool IsWireless = false;
	bool CanTrackLive = false;
	bool CanCall = true;
	bool CanAutoFocus = false;
	bool CanPanicOnDamage = false;
	bool CanInspect = false;
	bool CanInspectBodies = true;
	bool CanAvoidDeaths = true;
	bool CanHearDeaths = true;
	float DeathHearingRadius = 20.f;
	float BodyDetectionRadius = 25.f;
	float DeathHazardRadius = 12.f;
	u32 BodyInterestTime = 90000;
	u32 DeathHazardTime = 120000;
	u32 DeathHazardThreshold = 3;
	u32 BodyInvestigatorLimit = 2;
	u32 DeathFightQuietTime = 15000;
	bool CanIgnoreFoliage = false;
	float SharingRadius = 40.f;
	float CloseSharingRatio = .25f;
	float CallSharingRatio = .6f;
	float CloseDetectionRadius = 0.f;
	bool CanUseCloseScent = false;
	float CloseScentRadius = 5.f;
	u32 CloseScentTime = 5000;
	float CloseVisualRadius = 10.f;
	u32 CloseVisualTime = 5000;
	float NativeAttackDistance = 5.f;
	bool CanAvoidAnomalies = false;
	bool CanUsePeriphery = true;
	float PeripheralAngle = 150.f;
	float PeripheralRadius = 25.f;
	u32 PeripheralInterval = 1000;
	float AnomalyScanRadius = 60.f;
	float AnomalyMargin = 1.f;
	float AutoFocusRadius = 200.f;
	bool CanRecordTrail = true;
	float TargetCrowdDistance = 20.f;
	float ApproachSpeed = 2.f;
	float TrailLength = 5.f;
	float ApproachWidth = 5.f;
	float SearchRadius = 12.f;
	float SearchAngle = 60.f;
	bool CanHearActorSteps = true;
	bool CanHearMonsterSteps = true;
	bool CanHearCombat = true;
	bool CanHearWorld = true;
	u32 InterestNoiseTypes = 0;
	u32 FearNoiseTypes = 0;
	float NoiseFearStep = .25f;
	float NoiseFearThreshold = .65f;
	float NoisePanicChance = 0.f;
	float NoiseInterestStep = .25f;
	float NoiseInterestThreshold = .65f;
	u32 NoiseSearchTime = 60000;
	u32 NoiseShotPriority = 80;
	u32 NoiseDangerPriority = 100;
	u32 NoiseOtherPriority = 10;
	u32 GroundLocalRetry = 200;
	u32 GroundStallTime = 500;
	u32 SearchPointTime = 3000;
	u32 BodyInspectMin = 3000;
	u32 BodyInspectMax = 6000;
	bool CanExpandNoiseSearch = true;
	bool CanWitnessAggression = true;
	u32 WitnessWindow = 750;
	float NoiseMinPower = .01f;
	float NoiseImmediateChance = .35f;
	float NoiseDecay = .1f;
	float GeneralNoiseRadius = 25.f;
	float HearingFactor = 1.f;
	float SuppressedRatio = .3f;
	float MeleeRadius = 5.f;
	float MeleeRatio = 1.f;
	float ActorJumpRadius = 20.f;
	float ActorMovementRadii[5] = {7.f, 10.f, 15.f, 5.f, 2.f};
	float WeaponRatios[5] = {.6f, 1.f, .75f, .4f, .25f};
	u32 NoiseInterestInterval = 100;
	float HearingRatios[9] = {1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f};
	u32 SharingIntervalMin = 3000;
	u32 SharingIntervalMax = 5000;
	u32 MemoryMin = 30000;
	u32 MemoryMax = 60000;
	u32 SearchMin = 15000;
	u32 SearchMax = 25000;
	u32 ApproachMin = 15000;
	u32 ApproachMax = 90000;
	u32 CallCooldown = 30000;
	u32 NoiseCooldown = 3000;
	u32 NoisePanicDuration = 6000;
	u32 VisualAcquireTime = 400;
	u32 Strategies = 1;
	shared_str CallSound;
	SELNoiseProfile Shot;
	SELNoiseProfile Explosion;
};

class CELSharedPerceptionProfiles
{
public:
	static const char* NoiseName(EELNoiseType Type);
	static SELPerceptionProfile Read(const char* Section, const CInifile* SystemSettings);
};
