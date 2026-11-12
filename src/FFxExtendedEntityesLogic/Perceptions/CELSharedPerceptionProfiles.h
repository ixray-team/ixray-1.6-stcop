#pragma once

enum class EELApproachStrategy : u8
{
	Direct,
	Flank,
	Wide,
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
	bool HasCharacter = false;
	float CharacterAggression = 50.f;
	float CharacterCourage = 50.f;
	float CharacterSizeCourage = 15.f;
	float CharacterHealthPenalty = 45.f;
	float CharacterHitLoss = 70.f;
	float CharacterHitGain = 18.f;
	float CharacterAllyDeathLoss = 12.f;
	float CharacterRecovery = 2.f;
	float CharacterRetreatThreshold = 25.f;
	float CharacterResumeThreshold = 45.f;
	float CharacterTierPressure = 12.f;
	float CharacterPackBonus = 18.f;
	float CharacterCounterChance = .15f;
	float CharacterSafeDistance = 25.f;
	float CharacterDodgeCaution = .5f;
	u32 CharacterMemory = 180000;
	u32 CharacterReevaluate = 2000;
	u32 CharacterDecisionHold = 6000;
	bool CanCornerBerserk = true;
	float CornerBerserkHealth = .3f;
	float CornerBerserkDamage = 2.f;
	u32 CornerBerserkDuration = 60000;
	float InjuryFearGain = .5f;
	float FieldHeadacheGain = .08f;
	float FieldHeadacheRecovery = .12f;
	float FieldHeadacheResume = .25f;
	bool CanFearPredators = true;
	bool UsesPackCourage = true;
	shared_str PredatorSpecies;
	float PredatorFearChance = .65f;
	float PackCourageRadius = 25.f;
	float PackCourageGain = .08f;
	float SoloCourageDecay = .15f;
	float PackCouragePerAlly = .2f;
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
	float DayVision = 1.f;
	float NightVision = 1.f;
	float DayAcquire = 1.f;
	float NightAcquire = 1.f;
	float RainVisionLoss = .15f;
	float FogVisionLoss = .4f;
	bool CanDodge = true;
	bool CanDodgeFire = true;
	bool CanDodgeStep = true;
	bool CanDodgeJump = false;
	bool DodgeJumpOnAim = false;
	bool DodgeTrackTarget = true;
	u32 DodgeTrackInterval = 100;
	bool CanAttackJump = true;
	bool CanEvadeDive = true;
	bool CanCounterAttack = true;
	u32 CounterAttackDuration = 1500;
	bool CounterOnReload = true;
	bool CounterOnEmptyClick = true;
	bool CounterOnNearMiss = true;
	float CounterNearMissRadius = 3.f;
	u32 CounterOpportunityTime = 2000;
	u32 CounterOpportunityCooldown = 2000;
	float DodgeWidth = 2.5f;
	float DodgeMaxWidth = 6.f;
	float DodgeConeMargin = 1.f;
	float DodgeJumpMaxAngle = 40.f;
	float DodgePredictionTime = .35f;
	u32 DodgeDirectionInterval = 450;
	float DodgeStrength = .45f;
	float DodgeAimAngle = 10.f;
	float DodgeReleaseAngle = 12.f;
	u32 DodgeDurationMin = 1500;
	u32 DodgeDurationMax = 2200;
	u32 DodgePauseMin = 200;
	u32 DodgeFocusTime = 300;
	u32 DodgeVisibilityGrace = 250;
	u32 DodgeCounterTime = 500;
	u32 DodgeJumpCooldown = 8000;
	u32 DodgeJumpRetry = 3000;
	float DodgeAttackCommitTime = .35f;
	u32 SoundLookTime = 1200;
	u32 SoundDangerLookTime = 2500;
	u32 SoundLookInterval = 100;
	float SoundLookTurnSpeed = PI;
	float FeintChance = .25f;
	bool CanFeint = false;
	u32 PairedAttackDelay = 220;
	u32 DodgeStepCooldown = 600;
	float DodgeJumpChance = .35f;
	float DodgeJumpWidthMin = 1.8f;
	float DodgeJumpWidthMax = 3.2f;
	float DodgeJumpDistanceMax = 5.f;
	float DodgeJumpDistanceMin = .5f;
	float DodgeApproachAngle = 15.f;
	float DodgeStepVariation = 0.f;
	float DodgeStepMaxAngle = 40.f;
	float DodgeStepDistance = 4.f;
	float DodgeStepArrivalDistance = 1.f;
	bool CanSprint = false;
	float SprintBonus = .3f;
	u32 SprintDuration = 3000;
	u32 SprintCooldown = 8000;
	float SprintDistance = 10.f;
	bool CanPursuitJump = false;
	bool CanInterceptFlying = false;
	float InterceptChance = .35f;
	u32 TacticalJumpCooldown = 4000;
	float GroupInterceptDistance = 15.f;
	bool CanAmbush = false;
	bool CanGrabFlying = false;
	float GrabHealthThreshold = .3f;
	float GrabMassRatio = .5f;
	u32 GrabDuration = 650;
	float GrabThrowSpeed = 6.f;
	float GrabMouthOffset = .15f;
	shared_str GrabBone;
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
	float TargetSwitchDistanceRatio = .6f;
	float TargetCloseThreatDistance = 12.f;
	u32 TargetPursuitGrace = 5000;
	u32 TargetDamageMemory = 3000;
	float TargetMemoryCost = 4.f;
	float TargetDamageCost = .25f;
	float TargetCloseThreatCost = .5f;
	float TargetInterestGain = .025f;
	float TargetInterestDecay = .02f;
	float TargetInterestHitGain = .35f;
	float TargetInterestDamageGain = .5f;
	float TargetInterestWeight = 3.f;
	float ApproachSpeed = 2.f;
	float TrailLength = 5.f;
	float SearchRunDistance = 3.f;
	float ApproachWidth = 5.f;
	float RetreatHealthThreshold = .35f;
	float ChargeMinDistance = 15.f;
	float LungeMaxDistance = 7.f;
	u32 DirectRushMinMembers = 4;
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
	u32 NoisePanicDuration = 25000;
	u32 ShotPanicDuration = 20000;
	u32 ExplosionPanicDuration = 40000;
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
