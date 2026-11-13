#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
class IELAdapter;
class CEntityAlive;
class CELSharedHearing;
struct SELDebugRecord;

enum class EELCombatIntent : u8
{
	Close,
	Strike,
	DodgeFire,
	DodgeMelee,
	Counter,
	Sprint,
	PursuitJump,
	FlyingIntercept,
	EvadeDive,
	Feint,
	PairedAttack,
	EvasionJump
};

// Owns temporary combat decisions; route validation and locomotion remain in their executors.
class CELGroundCombatTactics
{
public:
	void Update(IELAdapter& Adapter, const SELPerceptionProfile& Profile, CEntityAlive& Target, bool Visible, const CELSharedHearing* Hearing = nullptr, bool Defensive = false);
	void RegisterHit(const SELPerceptionProfile& Profile, const Fvector& Direction);
	void RejectDefensiveGoal();
	void Release(IELAdapter& Adapter);
	void FillDebug(SELDebugRecord& Record) const;
	bool IsEvading() const { return Intent == EELCombatIntent::EvasionJump || Intent == EELCombatIntent::DodgeFire || Intent == EELCombatIntent::DodgeMelee || Intent == EELCombatIntent::Feint || Intent == EELCombatIntent::EvadeDive; }
	bool IsFireEvading() const { return Intent == EELCombatIntent::DodgeFire || Intent == EELCombatIntent::EvasionJump; }
	bool IsCounterAttacking() const { return Intent == EELCombatIntent::Counter; }
	bool HasGoal() const { return HasTacticalGoal; }
	const Fvector& Goal() const { return TacticalGoal; }
	bool OwnsMovement() const { return Intent == EELCombatIntent::EvasionJump || Intent == EELCombatIntent::DodgeFire || Intent == EELCombatIntent::DodgeMelee || Intent == EELCombatIntent::EvadeDive || Intent == EELCombatIntent::Feint || Intent == EELCombatIntent::PairedAttack; }
	const Fvector& DefensiveEye() const { return DefensiveEyePosition; }
	bool HasDefensiveSight() const { return DefensiveSawTarget; }
	bool StartedJump() const { return Intent == EELCombatIntent::EvasionJump || Intent == EELCombatIntent::PursuitJump || Intent == EELCombatIntent::FlyingIntercept; }
private:
	bool UpdateAvoidance(IELAdapter& Adapter, const SELPerceptionProfile& Profile, CEntityAlive& Target, bool Visible, bool Defensive, const CELSharedHearing* Hearing);
	Fvector DefensiveEyePosition = {0.f, 0.f, 0.f};
	Fvector LastAimDirection = {0.f, 0.f, 0.f};
	Fvector HitDirection = {0.f, 0.f, 0.f};
	u32 LastAimAt = 0;
	u32 HitReactionUntil = 0;
	u32 HitReactionAfter = 0;
	bool DefensiveMode = false;
	bool DefensiveSawTarget = false;
	bool HadRecoveryWindow = false;
	bool FreeEvasion = false;
	EELCombatIntent Intent = EELCombatIntent::Close;
	Fvector TacticalGoal = {0.f, 0.f, 0.f};
	Fvector LastTargetPosition = {0.f, 0.f, 0.f};
	Fvector TargetVelocity = {0.f, 0.f, 0.f};
	Fvector LastOwnerPosition = {0.f, 0.f, 0.f};
	Fvector OwnerVelocity = {0.f, 0.f, 0.f};
	u16 TargetIdentifier = u16(-1);
	u32 LastSampleAt = 0;
	u32 LastVisibleAt = 0;
	u32 FocusUntil = 0;
	u32 ManeuverUntil = 0;
	u32 DodgeAfter = 0;
	u32 DodgeJumpAfter = 0;
	u32 DodgeStepAfter = 0;
	u32 DodgeDirectionAfter = 0;
	u32 CounterUntil = 0;
	u32 OpportunityAfter = 0;
	u32 LastCounterEventAt = 0;
	u32 JumpAfter = 0;
	u32 SprintUntil = 0;
	u32 SprintAfter = 0;
	float Stamina = 1.f;
	float LastDistance = 0.f;
	float DodgeSide = 1.f;
	float DodgeStepScale = 1.f;
	float DodgeStrength = 0.f;
	bool HasTacticalGoal = false;
	const char* DodgeReason = "не запрошен";
	const char* JumpReason = "не запрошен";
	const char* EvasionJumpReason = "попыток не было";
	float EvasionJumpSide = 0.f;
	float EvasionJumpRequiredSide = 0.f;
};
