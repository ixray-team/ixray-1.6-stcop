#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELLogic.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/entity_alive.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"

namespace ExtendedEntityesAssignments
{
struct SAssignment
{
	u16 TargetIdentifier = u16(-1);
	u32 OwnerGeneration = 0;
	u32 Until = 0;
};
struct SInterest
{
	float Value = 0.f;
	u32 UpdatedAt = 0;
};
struct SPersonalTargets
{
	u32 Generation = 0;
	u32 SampleAt = 0;
	u16 ConfirmedTarget = u16(-1);
	u32 ConfirmedAt = 0;
	xr_map<u16, SInterest> Interests;
	u16 Selected = u16(-1);
	float SelectedCost = 0.f;
	float PreviousCost = 0.f;
	float SelectedInterest = 0.f;
};
xr_map<u16, SPersonalTargets> PersonalTargets;
SPersonalTargets& Personal(CBaseMonster& Owner)
{
	SPersonalTargets& State = PersonalTargets[Owner.ID()];
	const u32 Generation = ExtendedEntityes::OwnerGeneration(Owner.ID());
	if (State.Generation != Generation)
	{
		State = {};
		State.Generation = Generation;
	}
	return State;
}
float Interest(SInterest& Value, const SELPerceptionProfile& Profile)
{
	const u32 Now = Device.dwTimeGlobal;
	Value.Value = std::max(0.f, Value.Value - float(Now - Value.UpdatedAt) * .001f * Profile.TargetInterestDecay);
	Value.UpdatedAt = Now;
	return Value.Value;
}
xr_map<u16, SAssignment> Assignments;
u32 LastFrame = u32(-1);
u32 RemainingPlans = 0;

// Один раз собирает загрузку всех соседних союзников без отсечения первых 32 записей.
xr_map<u16, u32> AssignedCounts(IELAdapter& Adapter, float CrowdDistance)
{
	xr_map<u16, u32> Counts;
	xr_map<u16, SAssignment>::const_iterator AssignmentIterator;
	CEntityAlive* OwnerEntity;
	CBaseMonster* OwnerMonster;
	for (AssignmentIterator = Assignments.begin(); AssignmentIterator != Assignments.end(); ++AssignmentIterator)
	{
		if (AssignmentIterator->first == Adapter.Monster().ID() || s32(Device.dwTimeGlobal - AssignmentIterator->second.Until) >= 0)
		{
			continue;
		}
		OwnerEntity = ExtendedEntityes::FindEntity(AssignmentIterator->first);
		OwnerMonster = OwnerEntity ? smart_cast<CBaseMonster*>(OwnerEntity) : nullptr;
		if (!OwnerMonster || !OwnerMonster->g_Alive() || AssignmentIterator->second.OwnerGeneration != ExtendedEntityes::OwnerGeneration(AssignmentIterator->first))
		{
			continue;
		}
		if (Adapter.Monster().Position().distance_to_sqr(OwnerMonster->Position()) <= CrowdDistance * CrowdDistance && CELSharedEnemySharing::AreFriends(Adapter.Monster(), *OwnerMonster))
		{
			++Counts[AssignmentIterator->second.TargetIdentifier];
		}
	}
	return Counts;
}
} // namespace ExtendedEntityesAssignments

// Rank confirmed threats before assignment balancing; this only reads existing perception.
u32 CELSharedTargets::ThreatPriority(const SELObservation& Observation, IELAdapter& Adapter)
{
	CEntityAlive* Target = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	if (!Target || !Adapter.IsEnemy(*Target))
	{
		return 0;
	}
	CBaseMonster& Owner = Adapter.Monster();
	if (Owner.HitMemory.get_last_hit_object() == Target && Device.dwTimeGlobal - Owner.HitMemory.get_last_hit_time() < CELSharedSettings::Profile(*Owner.cNameSect()).TargetDamageMemory)
	{
		return 3;
	}
	if (Adapter.CanSee(*Target) || (Observation.Source == EELObservationSource::CloseContact && Device.dwTimeGlobal - Observation.ObservedAt < 1000u))
	{
		return 2;
	}
	return 0;
}

// Combat selection never waits for the optional squad assignment budget.
const SELObservation* CELSharedTargets::Select(const CELSharedMemory& Memory, IELAdapter& Adapter, u16 CurrentTarget, IELLogic* Logic)
{
	using namespace ExtendedEntityesAssignments;
	if (LastFrame != Device.dwFrame)
	{
		LastFrame = Device.dwFrame;
		RemainingPlans = CELSharedSettings::Work().TargetAssignments;
	}
	const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Adapter.Monster().cNameSect());
	const u16 OwnerIdentifier = Adapter.Monster().ID();
	const u32 Generation = ExtendedEntityes::OwnerGeneration(OwnerIdentifier);
	SPersonalTargets& State = Personal(Adapter.Monster());
	const SELObservation* Current = Memory.Find(CurrentTarget);
	CEntityAlive* CurrentEntity = Current ? ExtendedEntityes::FindEntity(CurrentTarget) : nullptr;
	if (!CurrentEntity || !Adapter.IsEnemy(*CurrentEntity) || !Adapter.CanPursue(*CurrentEntity, Current->Position) || (Logic && !Logic->CanEngage(*Current)))
	{
		Current = nullptr;
	}
	const u32 CurrentPriority = Current ? ThreatPriority(*Current, Adapter) : 0;
	if (CurrentPriority >= 2)
	{
		State.ConfirmedTarget = CurrentTarget;
		State.ConfirmedAt = Device.dwTimeGlobal;
	}
	const bool RememberedConfirmation = State.ConfirmedTarget == CurrentTarget && Device.dwTimeGlobal - State.ConfirmedAt < Profile.TargetPursuitGrace;
	const bool RecentConfirmation = Current && (RememberedConfirmation || ((Current->Source == EELObservationSource::Vision || Current->Source == EELObservationSource::Damage || Current->Source == EELObservationSource::CloseContact) && Device.dwTimeGlobal - Current->ObservedAt < Profile.TargetPursuitGrace));
	const bool Committed = Current && (CurrentPriority >= 2 || RecentConfirmation);
	const float Elapsed = State.SampleAt ? float(Device.dwTimeGlobal - State.SampleAt) * .001f : 0.f;
	State.SampleAt = Device.dwTimeGlobal;
	for (auto Iterator = State.Interests.begin(); Iterator != State.Interests.end();)
	{
		if (!Memory.Find(Iterator->first))
		{
			Iterator = State.Interests.erase(Iterator);
		}
		else
		{
			if (Committed && Iterator->first == CurrentTarget)
			{
				Iterator->second.UpdatedAt = Device.dwTimeGlobal;
			}
			else
			{
				Interest(Iterator->second, Profile);
			}
			++Iterator;
		}
	}
	if (Committed)
	{
		SInterest& Value = State.Interests[CurrentTarget];
		Interest(Value, Profile);
		Value.Value = std::min(1.f, Value.Value + std::min(Elapsed, 1.f) * Profile.TargetInterestGain);
	}
	State.PreviousCost = 0.f;
	const bool CanDistribute = CELSharedSettings::CanDistributeTargets() && RemainingPlans;
	const xr_map<u16, u32> Counts = CanDistribute ? AssignedCounts(Adapter, Profile.TargetCrowdDistance) : xr_map<u16, u32>();
	const SELObservation* BestObservation = nullptr;
	u32 BestAssignedCount = u32(-1);
	float BestScore = FLT_MAX;
	for (const SELObservation& Observation : Memory.GetRecords())
	{
		CEntityAlive* Candidate = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
		if (!Candidate || !Adapter.IsEnemy(*Candidate) || !Adapter.CanPursue(*Candidate, Observation.Position) || (Logic && !Logic->CanEngage(Observation)))
		{
			continue;
		}
		const u32 Priority = ThreatPriority(Observation, Adapter);
		// Unconfirmed noise/tracks remain remembered, but cannot replace an active confirmed fight.
		if (Committed && Priority == 0 && Observation.TargetIdentifier != CurrentTarget)
		{
			continue;
		}
		const auto CountIterator = Counts.find(Observation.TargetIdentifier);
		const u32 AssignedCount = CountIterator != Counts.end() ? CountIterator->second : 0;
		const float DistanceSquared = Adapter.Monster().Position().distance_to_sqr(Observation.Position);
		const auto PersonalIterator = State.Interests.find(Observation.TargetIdentifier);
		const float PersonalInterest = PersonalIterator != State.Interests.end() ? PersonalIterator->second.Value : 0.f;
		// All enemies use the same cost model. No identity-specific player bonus is applied.
		float Score = std::max(_sqr(Profile.NativeAttackDistance), DistanceSquared);
		Score *= 1.f + float(AssignedCount) / (1.f + float(AssignedCount));
		Score *= Priority == 0 ? Profile.TargetMemoryCost : 1.f;
		Score *= Priority == 3 ? Profile.TargetDamageCost : 1.f;
		Score *= Priority >= 2 && DistanceSquared <= _sqr(Profile.TargetCloseThreatDistance) ? Profile.TargetCloseThreatCost : 1.f;
		Score /= 1.f + PersonalInterest * Profile.TargetInterestWeight;
		if (Observation.TargetIdentifier == CurrentTarget)
		{
			Score *= Committed ? _sqr(Profile.TargetSwitchDistanceRatio) : 1.f;
			State.PreviousCost = Score;
		}
		if (!BestObservation || Score < BestScore || (Score == BestScore && AssignedCount < BestAssignedCount))
		{
			BestAssignedCount = AssignedCount;
			BestScore = Score;
			BestObservation = &Observation;
		}
	}
	State.Selected = BestObservation ? BestObservation->TargetIdentifier : u16(-1);
	State.SelectedCost = BestObservation ? BestScore : 0.f;
	const auto SelectedInterest = State.Interests.find(State.Selected);
	State.SelectedInterest = SelectedInterest != State.Interests.end() ? SelectedInterest->second.Value : 0.f;
	if (BestObservation && CanDistribute)
	{
		--RemainingPlans;
		Assignments[OwnerIdentifier] = {BestObservation->TargetIdentifier, Generation, Device.dwTimeGlobal + Random.randI(3000u, 5001u)};
	}
	return BestObservation;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedTargets::Clear()
{
	ExtendedEntityesAssignments::Assignments.clear();
	ExtendedEntityesAssignments::PersonalTargets.clear();
	ExtendedEntityesAssignments::LastFrame = u32(-1);
}

// Освобождает данные и назначения зарегистрированного владельца.
void CELSharedTargets::RemoveOwner(u16 OwnerIdentifier)
{
	ExtendedEntityesAssignments::Assignments.erase(OwnerIdentifier);
	ExtendedEntityesAssignments::PersonalTargets.erase(OwnerIdentifier);
}

void CELSharedTargets::RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth)
{
	if (Target.GetfHealth() >= PreviousHealth || AttackerIdentifier == Target.ID())
	{
		return;
	}
	const auto AddInterest = [](CBaseMonster* Owner, CEntityAlive& Enemy, bool Received)
	{
		if (!Owner || !Owner->g_Alive() || !ExtendedEntityes::OwnerGeneration(Owner->ID()) || !Owner->EnemyMan.is_enemy(&Enemy))
		{
			return;
		}
		const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Owner->cNameSect());
		auto& State = ExtendedEntityesAssignments::Personal(*Owner);
		if (State.Interests.size() >= 64 && !State.Interests.contains(Enemy.ID()))
		{
			return;
		}
		auto& Value = State.Interests[Enemy.ID()];
		ExtendedEntityesAssignments::Interest(Value, Profile);
		Value.Value = std::min(1.f, Value.Value + (Received ? Profile.TargetInterestDamageGain : Profile.TargetInterestHitGain));
	};
	CEntityAlive* Attacker = ExtendedEntityes::FindEntity(AttackerIdentifier);
	if (Attacker)
	{
		AddInterest(smart_cast<CBaseMonster*>(Attacker), Target, false);
		AddInterest(smart_cast<CBaseMonster*>(&Target), *Attacker, true);
	}
}

void CELSharedTargets::FillDebug(SELDebugRecord& Record, u16 OwnerIdentifier)
{
	const auto Iterator = ExtendedEntityesAssignments::PersonalTargets.find(OwnerIdentifier);
	if (Iterator == ExtendedEntityesAssignments::PersonalTargets.end())
	{
		return;
	}
	const auto& State = Iterator->second;
	string256 Label;
	xr_sprintf(Label, "ВЫБОР ЦЕЛИ #%u: цена %.2f | прежняя %.2f | личный интерес %.2f", State.Selected, State.SelectedCost, State.PreviousCost, State.SelectedInterest);
	Record.Senses.push_back(Label);
}
