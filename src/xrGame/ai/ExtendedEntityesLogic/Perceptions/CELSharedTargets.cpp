#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "entity_alive.h"
#include "ai/monsters/basemonster/base_monster.h"

namespace ExtendedEntityesAssignments
{
struct SAssignment
{
	u16 TargetIdentifier = u16(-1);
	u32 OwnerGeneration = 0;
	u32 Until = 0;
};
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

// Выбирает известного противника с учётом доступности, опасности и назначений союзников.
const SELObservation* CELSharedTargets::Select(const CELSharedMemory& Memory, IELAdapter& Adapter, u16 CurrentTarget)
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
	xr_map<u16, SAssignment>::iterator AssignmentIterator = Assignments.find(OwnerIdentifier);
	const SELObservation* HeldObservation = nullptr;
	CEntityAlive* HeldTarget = nullptr;
	if (AssignmentIterator != Assignments.end() && AssignmentIterator->second.OwnerGeneration == Generation && s32(Device.dwTimeGlobal - AssignmentIterator->second.Until) < 0)
	{
		HeldObservation = Memory.Find(AssignmentIterator->second.TargetIdentifier);
		HeldTarget = HeldObservation ? ExtendedEntityes::FindEntity(HeldObservation->TargetIdentifier) : nullptr;
		if (HeldTarget && (!Adapter.IsEnemy(*HeldTarget) || !Adapter.CanPursue(*HeldTarget, HeldObservation->Position)))
		{
			HeldObservation = nullptr;
		}
	}
	const bool CanDistribute = CELSharedSettings::CanDistributeTargets();
	if (HeldObservation && HeldTarget && Adapter.CanSee(*HeldTarget))
	{
		return HeldObservation;
	}
	if (CanDistribute && !RemainingPlans)
	{
		// Без квоты не выдаём незарегистрированное назначение всей оставшейся стае.
		return HeldObservation;
	}
	const xr_map<u16, u32> Counts = CanDistribute ? AssignedCounts(Adapter, Profile.TargetCrowdDistance) : xr_map<u16, u32>();
	u32 BestAssignedCount = u32(-1);
	u32 CandidateAssignedCount;
	xr_map<u16, u32>::const_iterator CountIterator;
	const SELObservation* BestObservation = nullptr;
	float BestScore = FLT_MAX;
	float CandidateScore;
	bool HasVision;
	bool HasVisibleTarget = false;
	CEntityAlive* Candidate;
	CELSharedMemory::Records::const_iterator ObservationIterator;
	for (ObservationIterator = Memory.GetRecords().begin(); ObservationIterator != Memory.GetRecords().end(); ++ObservationIterator)
	{
		Candidate = ExtendedEntityes::FindEntity(ObservationIterator->TargetIdentifier);
		if (!Candidate || !Adapter.IsEnemy(*Candidate) || !Adapter.CanPursue(*Candidate, ObservationIterator->Position))
		{
			continue;
		}
		HasVision = Adapter.CanSee(*Candidate);
		if (HasVisibleTarget && !HasVision)
		{
			continue;
		}
		if (!HasVisibleTarget && HasVision)
		{
			BestScore = FLT_MAX;
			BestAssignedCount = u32(-1);
			HasVisibleTarget = true;
		}
		CandidateScore = Adapter.Monster().Position().distance_to_sqr(ObservationIterator->Position);
		CountIterator = Counts.find(ObservationIterator->TargetIdentifier);
		CandidateAssignedCount = CountIterator != Counts.end() ? CountIterator->second : 0;

		if (ObservationIterator->TargetIdentifier == CurrentTarget)
		{
			CandidateScore *= .8f;
		}
		if (CandidateAssignedCount < BestAssignedCount || (CandidateAssignedCount == BestAssignedCount && CandidateScore < BestScore))
		{
			BestAssignedCount = CandidateAssignedCount;
			BestScore = CandidateScore;
			BestObservation = &*ObservationIterator;
		}
	}
	if (HeldObservation && HeldTarget && (!HasVisibleTarget || Adapter.CanSee(*HeldTarget)))
	{
		return HeldObservation;
	}
	if (BestObservation && RemainingPlans && CanDistribute)
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
	ExtendedEntityesAssignments::LastFrame = u32(-1);
}

// Освобождает данные и назначения зарегистрированного владельца.
void CELSharedTargets::RemoveOwner(u16 OwnerIdentifier)
{
	ExtendedEntityesAssignments::Assignments.erase(OwnerIdentifier);
}
