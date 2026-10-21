#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/CELGroundAttackBase.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/level_graph.h"
// Связывает общую логику атакующих наземных существ с необходимыми службами и состоянием владельца.
CELGroundAttackBase::CELGroundAttackBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELGround(std::move(OwnerAdapter), OwnerProfile), AttackBehavior(*Adapter, Profile), SearchBehavior(*Adapter, Profile) {}
// Завершает общую логику атакующих наземных существ и освобождает связанные ресурсы.
CELGroundAttackBase::~CELGroundAttackBase()
{
	BehaviorManager.Clear();
}
// Снимает управление расширения и освобождает регистрации владельца.
void CELGroundAttackBase::Detach()
{
	SearchedTarget = SuppressedTarget = u16(-1);
	SearchedObservation = SuppressedObservation = SuppressedUntil = 0;
	CELGround::Detach();
}
// Обновляет выбранного противника и сообщает его сведения схемам.
EELControl CELGroundAttackBase::UpdateTarget(const SELObservation& Observation, float DeltaTime)
{
	if (Observation.TargetIdentifier == SuppressedTarget && Observation.ObservedAt <= SuppressedObservation && s32(Device.dwTimeGlobal - SuppressedUntil) < 0)
	{
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	TargetIdentifier = Observation.TargetIdentifier;
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(TargetIdentifier);
	const bool HasVision = TargetEntity && Adapter->CanSee(*TargetEntity);
	Fvector TargetPosition = Observation.Position;
	if (HasVision)
	{
		TargetEntity->Center(TargetPosition);
	}
	Fvector OwnerPosition;
	Adapter->Monster().Center(OwnerPosition);
	const float Distance = OwnerPosition.distance_to(TargetPosition);
	const bool CanUseNativeJump = HasVision && !TargetEntity->UsedAI_Locations() && Adapter->Monster().com_man().get_jump_control() && Adapter->Monster().com_man().check_if_jump_possible(TargetPosition, true);
	const bool CanUseNativeAttack = HasVision && (TargetEntity->UsedAI_Locations() ? Distance < Profile.NativeAttackDistance : Adapter->Monster().MeleeChecker.can_start_melee(TargetEntity) || CanUseNativeJump);
	if (CanUseNativeAttack)
	{
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	const bool HasReached = Distance < 2.f && ai().level_graph().valid_vertex_id(Observation.VertexIdentifier) &&
							ai().level_graph().valid_vertex_id(ai().level_graph().check_position_in_direction(Adapter->Monster().ai_location().level_vertex_id(), Adapter->Monster().Position(), Observation.Position));
	if (!HasVision && (HasReached || (SearchedTarget == TargetIdentifier && SearchedObservation == Observation.ObservedAt)))
	{
		if (SearchedTarget == TargetIdentifier && SearchedObservation == Observation.ObservedAt && BehaviorManager.IsActive(SearchBehavior) && BehaviorManager.GetValidation().State == EELBehaviorState::Completed)
		{
			Memory.Remove(TargetIdentifier);
			BehaviorManager.Clear();
			return EELControl::Native;
		}
		SearchedTarget = TargetIdentifier;
		SearchedObservation = Observation.ObservedAt;
		SearchBehavior.SetObservation(Observation);
		BehaviorManager.Select(&SearchBehavior);
	}
	else
	{
		AttackBehavior.SetObservation(Observation);
		BehaviorManager.Select(&AttackBehavior);
	}
	BehaviorManager.Update(DeltaTime);
	const SELBehaviorValidation& Result = BehaviorManager.GetValidation();
	if (Result.State == EELBehaviorState::Completed && !HasVision && SearchedTarget == TargetIdentifier && SearchedObservation == Observation.ObservedAt)
	{
		Memory.Remove(TargetIdentifier);
		TargetIdentifier = u16(-1);
		BehaviorManager.Clear();
		return EELControl::Extended;
	}
	const bool HasReleased = !BehaviorManager.IsActive(AttackBehavior) && !BehaviorManager.IsActive(SearchBehavior);
	if (Result.State == EELBehaviorState::Completed || Result.State == EELBehaviorState::InvalidTarget || (HasReleased && (Result.State == EELBehaviorState::NoProgress || Result.State == EELBehaviorState::Blocked)))
	{
		SuppressedTarget = TargetIdentifier;
		SuppressedObservation = Observation.ObservedAt;
		SuppressedUntil = Device.dwTimeGlobal + 5000;
	}
	return EELControl::Extended;
}
