#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/CELGroundAttackBase.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/level_graph.h"
#include "xrGame/monster_scale.h"
#include "xrGame/ai/monsters/control_manager_custom.h"
#include "xrGame/CharacterPhysicsSupport.h"
#include "xrGame/PHMovementControl.h"
#include "xrPhysics/IPHCharacter.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
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
	Tactics.Release(*Adapter);
	SearchedTarget = SuppressedTarget = u16(-1);
	SearchedObservation = SuppressedObservation = SuppressedUntil = 0;
	CELGround::Detach();
}
// Обновляет выбранного противника и сообщает его сведения схемам.
EELControl CELGroundAttackBase::UpdateTarget(const SELObservation& Observation, float DeltaTime)
{
	CEntityAlive* ObservedTarget = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	const bool CurrentlyVisible = ObservedTarget && Adapter->CanSee(*ObservedTarget);
	if (!CurrentlyVisible && Observation.TargetIdentifier == SuppressedTarget && Observation.ObservedAt <= SuppressedObservation && s32(Device.dwTimeGlobal - SuppressedUntil) < 0)
	{
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	TargetIdentifier = Observation.TargetIdentifier;
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(TargetIdentifier);
	const bool HasVision = TargetEntity && Adapter->CanSee(*TargetEntity);
	if (!TargetEntity)
	{
		Tactics.Release(*Adapter);
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	const bool IsBerserk = s32(Adapter->Monster().CorneredBerserkUntil - Device.dwTimeGlobal) > 0;
	if (IsBerserk || IsCharacterCounterAttacking())
	{
		Tactics.Release(*Adapter);
	}
	else
	{
		Tactics.Update(*Adapter, Profile, *TargetEntity, HasVision, &Hearing);
	}
	if (Tactics.StartedJump())
	{
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	AttackBehavior.SetForceDirect(IsBerserk || IsCharacterCounterAttacking() || Tactics.IsCounterAttacking() || Tactics.IsFireEvading());
	AttackBehavior.SetTacticalGoal(Tactics.HasGoal() ? &Tactics.Goal() : nullptr);
	Fvector TargetPosition = Observation.Position;
	if (HasVision)
	{
		TargetEntity->Center(TargetPosition);
	}
	Fvector OwnerPosition;
	Adapter->Monster().Center(OwnerPosition);
	const float Distance = OwnerPosition.distance_to(TargetPosition);
	const bool CanUseNativeJump = Profile.CanAttackJump && HasVision && !TargetEntity->UsedAI_Locations() && Adapter->Monster().com_man().get_jump_control() && Adapter->Monster().com_man().check_if_jump_possible(TargetPosition, true);
	// Give actors on navigable ground the same native windup space as other opponents.
	// Switching back at the start-distance boundary used to cancel running bites before impact.
	const bool HasNativeApproach = HasVision && Distance < Profile.NativeAttackDistance &&
		Adapter->CanReach(TargetEntity->Position(), ai().level_graph().vertex_id(TargetEntity->Position()));
	const bool CanUseNativeAttack = HasVision && (HasNativeApproach || Adapter->Monster().MeleeChecker.can_start_melee(TargetEntity) || CanUseNativeJump);
	if (CELSharedDebug::Instance().CanLogCombat(Adapter->Monster().Position()) && s32(Device.dwTimeGlobal - NextAttackTrace) >= 0)
	{
		NextAttackTrace = Device.dwTimeGlobal + 1000;
		CELSharedDebug::Instance().LogCombat(Adapter->Monster().ID(), "[monsters_debug] ATTACK_GATE t=%u id=%u target=%u visible=%d distance=%.2f native_range=%.2f target_ai=%d native_eligible=%d native_jump=%d tactics_owns=%d evade=%d berserk=%d", Device.dwTimeGlobal, Adapter->Monster().ID(), TargetIdentifier, int(HasVision), Distance, Profile.NativeAttackDistance, int(TargetEntity->UsedAI_Locations()), int(CanUseNativeAttack), int(CanUseNativeJump), int(Tactics.OwnsMovement()), int(Tactics.IsEvading()), int(IsBerserk));
	}
	if (CanUseNativeAttack && !Tactics.OwnsMovement())
	{
		BehaviorManager.Clear();
		return EELControl::Native;
	}
	// Loss of sight selects search immediately; search owns the run to the last known point.
	const bool ContactGrace = !HasVision && Device.dwTimeGlobal - Observation.ObservedAt < 1000u &&
		(Observation.Source == EELObservationSource::Vision || Observation.Source == EELObservationSource::CloseContact || Observation.Source == EELObservationSource::CloseScent) &&
		Adapter->Monster().Position().distance_to_sqr(Observation.Position) <= _sqr(Profile.NativeAttackDistance * 2.f);
	if (!HasVision && !Profile.CanTrackLive && !ContactGrace)
	{
		if (SearchedTarget == TargetIdentifier && SearchedObservation == Observation.ObservedAt && BehaviorManager.IsActive(SearchBehavior) && BehaviorManager.GetValidation().State == EELBehaviorState::Completed)
		{
			Memory.Remove(TargetIdentifier);
			BehaviorManager.Clear();
			return EELControl::Native;
		}
		if (SearchedTarget != TargetIdentifier || SearchedObservation != Observation.ObservedAt)
		{
			Memory.KeepUntil(TargetIdentifier, Device.dwTimeGlobal + Profile.ApproachMax + Profile.SearchMax + 1000u);
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

void CELGroundAttackBase::FillGroundDebug(SELDebugRecord& Record) const
{
	if (IsFleeingPredator)
	{
		return;
	}
	if (TargetIdentifier != u16(-1))
	{
		Record.IsCounterAttacking = Record.IsCounterAttacking || Tactics.IsCounterAttacking();
		Tactics.FillDebug(Record);
		CControlJump* Jump = Adapter->Monster().com_man().get_jump_control();
		if (Jump && Jump->is_active() && Jump->setup_data().flags.test(SControlJumpData::eEvasiveJump))
		{
			Fvector Travel = Fvector().sub(Adapter->Monster().Position(), Jump->get_jump_start_pos());
			Fvector Planned = Fvector().sub(Jump->setup_data().target_position, Jump->get_jump_start_pos());
			Travel.y = Planned.y = 0.f;
			string256 FlightLabel;
			IPHCharacter* Character = Adapter->Monster().character_physics_support()->movement()->PHCharacter();
			Fvector Velocity;
			Character->GetVelocity(Velocity);
			xr_sprintf(FlightLabel, "УВОРОТ [%s]: старт %s | пройдено %.2f / %.2f м | время %.2f / %.2f с", Jump->GetPhaseName(), Jump->HasFlightStarted() ? "да" : "нет", Travel.magnitude(), Planned.magnitude(), Jump->GetFlightElapsed(), Jump->GetFlightTime());
			Record.Senses.push_back(FlightLabel);
			xr_sprintf(FlightLabel, "ФИЗИКА УВОРОТА: vXZ %.2f | vY %.2f | активно %d | прыжок %d | опора %d | scale %.2f | диапазон %.2f..%.2f м", std::sqrt(Velocity.x * Velocity.x + Velocity.z * Velocity.z), Velocity.y, int(Character->IsEnabled()), int(Character->JumpState()), int(Character->HasGroundSupport()), Adapter->Monster().GetVisualScale(), Profile.DodgeJumpDistanceMin, Profile.DodgeJumpDistanceMax);
			Record.Senses.push_back(FlightLabel);
		}
		Record.IsEvading = (AttackBehavior.ExecutingTacticalMove() || Tactics.StartedJump()) && Tactics.IsEvading();
		string256 ExecutionLabel;
		xr_sprintf(ExecutionLabel, "ШАГ УВОРОТА: %s", AttackBehavior.TacticalMovementReason());
		Record.Senses.push_back(ExecutionLabel);
	}
}
