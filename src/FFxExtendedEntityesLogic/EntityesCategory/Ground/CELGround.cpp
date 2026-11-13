#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnvironment.h"
#include "xrGame/ai/monsters/control_animation_base.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGround.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundFlyingGrab.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_monster_space.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundMovement.h"
#include "xrGame/movement_manager.h"

// Связывает общую логику наземной категории с необходимыми службами и состоянием владельца.
CELGround::CELGround(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELComponentBase(std::move(OwnerAdapter), OwnerProfile), BehaviorManager(*this, *Adapter, Profile) {}
// Завершает общую логику наземной категории и освобождает связанные ресурсы.
CELGround::~CELGround()
{
	BehaviorManager.Clear();
}

// Снимает управление расширения и освобождает регистрации владельца.
void CELGround::Detach()
{
	Tactics.Release(*Adapter);
	CloseScent.Clear();
	BehaviorManager.Clear();
	CELComponentBase::Detach();
}

// Обновляет общую логику наземной категории и обрабатывает текущий результат выполнения.
EELControl CELGround::Update(float DeltaTime)
{
	const EELControl Result = BehaviorManager.UpdateGround(DeltaTime);
	if (Result == EELControl::Native)
	{
		Adapter->EndMovementControl();
	}
	return Result;
}

// Обновляет выбранного противника и сообщает его сведения схемам.
EELControl CELGround::UpdateTarget(const SELObservation& Observation, float DeltaTime)
{
	BehaviorManager.Clear();
	return EELControl::Native;
}

// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELGround::FrameUpdate(float DeltaTime)
{
	UpdateAuditoryAttention();
	if (!HasAttached || !CELSharedDebug::Instance().IsDrawEnabled())
	{
		return;
	}
	SELDebugRecord Record;
	Record.OwnerIdentifier = Adapter->Monster().ID();
	Record.IgnoresFoliage = CELSharedSettings::IsEnabled() && Profile.CanIgnoreFoliage && CELSharedEnvironment::CanSmell();
	Record.Position = Adapter->Monster().Position();
	Fobb LocalBounds;
	LocalBounds.m_rotate.identity();
	Adapter->Monster().BoundingBox().get_CD(LocalBounds.m_translate, LocalBounds.m_halfsize);
	Record.Bounds.transform(LocalBounds, Adapter->Monster().XFORM());
	Record.Speed = Adapter->Monster().movement().speed();
	Record.GoalPosition = Record.Position;
	const SELObservation* Target = Memory.Find(TargetIdentifier);
	if (Target)
	{
		Record.GoalPosition = Target->Position;
	}
	Record.TargetIdentifier = TargetIdentifier;
	Record.KnownTargets = u32(Memory.GetRecords().size());
	Record.Validation = BehaviorManager.HasActiveBehavior() ? BehaviorManager.GetValidation().State : EELBehaviorState::Running;
	Record.Mode = BehaviorManager.HasActiveBehavior() ? "НАЗЕМНАЯ СХЕМА" : "ШТАТНАЯ СХЕМА";
	Record.State = BehaviorManager.HasActiveBehavior() ? BehaviorManager.GetValidation().Reason : Adapter->Monster().m_bSleep ? "Сон"
																															  : "Штатное поведение";
	Record.IsAttacking = Adapter->Monster().anim().m_tAction == ACT_ATTACK;
	if (BehaviorManager.HasActiveBehavior())
	{
		const SELBehaviorValidation& Current = BehaviorManager.GetValidation();
		string256 Timer;
		if (Current.State == EELBehaviorState::WaitingForBudget || Current.State == EELBehaviorState::WaitingForResult)
		{
			xr_sprintf(Timer, "ПУТЬ: ожидание %.1f с", float(Device.dwTimeGlobal - Current.WaitingSince) * .001f);
			Record.Senses.push_back(Timer);
		}
		if (Current.Deadline && s32(Current.Deadline - Device.dwTimeGlobal) > 0)
		{
			xr_sprintf(Timer, "ДЕЙСТВИЕ: %.1f с | осталось %.1f с", float(Device.dwTimeGlobal - Current.StartedAt) * .001f, float(Current.Deadline - Device.dwTimeGlobal) * .001f);
			Record.Senses.push_back(Timer);
		}
	}
	CloseScent.FillDebug(Record, Profile);
	if (IsFleeingPredator)
	{
		Tactics.FillDebug(Record);
	}
	CELGroundFlyingGrab::FillDebug(Adapter->Monster().ID(), Record);
	FillGroundDebug(Record);
	FillDebugRecord(Record);
	CELSharedDebug::Instance().Publish(std::move(Record));
}

// Прекращает преследование скрытой цели без отмены слухового исследования.
void CELGround::ForgetTarget(u16 ObjectIdentifier)
{
	CloseScent.Remove(ObjectIdentifier);
	if (TargetIdentifier == ObjectIdentifier)
	{
		BehaviorManager.Clear();
		CELGroundMovement Movement(*Adapter);
		Movement.Stop();
	}
	CELComponentBase::ForgetTarget(ObjectIdentifier);
}

// Отменяет преследование удалённой либо игнорируемой цели вместе с подготовленным маршрутом.
void CELGround::RemoveObject(u16 ObjectIdentifier)
{
	if (TargetIdentifier == ObjectIdentifier || Hearing.IsReactingTo(ObjectIdentifier))
	{
		BehaviorManager.Clear();
		CELGroundMovement Movement(*Adapter);
		Movement.Stop();
	}
	CELComponentBase::RemoveObject(ObjectIdentifier);
}

void CELGround::OnVisualContact(CEntityAlive& Target, u32 ObservedAt)
{
	CloseScent.See(*Adapter, Target, ObservedAt, Profile);
}

void CELGround::ObserveCloseScent()
{
	CloseScent.Update(*Adapter, Memory, Profile);
}

void CELGround::FillCombatDebug(SELDebugRecord& Record) const
{
	Record.Mode = "Ground";
	CloseScent.FillDebug(Record, Profile);
	Record.State = BehaviorManager.HasActiveBehavior() ? BehaviorManager.GetValidation().Reason : "native_control";
	if (IsFleeingPredator)
	{
		Tactics.FillDebug(Record);
	}
	FillGroundDebug(Record);
}

// Queue only a real physical impact; its direction does not reveal a hidden attacker's position.
void CELGround::RecordCombatHit(CEntityAlive& Opponent, float HealthFraction, bool Received)
{
	CELComponentBase::RecordCombatHit(Opponent, HealthFraction, Received);
	const CBaseMonster& Monster = Adapter->Monster();
	if (Received && HealthFraction > 0.f && !Profile.IsPassive && Adapter->IsEnemy(Opponent) &&
		Monster.ExtendedPhysicalHitAt == Device.dwTimeGlobal && Monster.ExtendedPhysicalHitSource == Opponent.ID())
	{
		Tactics.RegisterHit(Profile, Monster.ExtendedPhysicalHitDirection);
	}
}

// Commit a perceived opportunity long enough to act; fear cannot cancel it on the following update.
void CELGround::BeginDefensiveCounter(CEntityAlive& Target)
{
	const u32 Now = Device.dwTimeGlobal;
	SEncounter& State = Encounter(Target);
	State.Fleeing = State.Withdrawn = false;
	State.Engaged = true;
	State.ClosePressureSince = 0;
	State.DecisionUntil = State.NextDecisionAt = Now + Profile.CharacterDecisionHold;
	State.HasCombatDecision = State.CombatAccepted = true;
	State.CombatDecisionAt = Now;
	State.CombatReason = "observed_counter_opportunity";
	if (CELSharedDebug::Instance().CanLogCombat(Adapter->Monster().Position()))
	{
		CELSharedDebug::Instance().LogCombat(Adapter->Monster().ID(), "[monsters_debug] COUNTERATTACK t=%u id=%u target=%u reason=observed_counter_opportunity hold_ms=%u", Now, Adapter->Monster().ID(), Target.ID(), Profile.CharacterDecisionHold);
	}
	CharacterCounterTarget = Target.ID();
	CharacterCounterAttackUntil = State.DecisionUntil;
	IsFleeingPredator = IsWatchingThreat = false;
	PublishNativeMemory();
	Adapter->Monster().EnemyMan.update();
}
