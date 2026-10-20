#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/monsters/control_animation_base.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/CELGround.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_monster_space.h"
#include "CELGroundMovement.h"
#include "movement_manager.h"

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
	CloseScent.Clear();
	BehaviorManager.Clear();
	CELComponentBase::Detach();
}

// Обновляет общую логику наземной категории и обрабатывает текущий результат выполнения.
EELControl CELGround::Update(float DeltaTime)
{
	return BehaviorManager.UpdateGround(DeltaTime);
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
	if (!HasAttached || !CELSharedDebug::Instance().IsDrawEnabled())
	{
		return;
	}
	SELDebugRecord Record;
	Record.OwnerIdentifier = Adapter->Monster().ID();
	Record.IgnoresFoliage = CELSharedSettings::IsEnabled() && Profile.CanIgnoreFoliage;
	Record.Position = Adapter->Monster().Position();
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
