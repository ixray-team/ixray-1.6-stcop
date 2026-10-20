#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlying.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "Level.h"

// Связывает общую логику воздушной категории с необходимыми службами и состоянием владельца.
CELFlying::CELFlying(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELComponentBase(std::move(OwnerAdapter), OwnerProfile), FlightSettings(CELFlyingSettings::Read(*Adapter->Monster().cNameSect())), Movement(*Adapter, FlightSettings), BehaviorManager(*this, *Adapter, FlightSettings, Movement) {}
// Завершает общую логику воздушной категории и освобождает связанные ресурсы.
CELFlying::~CELFlying()
{
	ClearBehavior();
}
// Завершает выбранную схему и очищает управление категории.
void CELFlying::ClearBehavior()
{
	BehaviorManager.Clear();
}
// Выбирает воздушную схему через общий механизм её жизненного цикла.
void CELFlying::SelectFlight(CELFlyingBehaviorBase& Behavior)
{
	BehaviorManager.SelectFlight(Behavior);
}
// Снимает управление расширения и освобождает регистрации владельца.
void CELFlying::Detach()
{
	ClearBehavior();
	Movement.Stop();
	CELComponentBase::Detach();
}
// Обновляет общую логику воздушной категории и обрабатывает текущий результат выполнения.
EELControl CELFlying::Update(float DeltaTime)
{
	return BehaviorManager.UpdateFlight(DeltaTime);
}

// Возвращает цель для отображения в общей отладке.
Fvector CELFlying::DebugGoal() const
{
	return BehaviorManager.Goal();
}
// Собирает противников со всех направлений, включая снизу; память пополняется только после проверки видимости.
void CELFlying::ObserveVision(CBaseMonster& OwnerMonster, u32 CurrentTime)
{
	if (Profile.IsPassive || s32(CurrentTime - NextSphereVisionAt) < 0 || !ExtendedEntityes::PerceptionBudget().TrySpatial(OwnerMonster.ID()))
	{
		return;
	}
	NextSphereVisionAt = CurrentTime + 100;
	xr_vector<ISpatialShared> Candidates;
	Fvector ViewOrigin;
	OwnerMonster.Center(ViewOrigin);
	g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, ViewOrigin, OwnerMonster.eye_range);
	u32 Visited = 0;
	u32 Checked = 0;
	CObject* Object;
	CEntityAlive* Candidate;
	while (Visited < Candidates.size() && Checked < CELSharedSettings::Work().PerceptionRays)
	{
		VisionCursor %= u32(Candidates.size());
		Object = Candidates[VisionCursor] ? Candidates[VisionCursor]->dcast_CObject() : nullptr;
		++VisionCursor;
		++Visited;
		Candidate = Object ? Object->cast_entity_alive() : nullptr;
		if (!Candidate || !Adapter->IsEnemy(*Candidate))
		{
			continue;
		}
		++Checked;
		if (Adapter->CanSee(*Candidate) && HasVisualAcquired(*Candidate, CurrentTime))
		{
			RememberEntity(*Candidate, EELObservationSource::Vision, CurrentTime);
		}
	}
}

// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELFlying::FrameUpdate(float DeltaTime)
{
	if (!HasAttached)
	{
		return;
	}
	const bool CanMove = Adapter->CanControl();
	if (CanMove)
	{
		if (TargetIdentifier == u16(-1))
		{
			ObserveVision(Adapter->Monster(), Device.dwTimeGlobal);
		}
		BehaviorManager.FrameUpdate(DeltaTime);
		float RemainingTime = clampr(DeltaTime, 0.f, .1f);
		u32 Handoffs = 0;
		do
		{
			Movement.Update(RemainingTime);
			RemainingTime = Movement.UnusedTime();
			BehaviorManager.AfterMovement();
			++Handoffs;
		} while (RemainingTime > EPS_L && Movement.HasRoute() && Handoffs < 4);
	}
	if (!CELSharedDebug::Instance().IsDrawEnabled())
	{
		return;
	}
	SELDebugRecord Record;
	Record.OwnerIdentifier = Adapter->Monster().ID();
	Record.IgnoresFoliage = CELSharedSettings::IsEnabled() && Profile.CanIgnoreFoliage;
	Adapter->Monster().Center(Record.Position);
	Record.GoalPosition = DebugGoal();
	Record.Speed = Movement.GetSpeed();
	Record.TargetIdentifier = TargetIdentifier;
	Record.Radius = FlightSettings.CollisionRadius;
	Record.IsAttacking = CanMove && IsAttacking();
	if (Record.IsAttacking)
	{
		CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(TargetIdentifier);
		if (TargetEntity && Adapter->IsEnemy(*TargetEntity))
		{
			TargetEntity->Center(Record.GoalPosition);
		}
	}
	Record.KnownTargets = u32(Memory.GetRecords().size());
	const SELBehaviorValidation CurrentValidation = BehaviorManager.FlightValidation();
	Record.Validation = CanMove ? CurrentValidation.State : EELBehaviorState::WaitingForResult;
	Record.State = CanMove ? CurrentValidation.Reason : "Полёт заблокирован штатным или внешним управлением";
	Record.Mode = BehaviorManager.FlightMode();
	FillDebugRecord(Record);
	CELSharedDebug::Instance().Publish(std::move(Record));
}

// Прекращает преследование скрытой цели без отмены слухового исследования.
void CELFlying::ForgetTarget(u16 ObjectIdentifier)
{
	if (TargetIdentifier == ObjectIdentifier)
	{
		ClearBehavior();
	}
	CELComponentBase::ForgetTarget(ObjectIdentifier);
}

// Отменяет преследование удалённой либо игнорируемой цели вместе с подготовленным маршрутом.
void CELFlying::RemoveObject(u16 ObjectIdentifier)
{
	if (TargetIdentifier == ObjectIdentifier || Hearing.IsReactingTo(ObjectIdentifier))
	{
		ClearBehavior();
	}
	CELComponentBase::RemoveObject(ObjectIdentifier);
}
