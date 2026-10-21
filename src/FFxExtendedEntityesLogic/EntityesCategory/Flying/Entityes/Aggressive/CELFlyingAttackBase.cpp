#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackBase.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"

// Связывает общую логику атакующих летунов с необходимыми службами и состоянием владельца.
CELFlyingAttackBase::CELFlyingAttackBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELFlying(std::move(OwnerAdapter), OwnerProfile), Animation(*Adapter, FlightSettings), AttackBehavior(*Adapter, FlightSettings, Movement, Animation), RecoveryBehavior(*Adapter, FlightSettings, Movement), RestBehavior(*Adapter, FlightSettings, Movement, Animation) {}
// Завершает общую логику атакующих летунов и освобождает связанные ресурсы.
CELFlyingAttackBase::~CELFlyingAttackBase()
{
	ClearBehavior();
}
// Удерживает назначение захода ещё до обновления EnemyMan, не расходуя вторую квоту выбора.
const SELObservation* CELFlyingAttackBase::SelectTarget()
{
	const SELObservation* Observation = BehaviorManager.IsActive(AttackBehavior) && AttackBehavior.HasCommittedTarget() ? Memory.Find(AttackBehavior.CommittedTargetIdentifier()) : nullptr;
	CEntityAlive* CommittedTarget = Observation ? ExtendedEntityes::FindEntity(Observation->TargetIdentifier) : nullptr;
	return CommittedTarget && Adapter->IsEnemy(*CommittedTarget) && Adapter->CanSee(*CommittedTarget) ? Observation : CELComponentBase::SelectTarget();
}

// Обновляет общую логику атакующих летунов и обрабатывает текущий результат выполнения.
EELControl CELFlyingAttackBase::Update(float DeltaTime)
{
	if (!HasAttached || !Adapter->CanControl())
	{
		Animation.Reset();
		return CELFlying::Update(DeltaTime);
	}
	// Используем то же назначение, которое Observe уже опубликовал штатному выбору врага.
	const SELObservation* Observation = Memory.Find(TargetIdentifier);
	TargetIdentifier = Observation ? Observation->TargetIdentifier : u16(-1);
	AttackBehavior.SetObservation(Observation);
	CELFlyingBehaviorBase* DesiredBehavior = ChooseBehavior(Observation, DeltaTime);
	const EELControl Control = BehaviorManager.UpdateFlight(DeltaTime, DesiredBehavior);
	HandleFailure(DesiredBehavior);
	return Control;
}
// Выбирает схему по цели, необходимости восстановления и отдыха.
CELFlyingBehaviorBase* CELFlyingAttackBase::ChooseBehavior(const SELObservation* Observation, float DeltaTime)
{
	const bool HasStimulus = Observation || !Memory.GetRecords().empty() || s32(Device.dwTimeGlobal - Hearing.PanicUntil) < 0 || s32(Device.dwTimeGlobal - Hearing.InvestigateUntil) < 0;
	if (!HasStimulus && Movement.GetSpeed() > EPS_L)
	{
		AirborneTime += std::max(0.f, DeltaTime);
	}
	if (BehaviorManager.IsActive(RecoveryBehavior) && !RecoveryBehavior.ReadyToResume() && BehaviorManager.GetValidation().State != EELBehaviorState::Completed)
	{
		return &RecoveryBehavior;
	}
	if (HasStimulus && BehaviorManager.IsActive(RestBehavior))
	{
		ClearBehavior();
		Animation.Fly();
	}
	if (BehaviorManager.IsActive(RestBehavior) && !HasStimulus)
	{
		if (BehaviorManager.GetValidation().State != EELBehaviorState::Completed)
		{
			return &RestBehavior;
		}
		AirborneTime = 0.f;
	}
	if (Observation && s32(Device.dwTimeGlobal - RetryAfter) >= 0)
	{
		return &AttackBehavior;
	}
	if (!HasStimulus && FlightSettings.RestInterval > 0.f && AirborneTime >= FlightSettings.RestInterval && s32(Device.dwTimeGlobal - NextRestAttempt) >= 0)
	{
		return &RestBehavior;
	}
	return nullptr;
}
// Передаёт отказ схемы ограниченному восстановлению или освобождает управление.
void CELFlyingAttackBase::HandleFailure(CELFlyingBehaviorBase* RequestedBehavior)
{
	if (s32(Device.dwTimeGlobal - Hearing.PanicUntil) < 0)
	{
		return;
	}
	const EELBehaviorState State = BehaviorManager.GetValidation().State;
	const bool HasFailed = State == EELBehaviorState::Blocked || State == EELBehaviorState::NoProgress;
	if (RequestedBehavior == &RestBehavior && !BehaviorManager.IsActive(RestBehavior))
	{
		NextRestAttempt = Device.dwTimeGlobal + FlightSettings.RestRetryDelay;
	}
	if (RequestedBehavior == &RecoveryBehavior && !BehaviorManager.IsActive(RecoveryBehavior))
	{
		RetryAfter = Device.dwTimeGlobal + FlightSettings.RouteRetryDelay;
	}
	if (HasFailed && (RequestedBehavior == &AttackBehavior || RequestedBehavior == &RestBehavior))
	{
		BehaviorManager.SelectFlight(RecoveryBehavior);
	}
}
// Сообщает, действует ли сейчас фаза атаки или её отладочный индикатор.
bool CELFlyingAttackBase::IsAttacking() const
{
	return BehaviorManager.IsActive(AttackBehavior) && AttackBehavior.IsAttacking();
}
