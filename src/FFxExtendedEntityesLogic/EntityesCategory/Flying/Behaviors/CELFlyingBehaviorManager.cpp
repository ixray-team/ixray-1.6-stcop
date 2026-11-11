#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorManager.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlying.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/EntityCondition.h"

// Связывает выбор воздушных схем и реакцию на испуг с необходимыми службами и состоянием владельца.
CELFlyingBehaviorManager::CELFlyingBehaviorManager(CELFlying& OwnerLogic, IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement)
	: Owner(OwnerLogic), Movement(FlightMovement), CruiseBehavior(OwnerAdapter, FlightSettings, FlightMovement) {}
// Завершает выбор воздушных схем и реакцию на испуг и освобождает связанные ресурсы.
CELFlyingBehaviorManager::~CELFlyingBehaviorManager()
{
	Clear();
}
// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELFlyingBehaviorManager::Clear()
{
	CELSharedBehaviorManager::Clear();
	ActiveFlight = nullptr;
}
// Выбирает воздушную схему через общий механизм её жизненного цикла.
void CELFlyingBehaviorManager::SelectFlight(CELFlyingBehaviorBase& Behavior)
{
	Select(&Behavior);
	ActiveFlight = &Behavior;
}
// Обновляет активную воздушную схему и проверяет её состояние.
EELControl CELFlyingBehaviorManager::UpdateFlight(float DeltaTime, CELFlyingBehaviorBase* DesiredBehavior)
{
	Owner.UpdateFieldExposure();
	if (!Owner.HasAttached || !Owner.Profile.CanUseBehaviors || !Owner.Adapter->CanControl())
	{
		Clear();
		Movement.Stop();
		return EELControl::Native;
	}
	CBaseMonster& NativeMonster = Owner.Adapter->Monster();
	const bool WasFleeing = Owner.IsFieldRecovering || Owner.IsFleeingPredator || s32(Owner.Hearing.PanicUntil - Device.dwTimeGlobal) > 0;
	const bool BlockedEscape = WasFleeing && ActiveFlight == &CruiseBehavior && (GetValidation().State == EELBehaviorState::Blocked || GetValidation().State == EELBehaviorState::NoProgress);
	if (BlockedEscape)
	{
		if (!CorneredSince)
		{
			CorneredSince = Device.dwTimeGlobal;
		}
	}
	else
	{
		CorneredSince = 0;
	}
	const float Health = NativeMonster.conditions().GetHealth() / std::max(EPS_L, NativeMonster.conditions().GetMaxHealth());
	if (!Owner.Profile.IsPassive && Owner.Profile.CanCornerBerserk && Health < Owner.Profile.CornerBerserkHealth && CorneredSince && Device.dwTimeGlobal - CorneredSince >= 2000)
	{
		NativeMonster.CorneredBerserkUntil = Device.dwTimeGlobal + Owner.Profile.CornerBerserkDuration;
		NativeMonster.CorneredBerserkDamage = Owner.Profile.CornerBerserkDamage;
		CorneredSince = 0;
	}
	const bool IsBerserk = s32(NativeMonster.CorneredBerserkUntil - Device.dwTimeGlobal) > 0;
	if (IsBerserk)
	{
		Owner.Adapter->SetFieldEscapePermission(false);
	}
	if (Owner.IsFieldRecovering && !IsBerserk)
	{
		CBaseMonster& Monster = Owner.Adapter->Monster();
		Fvector Source;
		u32 Until;
		const bool Inside = CBaseMonster::PanicZonePressure(Monster.Position(), Source, Until, Monster.ThreatTier) > 0.f;
		Fvector Goal;
		const bool HasStimulus = Owner.Memory.Find(Owner.TargetIdentifier) || s32(Owner.Hearing.InvestigateUntil - Device.dwTimeGlobal) > 0;
		if (Inside)
		{
			CruiseBehavior.SetAttention(Source, true);
		}
		else if (HasStimulus && CBaseMonster::FindPanicPerimeter(Monster.Position(), Monster.ThreatTier, Monster.ID() % 2 ? .12f : -.12f, Goal))
		{
			CruiseBehavior.SetAttention(Goal, false);
		}
		else
		{
			Clear();
			Movement.Stop();
			return EELControl::Extended;
		}
		SelectFlight(CruiseBehavior);
		Update(DeltaTime);
		return EELControl::Extended;
	}
	const SELObservation* Target = Owner.Memory.Find(Owner.TargetIdentifier);
	if (Owner.ShouldFleePredator(Target))
	{
		CruiseBehavior.SetAttention(Target->Position, true);
		SelectFlight(CruiseBehavior);
		Update(DeltaTime);
		return EELControl::Extended;
	}
	const bool HasThreat = Target && CELSharedTargets::ThreatPriority(*Target, *Owner.Adapter);
	const bool IsPanicking = !IsBerserk && !HasThreat && (!Target || Owner.Hearing.DamagePanic) && s32(Device.dwTimeGlobal - Owner.Hearing.PanicUntil) < 0;
	const bool IsInvestigating = !HasThreat && !DesiredBehavior && s32(Device.dwTimeGlobal - Owner.Hearing.InvestigateUntil) < 0;
	if (IsPanicking || IsInvestigating)
	{
		Owner.TargetIdentifier = u16(-1);
		CruiseBehavior.SetAttention(Owner.Hearing.AttentionPosition, IsPanicking);
		SelectFlight(CruiseBehavior);
	}
	else if (DesiredBehavior)
	{
		SelectFlight(*DesiredBehavior);
	}
	else if (Owner.FlightSettings.CanWander)
	{
		CruiseBehavior.ClearAttention();
		SelectFlight(CruiseBehavior);
	}
	else
	{
		Clear();
		Movement.Stop();
		return EELControl::Extended;
	}
	Update(DeltaTime);
	return EELControl::Extended;
}
// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELFlyingBehaviorManager::FrameUpdate(float DeltaTime)
{
	if (ActiveFlight && IsActive(*ActiveFlight))
	{
		ActiveFlight->FrameUpdate(DeltaTime);
	}
}
// Возвращает текущую точку назначения активной схемы.
Fvector CELFlyingBehaviorManager::Goal() const
{
	return Movement.HasRoute() ? Movement.EndPosition() : Owner.Adapter->Monster().Position();
}

// Читает состояние активного участка покадрово, не ожидая очередного планового обновления поведения.
SELBehaviorValidation CELFlyingBehaviorManager::FlightValidation() const
{
	return ActiveFlight && IsActive(*ActiveFlight) ? ActiveFlight->Validate() : SELBehaviorValidation{EELBehaviorState::WaitingForResult, "Ожидание выбора воздушной схемы"};
}

// Сохраняет название выбранного режима в подписи даже при ожидании продолжения пути.
const char* CELFlyingBehaviorManager::FlightMode() const
{
	return ActiveFlight && IsActive(*ActiveFlight) ? ActiveFlight->ModeDescription() : "НЕТ АКТИВНОЙ СХЕМЫ";
}

// Читает возраст текущего запроса без запуска нового поиска.
u32 CELFlyingBehaviorManager::PendingRouteAge() const
{
	return ActiveFlight && IsActive(*ActiveFlight) ? ActiveFlight->PendingRouteAge() : 0;
}
// Читает задержку последнего готового начала маршрута, не суммируя возраст потоковых хвостов.
u32 CELFlyingBehaviorManager::LastRouteReadyMilliseconds() const
{
	return ActiveFlight && IsActive(*ActiveFlight) ? ActiveFlight->LastReadyMilliseconds() : 0;
}

// Передаёт активной схеме результат фактического перемещения, включая переход между маршрутами.
void CELFlyingBehaviorManager::AfterMovement()
{
	if (ActiveFlight && IsActive(*ActiveFlight))
	{
		ActiveFlight->AfterMovement();
	}
}
