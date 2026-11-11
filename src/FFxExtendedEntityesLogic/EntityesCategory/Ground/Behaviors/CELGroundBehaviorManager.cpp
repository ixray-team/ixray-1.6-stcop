#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorManager.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGround.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"
#include <algorithm>
#include "xrGame/EntityCondition.h"

// Связывает выбор наземных схем поведения с необходимыми службами и состоянием владельца.
CELGroundBehaviorManager::CELGroundBehaviorManager(CELGround& OwnerLogic, IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile)
	: Owner(OwnerLogic), Adapter(OwnerAdapter), Profile(OwnerProfile), ReactionBehavior(OwnerAdapter, OwnerProfile), BodyBehavior(OwnerAdapter, OwnerProfile) {}
// Завершает выбор наземных схем поведения и освобождает связанные ресурсы.
CELGroundBehaviorManager::~CELGroundBehaviorManager()
{
	Clear();
}

// Выбирает и обновляет наземную схему по текущей ситуации.
EELControl CELGroundBehaviorManager::UpdateGround(float DeltaTime)
{
	Owner.UpdateFieldExposure();
	if (!Owner.HasAttached || !Profile.CanUseBehaviors || !Adapter.CanControl())
	{
		Clear();
		return EELControl::Native;
	}
	CBaseMonster& Monster = Adapter.Monster();
	const float Health = Monster.conditions().GetHealth() / std::max(EPS_L, Monster.conditions().GetMaxHealth());
	if (!Profile.IsPassive && Profile.CanCornerBerserk && Health < Profile.CornerBerserkHealth && IsActive(ReactionBehavior) && ReactionBehavior.IsCornered())
	{
		Monster.CorneredBerserkUntil = Device.dwTimeGlobal + Profile.CornerBerserkDuration;
		Monster.CorneredBerserkDamage = Profile.CornerBerserkDamage;
		Monster.set_berserk();
		Clear();
	}
	const bool IsBerserk = s32(Monster.CorneredBerserkUntil - Device.dwTimeGlobal) > 0;
	if (IsBerserk)
	{
		Adapter.SetFieldEscapePermission(false);
	}
	if (Owner.IsFieldRecovering && !IsBerserk)
	{
		Fvector Source;
		u32 Until;
		const bool Inside = CBaseMonster::PanicZonePressure(Monster.Position(), Source, Until, Monster.ThreatTier) > 0.f;
		Fvector Goal;
		const bool HasStimulus = Owner.Memory.Find(Owner.TargetIdentifier) || s32(Owner.Hearing.InvestigateUntil - Device.dwTimeGlobal) > 0;
		if (Inside)
		{
			ReactionBehavior.Configure(Source, Device.dwTimeGlobal + 1000, true);
		}
		else if (HasStimulus && CBaseMonster::FindPanicPerimeter(Monster.Position(), Monster.ThreatTier, Monster.ID() % 2 ? .12f : -.12f, Goal))
		{
			ReactionBehavior.Configure(Goal, Device.dwTimeGlobal + 1000, false);
		}
		else
		{
			Clear();
			CELGroundMovement Idle(Adapter);
			Idle.Stop();
			return EELControl::Extended;
		}
		Select(&ReactionBehavior);
		Update(DeltaTime);
		SetValidationReason(Inside ? "Головная боль: покидаю поле" : "Восстановление: кружу снаружи поля");
		return EELControl::Extended;
	}
	// Используем то же назначение, которое Observe уже опубликовал штатному EnemyMan.
	const SELObservation* SelectedObservation = Profile.IsPassive ? nullptr : Owner.Memory.Find(Owner.TargetIdentifier);
	CEntityAlive* SelectedTarget = SelectedObservation ? ExtendedEntityes::FindEntity(SelectedObservation->TargetIdentifier) : nullptr;
	if (!SelectedTarget || !Adapter.IsEnemy(*SelectedTarget) || !Adapter.CanPursue(*SelectedTarget, SelectedObservation->Position))
	{
		SelectedObservation = nullptr;
		Owner.TargetIdentifier = u16(-1);
	}
	if (Owner.ShouldFleePredator(SelectedObservation))
	{
		ReactionBehavior.Configure(SelectedObservation->Position, Device.dwTimeGlobal + 1000, true);
		Select(&ReactionBehavior);
		Update(DeltaTime);
		SetValidationReason("Бегство от видового хищника");
		return EELControl::Extended;
	}
	const bool HasThreat = SelectedObservation && CELSharedTargets::ThreatPriority(*SelectedObservation, Adapter);
	if (!IsBerserk && UpdateEscape(DeltaTime))
	{
		return EELControl::Extended;
	}
	const bool IsPanicking = !IsBerserk && !HasThreat && (!SelectedObservation || Owner.Hearing.DamagePanic) && s32(Device.dwTimeGlobal - Owner.Hearing.PanicUntil) < 0;
	const bool IsInvestigating = !SelectedObservation && s32(Device.dwTimeGlobal - Owner.Hearing.InvestigateUntil) < 0;
	if (IsPanicking || IsInvestigating)
	{
		Owner.TargetIdentifier = u16(-1);
		ReactionBehavior.Configure(Owner.Hearing.AttentionPosition, IsPanicking ? Owner.Hearing.PanicUntil : Owner.Hearing.InvestigateUntil, IsPanicking);
		Select(&ReactionBehavior);
		Update(DeltaTime);
		return EELControl::Extended;
	}
	if (!SelectedObservation)
	{
		if (UpdateInvestigation(DeltaTime))
		{
			return EELControl::Extended;
		}
		Clear();
		return EELControl::Native;
	}
	return Owner.UpdateTarget(*SelectedObservation, DeltaTime);
}


// Выбирает и ведёт уход от известной опасности.
bool CELGroundBehaviorManager::UpdateEscape(float DeltaTime)
{
	xr_vector<SELNavigationObstacle> Hazards;
	CELSharedAnomalies::Snapshot(Adapter.Monster().ID(), Hazards);
	CELSharedDeaths::AppendObstacles(Adapter.Monster().ID(), Hazards);
	xr_vector<SELNavigationObstacle>::const_iterator Hazard;
	for (Hazard = Hazards.begin(); Hazard != Hazards.end(); ++Hazard)
	{
		if (Adapter.Monster().Position().distance_to_sqr(Hazard->Position) >= Hazard->Radius * Hazard->Radius)
		{
			continue;
		}
		ReactionBehavior.Configure(Hazard->Position, Device.dwTimeGlobal + Profile.PeripheralInterval, true);
		Select(&ReactionBehavior);
		Update(DeltaTime);
		return true;
	}
	return false;
}

// Выбирает и ведёт исследование интересующей точки.
bool CELGroundBehaviorManager::UpdateInvestigation(float DeltaTime)
{
	if (s32(Device.dwTimeGlobal - BodyRetryAfter) < 0)
	{
		return false;
	}
	SELObservation Evidence;
	if (!CELSharedDeaths::Claim(Adapter.Monster(), Profile, Evidence))
	{
		if (IsActive(BodyBehavior))
		{
			Clear();
		}
		return false;
	}
	BodyBehavior.SetObservation(Evidence);
	Select(&BodyBehavior);
	Update(DeltaTime);
	const SELBehaviorValidation& Result = GetValidation();
	if (Result.State == EELBehaviorState::Completed || !IsActive(BodyBehavior))
	{
		Clear();
		BodyRetryAfter = Device.dwTimeGlobal + 5000;
	}
	return true;
}
