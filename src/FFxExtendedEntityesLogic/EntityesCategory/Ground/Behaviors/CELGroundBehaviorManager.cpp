#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorManager.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGround.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"

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
	if (!Owner.HasAttached || !Profile.CanUseBehaviors || !Adapter.CanControl())
	{
		Clear();
		return EELControl::Native;
	}
	// Используем то же назначение, которое Observe уже опубликовал штатному EnemyMan.
	const SELObservation* SelectedObservation = Profile.IsPassive ? nullptr : Owner.Memory.Find(Owner.TargetIdentifier);
	CEntityAlive* SelectedTarget = SelectedObservation ? ExtendedEntityes::FindEntity(SelectedObservation->TargetIdentifier) : nullptr;
	if (!SelectedTarget || !Adapter.IsEnemy(*SelectedTarget) || !Adapter.CanPursue(*SelectedTarget, SelectedObservation->Position))
	{
		SelectedObservation = nullptr;
		Owner.TargetIdentifier = u16(-1);
	}
	const bool IsPanicking = s32(Device.dwTimeGlobal - Owner.Hearing.PanicUntil) < 0;
	const bool IsInvestigating = !SelectedObservation && s32(Device.dwTimeGlobal - Owner.Hearing.InvestigateUntil) < 0;
	if (IsPanicking || IsInvestigating)
	{
		Owner.TargetIdentifier = u16(-1);
		ReactionBehavior.Configure(Owner.Hearing.AttentionPosition, IsPanicking ? Owner.Hearing.PanicUntil : Owner.Hearing.InvestigateUntil, IsPanicking);
		Select(&ReactionBehavior);
		Update(DeltaTime);
		return EELControl::Extended;
	}
	if (UpdateEscape(DeltaTime))
	{
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
