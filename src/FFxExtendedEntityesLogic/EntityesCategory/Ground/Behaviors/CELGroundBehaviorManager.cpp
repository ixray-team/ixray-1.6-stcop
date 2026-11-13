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
#include <cmath>
#include "xrGame/ai/monsters/control_manager_custom.h"
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
	if (!Profile.IsPassive && Profile.CanCornerBerserk && Health < Profile.CornerBerserkHealth && IsActive(ReactionBehavior) && ReactionBehavior.IsCornered() && (!Profile.HasCharacter || Owner.HasEnemyPressure()))
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
		const bool Inside = CBaseMonster::PanicZonePressure(Monster.Position(), Source, Until, Monster.EmmiPainProtection) > 0.f;
		Fvector Goal;
		const bool HasStimulus = Owner.Memory.Find(Owner.TargetIdentifier) || s32(Owner.Hearing.InvestigateUntil - Device.dwTimeGlobal) > 0;
		if (Inside)
		{
			ReactionBehavior.Configure(Source, Device.dwTimeGlobal + Profile.ReactionCommandTime, true);
		}
		else if (HasStimulus && CBaseMonster::FindPanicPerimeter(Monster.Position(), Monster.EmmiPainProtection, Monster.ID() % 2 ? .12f : -.12f, Goal))
		{
			ReactionBehavior.Configure(Goal, Device.dwTimeGlobal + Profile.ReactionCommandTime, false);
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
		if (Owner.IsWatchingThreat && SelectedObservation && SelectedTarget)
		{
			if (Adapter.CanSee(*SelectedTarget))
			{
				ReactionBehavior.ConfigureWatching(SelectedObservation->Position, SelectedTarget->Direction(), Device.dwTimeGlobal + Profile.ReactionCommandTime);
			}
			else
			{
				ReactionBehavior.Configure(SelectedObservation->Position, Device.dwTimeGlobal + Profile.ReactionCommandTime, false);
			}
		}
		else
		{
			ReactionBehavior.Configure(SelectedObservation ? SelectedObservation->Position : Owner.LastFearPosition, Device.dwTimeGlobal + Profile.ReactionCommandTime, true);
		}
		Select(&ReactionBehavior);
		Update(DeltaTime);
		SetValidationReason(Owner.IsWatchingThreat ? "Осторожное сопровождение: держу дистанцию и обхожу сбоку" : Profile.HasCharacter ? "Отступление: характер и полученный отпор" : "Бегство от видового хищника");
		return EELControl::Extended;
	}
	// A perceived but inaccessible enemy is a waiting stimulus, not a stale sound task.
	for (const SELObservation& Known : Owner.Memory.GetRecords())
	{
		if (SelectedObservation && Known.TargetIdentifier != SelectedObservation->TargetIdentifier) { continue; }
		CEntityAlive* VisibleEnemy = ExtendedEntityes::FindEntity(Known.TargetIdentifier);
		if (!VisibleEnemy || !Adapter.IsEnemy(*VisibleEnemy) || !Adapter.CanSee(*VisibleEnemy) || !Owner.CanEngage(Known)) { continue; }
		const bool CanPursue = Adapter.CanPursue(*VisibleEnemy, Known.Position);
		if (CanPursue && Adapter.CanReach(Known.Position, Known.VertexIdentifier)) { continue; }
		// Preserve legal bites and native jumps at graph edges, and the aerial interception executor.
		if (CanPursue && ((!VisibleEnemy->UsedAI_Locations() && Profile.CanInterceptFlying) || Monster.MeleeChecker.can_start_melee(VisibleEnemy))) { continue; }
		if (CanPursue && Profile.CanAttackJump && Monster.com_man().get_jump_control() && Monster.Position().distance_to_sqr(Known.Position) <= _sqr(Profile.LungeMaxDistance) && Monster.com_man().check_if_jump_possible(Known.Position, true)) { continue; }
		if (Owner.ShouldFleePredator(&Known))
		{
			ReactionBehavior.Configure(Known.Position, Device.dwTimeGlobal + Profile.ReactionCommandTime, true);
		}
		else
		{
			Fvector Away = Fvector().sub(Monster.Position(), Known.Position);
			const float Angle = atan2f(Away.x, Away.z) + (Monster.ID() % 2 ? .35f : -.35f);
			const float Radius = std::max(3.f, Profile.NativeAttackDistance * 1.5f);
			Fvector WaitingPoint = Known.Position;
			WaitingPoint.x += sinf(Angle) * Radius;
			WaitingPoint.z += cosf(Angle) * Radius;
			WaitingPoint.y = Monster.Position().y;
			ReactionBehavior.Configure(WaitingPoint, Device.dwTimeGlobal + Profile.ReactionCommandTime, false);
		}
		Select(&ReactionBehavior);
		Update(DeltaTime);
		SetValidationReason(Owner.IsFleeingPredator ? "Отход от недосягаемой угрозы" : "Ожидание: противник обнаружен, подход недоступен");
		return EELControl::Extended;
	}
	const bool HasThreat = SelectedObservation && CELSharedTargets::ThreatPriority(*SelectedObservation, Adapter);
	if (!IsBerserk && UpdateEscape(DeltaTime))
	{
		return EELControl::Extended;
	}
	const bool IsPanicking = !IsBerserk && !HasThreat && (!SelectedObservation || Owner.Hearing.DamagePanic) && s32(Device.dwTimeGlobal - Owner.Hearing.PanicUntil) < 0;
	Fvector Rally;
	bool WaitForPack = false;
	if (!SelectedObservation && !IsPanicking && Owner.RegroupPoint(Rally, WaitForPack))
	{
		if (WaitForPack)
		{
			Clear();
			CELGroundMovement Idle(Adapter);
			Idle.Stop();
		}
		else
		{
			ReactionBehavior.Configure(Rally, Device.dwTimeGlobal + Profile.ReactionCommandTime, false);
			Select(&ReactionBehavior);
			Update(DeltaTime);
		}
		SetValidationReason(WaitForPack ? "Сбор стаи: ожидаю сородичей" : "Сбор стаи: возвращаюсь к сородичам");
		return EELControl::Extended;
	}
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
