#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundApproach.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "xrGame/monster_scale.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundLocalSearch.h"
#include "xrGame/Level.h"

// Сбрасывает временное состояние перед новым действием.
void CELGroundApproach::Reset()
{
	ReconsiderAt = LastSampleAt = 0;
	ObservedVelocity.set(0.f, 0.f, 0.f);
	IsIntercepting = HasConcealedPosition = false;
	GoalVertex = u32(-1);
}

// Выбирает разрешённый способ подхода к текущему противнику.
void CELGroundApproach::ChooseStrategy(IELAdapter& Adapter, const SELPerceptionProfile& Profile, float Distance, bool HasVision, u32 NearbyCount, const Fvector& KnownPosition)
{
	const auto Allowed = [&Profile](EELApproachStrategy Candidate) { return (Profile.Strategies & ELApproachStrategyMask(Candidate)) != 0; };
	Strategy = EELApproachStrategy::Direct;
	if (!HasVision && !Profile.CanTrackLive)
	{
		// Pursue the known point without reading the hidden target or slowing to an investigation walk.
		return;
	}
	if (Adapter.Monster().GetfHealth() < Profile.RetreatHealthThreshold && Allowed(EELApproachStrategy::Retreat))
	{
		Strategy = EELApproachStrategy::Retreat;
		return;
	}
	// Immediate joint pressure wins over a lengthy orbit when the pack is already close.
	if (Distance < Profile.NativeAttackDistance * (NearbyCount + 1 >= Profile.DirectRushMinMembers ? 2.f : 1.f))
	{
		return;
	}
	if (NearbyCount && GroupRank > 0 && !IsObserved)
	{
		if (Allowed(EELApproachStrategy::Encircle))
		{
			Strategy = EELApproachStrategy::Encircle;
		}
		else if (Allowed(EELApproachStrategy::Wide))
		{
			Strategy = EELApproachStrategy::Wide;
		}
		else if (Allowed(EELApproachStrategy::Flank))
		{
			Strategy = EELApproachStrategy::Flank;
		}
		return;
	}
	if (Profile.CanAttackJump && Distance < Profile.LungeMaxDistance && Allowed(EELApproachStrategy::Lunge) && Adapter.Monster().com_man().get_jump_control())
	{
		Strategy = EELApproachStrategy::Lunge;
		return;
	}
	if (Distance > Profile.ChargeMinDistance && Allowed(EELApproachStrategy::Charge))
	{
		Strategy = EELApproachStrategy::Charge;
	}
	else if (NearbyCount && Allowed(EELApproachStrategy::Pressure))
	{
		Strategy = EELApproachStrategy::Pressure;
	}

}

// Выбирает достижимую точку подхода по текущей стратегии и расположению союзников.
void CELGroundApproach::Select(IELAdapter& Adapter, const SELPerceptionProfile& Profile, const Fvector& KnownPosition, bool HasVision, CEntityAlive* Target, bool ForceDirect)
{
	CBaseMonster& OwnerMonster = Adapter.Monster();
	const float PathScale = MonsterScale::Navigation(OwnerMonster.GetVisualScale());
	const u32 Now = Device.dwTimeGlobal;
	if (ForceDirect)
	{
		ReconsiderAt = 0;
	}
	const u16 CurrentTarget = Target ? u16(Target->ID()) : u16(-1);
	if (ObservedTarget != CurrentTarget)
	{
		LastSampleAt = ReconsiderAt = 0;
		ObservedVelocity.set(0.f, 0.f, 0.f);
		ObservedTarget = CurrentTarget;
	}
	if ((HasVision || Profile.CanTrackLive) && LastSampleAt && Now > LastSampleAt)
	{
		const float Delta = float(Now - LastSampleAt) * .001f;
		if (Delta < .5f)
		{
			Fvector Sample;
			Sample.sub(KnownPosition, LastKnownPosition).div(Delta);
			Sample.y = 0.f;
			if (Sample.magnitude() <= 50.f)
			{
				ObservedVelocity.lerp(ObservedVelocity, Sample, clampr(Delta * 5.f, 0.f, 1.f));
			}
		}
		else
		{
			ObservedVelocity.set(0.f, 0.f, 0.f);
		}
	}
	else if (!HasVision && !Profile.CanTrackLive)
	{
		ObservedVelocity.set(0.f, 0.f, 0.f);
	}
	const Fvector PreviousKnown = LastKnownPosition;
	LastKnownPosition = KnownPosition;
	LastSampleAt = Now;
	if (s32(Device.dwTimeGlobal - ReconsiderAt) < 0 && PreviousKnown.distance_to_sqr(KnownPosition) < _sqr(CELSharedSettings::Work().GroundRouteReplanDistance) &&
		OwnerMonster.Position().distance_to_sqr(GoalPosition) > 2.25f * _sqr(PathScale))
	{
		return;
	}
	const SELHuntGroup Group = CELSharedHuntGroups::Observe(OwnerMonster, Target ? Target->ID() : u16(-1), std::min(Profile.TargetCrowdDistance * PathScale, Profile.SharingRadius * Profile.CallSharingRatio));
	GroupRank = Group.Rank;
	GroupCount = Group.Count;
	HasAlly = Group.HasAlly;
	AllyPosition = Group.AllyPosition;
	Fvector ToOwner;
	ToOwner.sub(OwnerMonster.Position(), KnownPosition).normalize_safe();
	IsObserved = HasVision && Target && Target->Direction().dotproduct(ToOwner) > .3f;
	const Fvector Separation = Group.Separation;
	const u32 NearbyCount = Group.Count > 0 ? Group.Count - 1 : 0;
	const float Distance = OwnerMonster.Position().distance_to(KnownPosition);
	ChooseStrategy(Adapter, Profile, Distance, HasVision, NearbyCount, KnownPosition);
	if (ForceDirect)
	{
		Strategy = EELApproachStrategy::Direct;
	}
	Fvector Forward;
	Forward.sub(OwnerMonster.Position(), KnownPosition);
	Forward.y = 0.f;
	Forward.normalize_safe();
	Fvector Lateral = {Forward.z, 0.f, -Forward.x};
	Lateral.mul(OwnerMonster.ID() % 2 ? 1.f : -1.f);
	const float CommunicationReach = Profile.SharingRadius * Profile.CallSharingRatio * .8f;
	const float Width = std::min(std::min(Profile.ApproachWidth * PathScale, Distance * .45f), CommunicationReach);
	GoalPosition = KnownPosition;
	IsIntercepting = HasConcealedPosition = IsDrivingToAllies = false;
	if (HasVision || Profile.CanTrackLive)
	{
		if (Strategy == EELApproachStrategy::Flank || Strategy == EELApproachStrategy::Wide || Strategy == EELApproachStrategy::Encircle || Strategy == EELApproachStrategy::Pressure)
		{
			GoalPosition.mad(Lateral, Width * (Strategy == EELApproachStrategy::Wide ? 1.6f : 1.f));
			if (Strategy == EELApproachStrategy::Encircle && Target)
			{
				const float Side = GroupRank % 2 ? 1.f : -1.f;
				GoalPosition = KnownPosition;
				const float Lead = std::min(std::min(Profile.GroupInterceptDistance, Distance * .5f), CommunicationReach);
				Fvector Travel = ObservedVelocity;
				const bool MovingTarget = Travel.square_magnitude() > 1.f;
				if (MovingTarget)
				{
					Travel.normalize_safe();
					GoalPosition.mad(Travel, Lead);
					IsIntercepting = true;
				}
				else if (HasVision)
				{
					GoalPosition.mad(Target->Direction(), -Lead);
				}
				GoalPosition.mad(Lateral, Width * Side);
				if (Profile.CanAmbush && !IsObserved && ExtendedEntityes::Navigation().TryGeometryRays(1))
				{
					Fvector Point;
					u32 Vertex;
					if (CELGroundLocalSearch::Resolve(Adapter, GoalPosition, Point, Vertex))
					{
						Fvector Eye = Point;
						Eye.y += std::max(.5f, OwnerMonster.BoundingBox().max.y);
						Fvector Direction;
						Direction.sub(KnownPosition, Eye);
						const float Range = Direction.magnitude();
						if (Range > EPS_L)
						{
							Direction.div(Range);
							collide::rq_result Obstacle;
							HasConcealedPosition = Level().ObjectSpace.RayPick(Eye, Direction, Range, collide::rqtStatic, Obstacle, &OwnerMonster);
						}
					}
				}
			}
		}
		else if (Strategy == EELApproachStrategy::Retreat && OwnerMonster.GetfHealth() < Profile.RetreatHealthThreshold)
		{
			if (HasAlly)
			{
				GoalPosition = AllyPosition;
			}
			GoalPosition.mad(Forward, 8.f * PathScale);
		}
		else if (Strategy == EELApproachStrategy::Direct && NearbyCount)
		{
			if (GroupRank == 0 && Group.HasBlockingPosition && Distance > Profile.NativeAttackDistance)
			{
				Fvector PressureSide;
				PressureSide.sub(KnownPosition, Group.BlockingPosition);
				PressureSide.y = 0.f;
				PressureSide.normalize_safe();
				GoalPosition.mad(PressureSide, std::min(Width, Distance * .3f));
				IsDrivingToAllies = true;
			}
			else
			{
				GoalPosition.mad(Lateral, std::min(Width, 1.5f * PathScale));
			}
		}
		GoalPosition.add(Separation);
	}
	GoalVertex = ai().level_graph().vertex_id(GoalPosition);
	if (ai().level_graph().valid_vertex_id(GoalVertex))
	{
		GoalPosition.y = ai().level_graph().vertex_plane_y(GoalVertex, GoalPosition.x, GoalPosition.z);
	}
	if (!Adapter.CanReach(GoalPosition, GoalVertex))
	{
		IsIntercepting = HasConcealedPosition = IsDrivingToAllies = false;
		GoalPosition = KnownPosition;
		GoalVertex = ai().level_graph().vertex_id(GoalPosition);
	}
	if (ai().level_graph().valid_vertex_id(GoalVertex))
	{
		GoalPosition.y = ai().level_graph().vertex_plane_y(GoalVertex, GoalPosition.x, GoalPosition.z);
	}
	CELSharedHuntGroups::ReportPosition(OwnerMonster, GoalPosition, CurrentTarget, IsIntercepting || HasConcealedPosition);
	IsRunning = true;
	LastKnownPosition = KnownPosition;
	ReconsiderAt = Device.dwTimeGlobal + (Strategy == EELApproachStrategy::Charge ? 500 : 400 + OwnerMonster.ID() % 201);
}

// Возвращает короткое имя текущей стратегии для отладки.
const char* CELGroundApproach::Description() const
{
	if (IsDrivingToAllies)
	{
		return "Вытеснение к позиции союзника";
	}
	if (HasConcealedPosition)
	{
		return "Скрытый выход к засаде";
	}
	if (IsIntercepting)
	{
		return "Перехват по наблюдаемому движению";
	}
	static const char* Names[] = {"Прямой подход", "Обход с фланга", "Широкая дуга", "Сближение перед прыжком", "Рывок", "Окружение", "Дуга сближения", "Отступление"};
	static_assert(std::size(Names) == u32(EELApproachStrategy::Count));
	return Names[u32(Strategy)];
}
