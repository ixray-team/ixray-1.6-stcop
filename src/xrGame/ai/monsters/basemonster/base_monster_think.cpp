#include "StdAfx.h"
#include "../../../../xrEngine/MonsterLogicTelemetry.h"
#include "base_monster.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"
#include "../state_manager.h"
#include "../../../../xrPhysics/PhysicsShell.h"
#include "../../../detail_path_manager.h"
#include "../../../level_path_manager.h"
#include "../monster_velocity_space.h"
#include "../../../Level.h"
#include "../../../Actor.h"
#include "../../../../xrCore/Kernel/EngineExternal.h"
#include "../control_animation_base.h"
#include "../controlled_entity.h"
#include "../monster_home.h"
#include "../../../ai_space.h"
#include "../../../level_graph.h"
#include "../../../ai_object_location.h"

void CBaseMonster::Think()
{
	if (!g_Alive() || getDestroy())
		return;

	PROF_EVENT("Base Monster/Think");
	CMonsterLogicTimerScope ThinkTimer(EMonsterLogicTimer::Think);
	if (EnemyMan.get_enemy())
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::CombatUpdates);
	}

	// Инициализировать
	InitThink();
	anim().ScheduledInit();

	// Обновить память
	UpdateMemory();

	// Обновить сквад
	monster_squad().update(this);

	// Запустить FSM
	update_fsm();
}

bool CBaseMonster::ShouldApproachHiddenEnemy()
{
	const CEntityAlive* Target = EnemyMan.get_enemy();
	return EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		!MonsterPeaceful && !EnemyTrackingLive && EnemyCloseDetectionRadius > 0.f &&
		Target && !Target->getDestroy() && Target->g_Alive() &&
		!GetScriptControl() && !(m_controlled && m_controlled->is_under_control()) && !HasDamagePanic() &&
		Position().distance_to_sqr(Target->Position()) <= EnemyCloseDetectionRadius * EnemyCloseDetectionRadius &&
		!EnemyMan.see_enemy_now(Target) && !ShouldUseCloseEnemyCombat() &&
		!HasCloseSightCombat(Target);
}

bool CBaseMonster::HasCloseSightCombat(const CEntityAlive* Target) const
{
	return EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		Target && !Target->getDestroy() && Target->g_Alive() && CloseSightCombatTargetID == Target->ID() &&
		CloseSightCombatUntil && s32(CloseSightCombatUntil - Device.dwTimeGlobal) > 0;
}

bool CBaseMonster::ShouldUseCloseEnemyCombat()
{
	const CEntityAlive* Target = EnemyMan.get_enemy();
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		MonsterPeaceful || EnemyTrackingLive || !Target || Target->getDestroy() || !Target->g_Alive())
	{
		return false;
	}
	const float Distance = std::min(EnemyCloseAttackDistance, EnemyCloseDetectionRadius);
	if (Distance <= 0.f || Position().distance_to_sqr(Target->Position()) > Distance * Distance)
	{
		return false;
	}
	return HasOpenEnemyCorridor(Target->Position(), Target->ai_location().level_vertex_id());
}

bool CBaseMonster::HasReachedEnemyPoint(const Fvector& Point, u32 Vertex)
{
	if (Position().distance_to_sqr(Point) > 4.f)
	{
		return false;
	}
	return HasOpenEnemyCorridor(Point, Vertex);
}

bool CBaseMonster::HasOpenEnemyCorridor(const Fvector& Point, u32 Vertex)
{
	const u32 CurrentVertex = ai_location().level_vertex_id();
	if (!ai().level_graph().valid_vertex_id(CurrentVertex) || !ai().level_graph().valid_vertex_id(Vertex))
	{
		return false;
	}
	if (std::abs(Point.y - ai().level_graph().vertex_plane_y(Vertex, Point.x, Point.z)) > 1.f ||
		ai().level_graph().check_position_in_direction(CurrentVertex, Position(), Point) != Vertex)
	{
		return false;
	}
	// AI cells may overlap coarse geometry; one cached short static ray rejects a wall inside the same cell.
	if (ArrivalGeometryCheckTime && Device.dwTimeGlobal - ArrivalGeometryCheckTime < 200 &&
		ArrivalGeometryPoint.distance_to_sqr(Point) < .01f && ArrivalGeometryStart.distance_to_sqr(Position()) < .01f)
	{
		return ArrivalGeometryClear;
	}
	ArrivalGeometryCheckTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
	ArrivalGeometryPoint = Point;
	ArrivalGeometryStart = Position();
	Fvector From = Position(), To = Point;
	From.y += .5f;
	To.y += .5f;
	Fvector Direction;
	Direction.sub(To, From);
	const float Length = Direction.magnitude();
	if (Length < EPS_L)
	{
		ArrivalGeometryClear = true;
	}
	else
	{
		Direction.div(Length);
		ArrivalGeometryClear = !Level().ObjectSpace.RayTest(From, Direction, Length, collide::rqtStatic, nullptr, this);
	}
	return ArrivalGeometryClear;
}

bool CBaseMonster::ShouldFollowLastKnownEnemy()
{
	const CEntityAlive* Target = EnemyMan.get_enemy();
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		MonsterPeaceful || EnemyTrackingLive || !Target || Target->getDestroy() || !Target->g_Alive() ||
		GetScriptControl() || EnemyMan.get_script_enemy() || HasDamagePanic() ||
		(m_controlled && m_controlled->is_under_control()) ||
		is_state(StateMan->get_state_type(), eStatePanic) || EnemyMan.see_enemy_now(Target) || ShouldUseCloseEnemyCombat())
	{
		return false;
	}
	const auto& Memory = EnemyMemory.get_memory();
	const auto Entry = Memory.find(Target);
	return Entry != Memory.end() && EnemyMemory.IsActual(Entry->second) &&
		ai().level_graph().valid_vertex_id(Entry->second.vertex) && Home->at_home(Entry->second.position) &&
		!HasReachedEnemyPoint(Entry->second.position, Entry->second.vertex);
}

void CBaseMonster::UpdateUnreachablePursuit(const CEntityAlive* Target, bool WantsMovement)
{
	PursuitSearchingAround = false;
	if (!Target || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		GetScriptControl() || EnemyMan.get_script_enemy() || MonsterPeaceful || HasDamagePanic() ||
		(m_controlled && m_controlled->is_under_control()) || control().is_captured_pure() ||
		is_state(StateMan->get_state_type(), eStatePanic))
	{
		PursuitProgressTime = 0;
		PursuitTargetID = u32(-1);
		return;
	}
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
	const Fvector Goal = EnemyMan.GetTrackingPosition();
	const u32 GoalVertex = EnemyMan.GetTrackingVertex();
	if (PursuitTargetID != Target->ID() || !PursuitProgressTime)
	{
		PursuitTargetID = Target->ID();
		PursuitProgressPosition = Position();
		PursuitProgressTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
		NextUnreachableSearchPoint = 0;
	}
	if (!EnemyMemory.IsUnreachable(Target))
	{
		if (!WantsMovement || HasReachedEnemyPoint(Goal, GoalVertex))
		{
			PursuitProgressPosition = Position();
			PursuitProgressTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
			return;
		}
		if (Position().distance_to_xz(PursuitProgressPosition) >= .75f)
		{
			PursuitProgressPosition = Position();
			PursuitProgressTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
		}
		const bool Failed = !ai().level_graph().valid_vertex_id(GoalVertex) ||
			!control().path_builder().accessible(GoalVertex) || control().path_builder().level_path().failed();
		if (!Failed && Device.dwTimeGlobal - PursuitProgressTime < EnemyStuckTime)
		{
			return;
		}
		EnemyMemory.MarkUnreachable(Target);
		UnreachableBlockedGoal = Goal;
		UnreachableSearchAnchor = Position();
		UnreachableSearchVertex = u32(-1);
		NextUnreachableSearchPoint = 0;
	}
	if (!EnemyMemory.IsUnreachable(Target))
	{
		return;
	}
	if (HasReachedEnemyPoint(Goal, GoalVertex) ||
		(EnemyMan.see_enemy_now(Target) && Goal.distance_to_sqr(UnreachableBlockedGoal) > 4.f &&
			Position().distance_to_sqr(Goal) <= 400.f && HasOpenEnemyCorridor(Goal, GoalVertex)))
	{
		EnemyMemory.ClearUnreachable(Target);
		return;
	}
	PursuitSearchingAround = true;
	const u32 CurrentVertex = ai_location().level_vertex_id();
	if (!NextUnreachableSearchPoint || s32(Device.dwTimeGlobal - NextUnreachableSearchPoint) >= 0 ||
		(UnreachableSearchVertex != u32(-1) && Position().distance_to_sqr(UnreachableSearchPoint) < .64f))
	{
		NextUnreachableSearchPoint = Device.dwTimeGlobal + u32(Random.randI(1500, 3001));
		UnreachableSearchVertex = u32(-1);
		if (ai().level_graph().valid_vertex_id(CurrentVertex))
		{
			// A bounded local AI corridor check avoids scans and global searches for orbit points.
			for (u32 Attempt = 0; Attempt < 12; ++Attempt)
			{
				Fvector ToGoal;
				ToGoal.sub(Goal, Position());
				const float Tangent = atan2f(ToGoal.z, ToGoal.x) + (ID() % 2 ? PI_DIV_2 : -PI_DIV_2);
				const float Angle = Attempt < 4 ? Tangent + Random.randF(-.6f, .6f) : Random.randF(0.f, PI_MUL_2);
				const float Radius = Random.randF(.8f, 4.f);
				Fvector Candidate = Position();
				Candidate.x += cosf(Angle) * Radius;
				Candidate.z += sinf(Angle) * Radius;
				if (Candidate.distance_to_sqr(UnreachableSearchAnchor) > 100.f || !Home->at_home(Candidate))
				{
					continue;
				}
				const u32 Node = ai().level_graph().check_position_in_direction(CurrentVertex, Position(), Candidate);
				if (!ai().level_graph().valid_vertex_id(Node) || !control().path_builder().accessible(Node) ||
					!control().path_builder().accessible(Candidate))
				{
					continue;
				}
				Candidate.y = ai().level_graph().vertex_plane_y(Node, Candidate.x, Candidate.z);
				UnreachableSearchPoint = Candidate;
				UnreachableSearchVertex = Node;
				path().prepare_builder();
				break;
			}
		}
	}
	if (UnreachableSearchVertex != u32(-1))
	{
		set_action(EnemyMan.see_enemy_now(Target) ? ACT_RUN : ACT_WALK_FWD);
		anim().clear_override_animation();
		anim().accel_deactivate();
		path().set_target_point(UnreachableSearchPoint, UnreachableSearchVertex);
		path().set_distance_to_end(.5f);
		path().set_rebuild_time(1000);
		path().set_use_covers(false);
		path().set_try_min_time(false);
		path().extrapolate_path(false);
	}
	else
	{
		// No traversable local tile: stop pushing into geometry and retry from the next sample.
		set_action(ACT_LOOK_AROUND);
	}
	set_state_sound(MonsterSound::eMonsterSoundIdle);
}

void CBaseMonster::update_fsm()
{
	PROF_EVENT("FSM");
	CMonsterLogicTimerScope FSMTimer(EMonsterLogicTimer::FSM);
	const CEntityAlive* Target = EnemyMan.get_enemy();
	const bool VisibleTarget = EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		!MonsterPeaceful && Target && !Target->getDestroy() && Target->g_Alive() && EnemyMan.see_enemy_now(Target);
	if (VisibleTarget && EnemyCloseDetectionRadius > 0.f &&
		Position().distance_to_sqr(Target->Position()) <= EnemyCloseDetectionRadius * EnemyCloseDetectionRadius)
	{
		// Nearby visual contact grants five seconds of live pursuit; sharing alone cannot grant it.
		CloseSightCombatTargetID = Target->ID();
		CloseSightCombatUntil = Device.dwTimeGlobal + 5000;
	}
	const bool DamagePanic = HasDamagePanic() && !GetScriptControl() &&
		!(m_controlled && m_controlled->is_under_control());
	if (DamagePanic)
	{
		CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
		StateMan->force_script_state(eStatePanic);
		StateMan->execute_script_state();
		DamagePanicWasActive = true;
	}
	else
	{
		if (DamagePanicWasActive)
		{
			CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
			StateMan->critical_finalize();
			DamagePanicWasActive = false;
			DamagePanicUntil = 0;
		}
		StateMan->update();
	}
	
	const bool FollowLastKnown = !DamagePanic && ShouldFollowLastKnownEnemy() && !control().is_captured_pure();
	const bool ApproachHidden = !DamagePanic && !is_state(StateMan->get_state_type(), eStatePanic) &&
		!EnemyMan.get_script_enemy() && ShouldApproachHiddenEnemy() && !control().is_captured_pure();
	const bool ChaseVisible = (VisibleTarget || HasCloseSightCombat(Target)) && !DamagePanic && !GetScriptControl() && !EnemyMan.get_script_enemy() &&
		!(m_controlled && m_controlled->is_under_control()) && !control().is_captured_pure() &&
		!is_state(StateMan->get_state_type(), eStatePanic) &&
		Position().distance_to_sqr(EnemyMan.GetTrackingPosition()) > EnemyCloseAttackDistance * EnemyCloseAttackDistance &&
		ai().level_graph().valid_vertex_id(EnemyMan.GetTrackingVertex()) && Home->at_home(EnemyMan.GetTrackingPosition()) &&
		anim().m_tAction != ACT_ATTACK;
	const bool WantsMovement = FollowLastKnown || ApproachHidden || ChaseVisible ||
		(Target && Home->at_home(EnemyMan.GetTrackingPosition()) &&
			!HasReachedEnemyPoint(EnemyMan.GetTrackingPosition(), EnemyMan.GetTrackingVertex()));
	UpdateUnreachablePursuit(Target, WantsMovement);
	if (!PursuitSearchingAround && (FollowLastKnown || ApproachHidden || ChaseVisible))
	{
		CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
		if (!LastKnownPursuitWasActive)
		{
			path().prepare_builder();
		}
		set_action(ShouldApproachHiddenEnemy() ? ACT_WALK_FWD : ACT_RUN);
		anim().clear_override_animation();
		anim().accel_deactivate();
		path().set_target_point(EnemyMan.GetTrackingPosition(), EnemyMan.GetTrackingVertex());
		path().set_rebuild_time(1000);
		path().set_distance_to_end(1.f);
		path().set_use_covers(false);
		path().set_try_min_time(false);
		path().extrapolate_path(false);
		set_state_sound(MonsterSound::eMonsterSoundAggressive);
	}

	if (!DamagePanic && !control().is_captured_pure() &&
		!(m_controlled && m_controlled->is_under_control()) && !is_state(StateMan->get_state_type(), eStatePanic))
	{
		EnemyMemory.UpdateReachedPoint(EnemyMan.get_enemy());
	}
	LastKnownPursuitWasActive = !PursuitSearchingAround && (FollowLastKnown || ApproachHidden || ChaseVisible);

	if (NoiseInvestigationUntil && (s32(NoiseInvestigationUntil - Device.dwTimeGlobal) <= 0 ||
		HasReachedEnemyPoint(NoiseInvestigationPosition, NoiseInvestigationVertex)))
	{
		NoiseInvestigationUntil = 0;
	}
	if (!DamagePanic && NoiseInvestigationUntil && !EnemyMan.get_enemy() && !GetScriptControl() &&
		!(m_controlled && m_controlled->is_under_control()) && !control().is_captured_pure())
	{
		CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
		set_action(ACT_WALK_FWD);
		anim().clear_override_animation();
		anim().accel_deactivate();
		path().set_target_point(NoiseInvestigationPosition, NoiseInvestigationVertex);
		path().set_rebuild_time(1000);
		path().set_use_covers(false);
		path().set_try_min_time(false);
		path().extrapolate_path(false);
		set_state_sound(MonsterSound::eMonsterSoundAggressive);
	}

	// завершить обработку установленных в FSM параметров
	post_fsm_update					();
	
	TranslateActionToPathParams		();

	// информировать squad о своих целях
	squad_notify					();

#ifdef DEBUG
	debug_fsm						();
#endif
}

void CBaseMonster::post_fsm_update()
{
	if (!EnemyMan.get_enemy()) return;
	
	EMonsterState state = StateMan->get_state_type();


	// Look at enemy while running
	m_bRunTurnLeft = m_bRunTurnRight = false;
	

	Fvector direction;
	if ( is_state(state, eStateAttack) && 
		 control().path_builder().is_moving_on_path() &&
		 control().path_builder().detail().try_get_direction(direction) ) {

		Fvector const self_to_enemy	=	Fvector().sub(EnemyMan.GetTrackingPosition(), Position());
		if ( magnitude(self_to_enemy) > 3.f ) {

			float	dir_yaw = direction.getH();
			float	yaw_target = self_to_enemy.getH();

			float angle_diff	= angle_difference(yaw_target, dir_yaw);

			if ((angle_diff > PI_DIV_3) && (angle_diff < 5 * PI_DIV_6)) {
				if (from_right(dir_yaw, yaw_target))	m_bRunTurnRight = true;
				else									m_bRunTurnLeft	= true;
			}
		}
	}
}

void CBaseMonster::squad_notify()
{
	CMonsterSquad	*squad = monster_squad().get_squad(this);
	SMemberGoal		goal;

	EMonsterState state = StateMan->get_state_type();

	if (is_state(state, eStateAttack)) {
		
		goal.type	= MG_AttackEnemy;
		goal.entity	= const_cast<CEntityAlive*>(EnemyMan.get_enemy());

	} else if (is_state(state, eStateRest)) {
		goal.entity	= squad->GetLeader();

		if (state == eStateRest_Idle)							goal.type	= MG_Rest;
		else if (state == eStateRest_WalkGraphPoint) 			goal.type	= MG_WalkGraph;
		else if (state == eStateRest_MoveToHomePoint) 			goal.type	= MG_WalkGraph;
		else if (state == eStateCustomMoveToRestrictor)			goal.type	= MG_WalkGraph;
		else if (state == eStateRest_WalkToCover)				goal.type	= MG_WalkGraph;
		else if (state == eStateRest_LookOpenPlace)				goal.type	= MG_Rest;
		else													goal.entity	= 0;

	} else if (is_state(state, eStateSquad)) {
		goal.type	= MG_Rest;
		goal.entity	= squad->GetLeader();
	}
	
	squad->UpdateGoal(this, goal);
}
