#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/CELBaseAdapter.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai/monsters/state_manager.h"
#include "xrGame/ai/monsters/controlled_entity.h"
#include "xrGame/ai/monsters/monster_home.h"
#include "xrGame/ai/monsters/control_path_builder_base.h"
#include "xrGame/ai/monsters/control_animation_base.h"
#include "xrGame/ai_monster_space.h"
#include "xrGame/ai_space.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/level_graph.h"
#include "xrGame/ai/monsters/control_path_builder.h"
#include "xrGame/movement_manager.h"
#include "xrGame/detail_path_manager.h"

// Связывает доступ расширения к штатным операциям монстра с необходимыми службами и состоянием владельца.
CELBaseAdapter::CELBaseAdapter(CBaseMonster& Monster) : OwnerMonster(Monster) {}
// Возвращает штатного монстра, которым владеет адаптер.
CBaseMonster& CELBaseAdapter::Monster() const
{
	return OwnerMonster;
}

// Разрешает перехват управления только вне конфликтующих штатных и скриптовых действий.
bool CELBaseAdapter::CanControl() const
{
	// Species-specific attacks must finish in their native FSM, even between captures.
	if (OwnerMonster.StateMan)
	{
		const EMonsterState State = OwnerMonster.StateMan->get_state_type();
		if (State != eStateUnknown && (is_state(State, eStateCustom) || State == eStateAttack_Psy || State == eStateAttack_ControlFire || State == eStateAttack_ControlTube))
		{
			return false;
		}
	}
	const EMotionAnim Override = OwnerMonster.anim().get_override_animation();
	if (OwnerMonster.UsesExtendedCombat && (Override == eAnimAttackOnRunLeft || Override == eAnimAttackOnRunRight))
	{
		// Preserve the running attack through its impact event, just like a stationary melee animation.
		return false;
	}
	if (Override != eAnimUndefined && Override != eAnimAttackOnRunLeft && Override != eAnimAttackOnRunRight && Override != eAnimSniff)
	{
		return false;
	}
	return !OwnerMonster.is_jumping() && OwnerMonster.anim().GetCurAnim() != eAnimAttack && OwnerMonster.g_Alive() && !OwnerMonster.getDestroy() && !OwnerMonster.GetScriptControl() &&
		   !OwnerMonster.EnemyMan.get_script_enemy() && !OwnerMonster.m_script_processing_active &&
		   !(OwnerMonster.m_controlled && OwnerMonster.m_controlled->is_under_control()) &&
		   !OwnerMonster.control().is_captured_pure() && OwnerMonster.anim().m_tAction != ACT_ATTACK;
}

// Проверяет, является ли живая доступная сущность противником владельца.
bool CELBaseAdapter::IsEnemy(const CEntityAlive& Candidate) const
{
	return !ExtendedEntityes::IsIgnored(Candidate) && Candidate.g_Alive() && !Candidate.getDestroy() && OwnerMonster.EnemyMan.is_enemy(&Candidate);
}

// Проверяет подтверждённую штатным зрением видимость цели.
bool CELBaseAdapter::CanSee(const CEntityAlive& Candidate) const
{
	return OwnerMonster.EnemyMan.see_enemy_now(&Candidate) || CELSharedSight::HasEvasionTarget(OwnerMonster, Candidate);
}

// Проверяет допустимость назначения с учётом ограничений категории.
bool CELBaseAdapter::CanReach(const Fvector& TargetPosition, u32 TargetVertex) const
{
	if (HasFieldEscapePermission)
	{
		Fvector Source;
		u32 Until;
		const float Current = CBaseMonster::PanicZonePressure(OwnerMonster.Position(), Source, Until, OwnerMonster.EmmiPainProtection);
		const float Next = CBaseMonster::PanicZonePressure(TargetPosition, Source, Until, OwnerMonster.EmmiPainProtection);
		if ((Next > 0.f && Next >= Current) || !CBaseMonster::IsPanicRouteSafe(OwnerMonster.Position(), TargetPosition, OwnerMonster.EmmiPainProtection))
		{
			return false;
		}
	}
	return ai().get_level_graph() && ai().level_graph().valid_vertex_id(TargetVertex) &&
		   (!CELSharedAnomalies::Contains(OwnerMonster.ID(), TargetPosition) ||
			(CELSharedAnomalies::Contains(OwnerMonster.ID(), OwnerMonster.Position()) && !CELSharedAnomalies::Crosses(OwnerMonster.ID(), OwnerMonster.Position(), TargetPosition))) &&
		   (!CELSharedDeaths::Contains(OwnerMonster.ID(), TargetPosition) ||
			(CELSharedDeaths::Contains(OwnerMonster.ID(), OwnerMonster.Position()) && !CELSharedDeaths::Crosses(OwnerMonster.ID(), OwnerMonster.Position(), TargetPosition))) &&
		   (HasFieldEscapePermission || (OwnerMonster.Home && OwnerMonster.Home->at_home(TargetPosition))) &&
		   OwnerMonster.control().path_builder().accessible(TargetVertex) && OwnerMonster.control().path_builder().accessible(TargetPosition);
}

// Разрешает выбор цели; ограничения способа преследования задаёт адаптер категории.
bool CELBaseAdapter::CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const
{
	return true;
}

// Finalize the previous native action before issuing the first extended movement command.
// Do not restart the FSM on every waypoint or when switching extended movement schemes.
void CELBaseAdapter::BeginMovementControl()
{
	if (HasMovementControl)
	{
		return;
	}
	if (OwnerMonster.StateMan)
	{
		OwnerMonster.StateMan->critical_finalize();
	}
	OwnerMonster.m_bRunTurnLeft = OwnerMonster.m_bRunTurnRight = false;
	HasMovementControl = true;
}

// Native control may already be starting a jump: do not overwrite its new commands here.
void CELBaseAdapter::EndMovementControl()
{
	HasMovementControl = false;
}

// Снимает команды расширения и возвращает штатное управление.
void CELBaseAdapter::ReleaseControl()
{
	EndMovementControl();
	HasFieldEscapePermission = false;
	OwnerMonster.movement().detail().SetMovementFactor(1.f);
	OwnerMonster.path().enable_path();
	OwnerMonster.path().set_generic_parameters();
	if (OwnerMonster.StateMan)
	{
		OwnerMonster.StateMan->critical_finalize();
	}
}
