#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/CELBaseAdapter.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/state_manager.h"
#include "ai/monsters/controlled_entity.h"
#include "ai/monsters/monster_home.h"
#include "ai/monsters/control_path_builder_base.h"
#include "ai/monsters/control_animation_base.h"
#include "ai_monster_space.h"
#include "ai_space.h"
#include "EntryPoint.h"
#include "level_graph.h"
#include "ai/monsters/control_path_builder.h"

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
	return OwnerMonster.g_Alive() && !OwnerMonster.getDestroy() && !OwnerMonster.GetScriptControl() &&
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
	return OwnerMonster.EnemyMan.see_enemy_now(&Candidate);
}

// Проверяет допустимость назначения с учётом ограничений категории.
bool CELBaseAdapter::CanReach(const Fvector& TargetPosition, u32 TargetVertex) const
{
	return ai().get_level_graph() && ai().level_graph().valid_vertex_id(TargetVertex) &&
		   (!CELSharedAnomalies::Contains(OwnerMonster.ID(), TargetPosition) ||
			(CELSharedAnomalies::Contains(OwnerMonster.ID(), OwnerMonster.Position()) && !CELSharedAnomalies::Crosses(OwnerMonster.ID(), OwnerMonster.Position(), TargetPosition))) &&
		   (!CELSharedDeaths::Contains(OwnerMonster.ID(), TargetPosition) ||
			(CELSharedDeaths::Contains(OwnerMonster.ID(), OwnerMonster.Position()) && !CELSharedDeaths::Crosses(OwnerMonster.ID(), OwnerMonster.Position(), TargetPosition))) &&
		   OwnerMonster.Home && OwnerMonster.Home->at_home(TargetPosition) &&
		   OwnerMonster.control().path_builder().accessible(TargetVertex) && OwnerMonster.control().path_builder().accessible(TargetPosition);
}

// Разрешает выбор цели; ограничения способа преследования задаёт адаптер категории.
bool CELBaseAdapter::CanPursue(CEntityAlive& Candidate, const Fvector& KnownPosition) const
{
	return true;
}

// Снимает команды расширения и возвращает штатное управление.
void CELBaseAdapter::ReleaseControl()
{
	OwnerMonster.path().enable_path();
	OwnerMonster.path().set_generic_parameters();
	if (OwnerMonster.StateMan)
	{
		OwnerMonster.StateMan->critical_finalize();
	}
}
