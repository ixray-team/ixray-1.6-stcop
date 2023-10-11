#pragma once

#include "../../../ai_space.h"
#include "../monster_cover_manager.h"
#include "../../../cover_point.h"

#define TEMPLATE_SPECIALIZATION template <\
	typename _Object\
>
#define CStatePsyDogHideAbstract CStatePsyDogHide<_Object>

TEMPLATE_SPECIALIZATION
void CStatePsyDogHideAbstract::initialize()
{
	inherited::initialize();

	select_target_point();
	this->object->path().prepare_builder();

}

TEMPLATE_SPECIALIZATION
void CStatePsyDogHideAbstract::execute()
{
	this->object->set_action					(ACT_RUN);
	this->object->path().set_target_point		(target.position, target.node);
	this->object->path().set_rebuild_time		(0);
	this->object->path().set_distance_to_end	(0);
	this->object->path().set_use_covers		(false);

	this->object->anim().accel_activate		(eAT_Aggressive);
	this->object->anim().accel_set_braking	(false);

	this->object->sound().play				(MonsterSound::eMonsterSoundAggressive, 0,0,this->object->db().m_dwAttackSndDelay);
}

TEMPLATE_SPECIALIZATION
bool CStatePsyDogHideAbstract::check_start_conditions()
{
	return true;
}

TEMPLATE_SPECIALIZATION
bool CStatePsyDogHideAbstract::check_completion()
{
	return ((this->object->ai_location().level_vertex_id() == target.node) && !this->object->control().path_builder().is_moving_on_path());
}

TEMPLATE_SPECIALIZATION
void CStatePsyDogHideAbstract::select_target_point()
{
	const CCoverPoint	*point = this->object->CoverMan->find_cover(this->object->EnemyMan.get_enemy_position(),10.f,30.f);
	if (point && (this->object->Position().distance_to(point->position()) > 2.f)) {
		target.node					= point->level_vertex_id	();
		target.position				= point->position			();
	} else {
		const CCoverPoint	*point = this->object->CoverMan->find_cover(this->object->Position(),10.f,30.f);
		if (point && (this->object->Position().distance_to(point->position()) > 2.f)) {
			target.node					= point->level_vertex_id	();
			target.position				= point->position			();
		} else {
			target.node					= 0;
			target.position				= ai().level_graph().vertex_position(target.node);
		}
	}
}

#undef TEMPLATE_SPECIALIZATION
#undef CStatePsyDogHideAbstract
