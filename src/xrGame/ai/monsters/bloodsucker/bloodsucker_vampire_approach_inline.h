#pragma once

CStateBloodsuckerVampireApproach::CStateBloodsuckerVampireApproach(CAI_Bloodsucker*obj) : inherited(obj)
{
}

CStateBloodsuckerVampireApproach::~CStateBloodsuckerVampireApproach()
{
}

void CStateBloodsuckerVampireApproach::initialize()
{
	inherited::initialize();
	this->object->path().prepare_builder	();	
}

void CStateBloodsuckerVampireApproach::execute()
{
	// установка параметров функциональных блоков
	this->object->set_action								(ACT_RUN);
	this->object->anim().accel_activate					(eAT_Aggressive);
	this->object->anim().accel_set_braking				(false);

	u32 const target_vertex		=	this->object->EnemyMan.get_enemy()->ai_location().level_vertex_id();
	Fvector const target_pos	=	ai().level_graph().vertex_position(target_vertex);

	this->object->path().set_target_point					(target_pos, target_vertex);
	this->object->path().set_rebuild_time					(this->object->get_attack_rebuild_time());
	this->object->path().set_use_covers					(false);
	this->object->path().set_distance_to_end				(0.1f);
	this->object->set_state_sound							(MonsterSound::eMonsterSoundAggressive);
}

