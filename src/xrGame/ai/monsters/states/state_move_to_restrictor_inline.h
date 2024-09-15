#pragma once


void CStateMonsterMoveToRestrictor::initialize()
{
	inherited::initialize();
	this->object->path().prepare_builder();

	Fvector position;
	u32		node = this->object->control().path_builder().restrictions().accessible_nearest(this->object->Position(), position);
	this->object->path().set_target_point	(ai().level_graph().vertex_position(node), node);
}


void CStateMonsterMoveToRestrictor::execute()
{
	this->object->set_action					(ACT_RUN);
	
	this->object->anim().accel_activate		(EAccelType(eAT_Aggressive));
	this->object->anim().accel_set_braking	(true);
	this->object->set_state_sound			(MonsterSound::eMonsterSoundIdle);
}


bool CStateMonsterMoveToRestrictor::check_start_conditions()
{
	return (!this->object->control().path_builder().accessible(this->object->Position()));
}


bool CStateMonsterMoveToRestrictor::check_completion()
{
	return (this->object->control().path_builder().accessible(this->object->Position()));
}
