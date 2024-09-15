#pragma once

CStateMonsterRestWalkGraph::CStateMonsterRestWalkGraph(CBaseMonster*obj) : inherited(obj)
{
}

CStateMonsterRestWalkGraph::~CStateMonsterRestWalkGraph	()
{
}

void CStateMonsterRestWalkGraph::execute()
{
	this->object->path().detour_graph_points	();
	this->object->set_action					(ACT_WALK_FWD);
	this->object->set_state_sound				(MonsterSound::eMonsterSoundIdle);
}

