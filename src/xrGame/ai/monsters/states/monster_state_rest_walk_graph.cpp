#include "StdAfx.h"
#include "monster_state_rest_walk_graph.h"


CStateMonsterRestWalkGraph::CStateMonsterRestWalkGraph(CBaseMonster* obj) : inherited(obj)
{
}

CStateMonsterRestWalkGraph::~CStateMonsterRestWalkGraph()
{
}

void CStateMonsterRestWalkGraph::execute()
{
	object->path().detour_graph_points();
	object->set_action(ACT_WALK_FWD);
	object->set_state_sound(MonsterSound::eMonsterSoundIdle);
}

