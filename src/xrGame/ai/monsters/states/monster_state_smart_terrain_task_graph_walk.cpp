#include "StdAfx.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_object_registry.h"
#include "alife_monster_brain.h"
#include "ai_object_location.h"

#include "monster_state_smart_terrain_task_graph_walk.h"


void CStateMonsterSmartTerrainTaskGraphWalk::initialize()
{
	inherited::initialize();

	CSE_ALifeMonsterAbstract* monster = smart_cast<CSE_ALifeMonsterAbstract*>(ai().alife().objects().object(object->ID()));
	VERIFY(monster);
	VERIFY(monster->m_smart_terrain_id != 0xffff);

	// get task
	m_task = monster->brain().smart_terrain().task(monster);
	VERIFY(m_task);
}

bool CStateMonsterSmartTerrainTaskGraphWalk::check_start_conditions()
{
	CSE_ALifeMonsterAbstract* monster = smart_cast<CSE_ALifeMonsterAbstract*>(ai().alife().objects().object(object->ID()));
	VERIFY(monster);

	if (monster->m_smart_terrain_id == 0xffff) return false;

	m_task = monster->brain().smart_terrain().task(monster);
	VERIFY3(m_task, "Smart terrain selected, but task was not set for monster ", *object->cName());
	if (object->ai_location().game_vertex_id() == m_task->game_vertex_id()) return false;

	return							true;
}

bool CStateMonsterSmartTerrainTaskGraphWalk::check_completion()
{
	// if we get to the graph point - work complete
	if (object->ai_location().game_vertex_id() == m_task->game_vertex_id()) return true;
	return false;
}

void CStateMonsterSmartTerrainTaskGraphWalk::execute()
{
	object->set_action(ACT_WALK_FWD);
	object->set_state_sound(MonsterSound::eMonsterSoundIdle);

	object->path().detour_graph_points(m_task->game_vertex_id());
}
