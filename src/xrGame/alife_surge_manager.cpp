////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_surge_manager.cpp
//	Created 	: 25.12.2002
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator surge manager
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "alife_surge_manager.h"
#include "alife_object_registry.h"
#include "alife_spawn_registry.h"
#include "alife_time_manager.h"
#include "alife_graph_registry.h"
#include "alife_schedule_registry.h"
#include "alife_simulator_header.h"
#include "ai_space.h"
#include "ef_storage.h"
#include "ef_pattern.h"
#include "graph_engine.h"
#include "xrserver.h"
#include "alife_human_brain.h"

using namespace ALife;

CALifeSurgeManager::~CALifeSurgeManager	()
{
}

void CALifeSurgeManager::spawn_new_spawns			()
{
	// FX: Dirty hack
	for (auto Iter = m_temp_spawns.begin(); Iter < m_temp_spawns.end(); Iter++) {
		auto& AbstarctObject = spawns().spawns().vertex(*Iter)->data()->object();
		if (smart_cast<CSE_ALifeCreatureActor*>(&AbstarctObject)) {
			u16 ID = *Iter;
			m_temp_spawns.erase(Iter);
			m_temp_spawns.emplace(m_temp_spawns.begin(), ID);
			break;
		}
	}

	for (u16 ObjectID : m_temp_spawns) {
		CSE_ALifeDynamicObject* object = nullptr;
		auto& AbstarctObject = spawns().spawns().vertex(ObjectID)->data()->object();
		CSE_ALifeDynamicObject* spawn = smart_cast<CSE_ALifeDynamicObject*>(&AbstarctObject);

		create(object, spawn, ObjectID);
	}
}

void CALifeSurgeManager::fill_spawned_objects		()
{
	m_temp_spawned_objects.clear	();

	D_OBJECT_P_MAP::const_iterator	I = objects().objects().begin();
	D_OBJECT_P_MAP::const_iterator	E = objects().objects().end();
	for ( ; I != E; ++I)
		if (spawns().spawns().vertex((*I).second->m_tSpawnID))
			m_temp_spawned_objects.push_back	((*I).second->m_tSpawnID);
}

void CALifeSurgeManager::spawn_new_objects			()
{
	fill_spawned_objects			();
	spawns().fill_new_spawns		(m_temp_spawns,time_manager().game_time(),m_temp_spawned_objects);
	spawn_new_spawns				();
	VERIFY							(graph().actor());
}
