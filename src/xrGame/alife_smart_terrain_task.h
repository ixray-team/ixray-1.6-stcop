////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_smart_terrain_task.h
//	Created 	: 20.09.2005
//  Modified 	: 20.09.2005
//	Author		: Dmitriy Iassenev
//	Description : ALife smart terrain task
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "game_graph_space.h"

class CPatrolPoint;

XREFLECT(LuaExport)
class CALifeSmartTerrainTask
{
private:
#ifdef DEBUG
	shared_str m_patrol_path_name;
	u32 m_patrol_point_index;
#endif
	const CPatrolPoint* m_patrol_point;
	GameGraph::_GRAPH_ID m_game_vertex_id;
	u32 m_level_vertex_id;

private:
#ifdef DEBUG
	IC const shared_str& patrol_path_name() const;
	IC const u32& patrol_point_index() const;
#endif

	void setup_patrol_point(const shared_str& patrol_path_name, const u32& patrol_point_index);
	IC const CPatrolPoint& patrol_point() const;
	IC void init(const shared_str& patrol_path_name, const u32& patrol_point_index);

public:
	CALifeSmartTerrainTask(const shared_str& patrol_path_name);
	CALifeSmartTerrainTask(const shared_str& patrol_path_name, const u32& patrol_point_index = 0);

	XPROPERTY(LuaConstructor)
	CALifeSmartTerrainTask(const char* patrol_path_name);

	XPROPERTY(LuaConstructor)
	CALifeSmartTerrainTask(const char* patrol_path_name, const u32& patrol_point_index);

	XPROPERTY(LuaConstructor)
	CALifeSmartTerrainTask(const GameGraph::_GRAPH_ID& game_vertex_id, const u32& level_vertex_id);

	XPROPERTY(LuaCallable)
	GameGraph::_GRAPH_ID game_vertex_id() const;

	XPROPERTY(LuaCallable)
	u32 level_vertex_id() const;

	XPROPERTY(LuaCallable)
	Fvector position() const;
};

#include "alife_smart_terrain_task_inline.h"