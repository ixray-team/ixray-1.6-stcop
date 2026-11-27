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
class CPatrolPath;
class CPatrolPathStorage;

XREFLECT(LuaExport)
class CALifeSmartTerrainTask
{
private:
	mutable shared_str m_patrol_path_name;
	mutable u32 m_patrol_point_index = 0;
	mutable xr_shared_ptr<const CPatrolPath> m_patrol_path;
	mutable const CPatrolPathStorage* m_patrol_storage = nullptr;
	mutable u32 m_patrol_revision = 0;
	mutable const CPatrolPoint* m_patrol_point = nullptr;
	GameGraph::_GRAPH_ID m_game_vertex_id;
	u32 m_level_vertex_id;

private:
#ifdef DEBUG
	IC const shared_str& patrol_path_name() const;
	IC const u32& patrol_point_index() const;
#endif

	void setup_patrol_point(const shared_str& patrol_path_name, const u32& patrol_point_index) const;
	void RefreshPatrolPoint() const;
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