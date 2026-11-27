////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_smart_terrain_task.cpp
//	Created 	: 20.09.2005
//  Modified 	: 20.09.2005
//	Author		: Dmitriy Iassenev
//	Description : ALife smart terrain task
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "alife_smart_terrain_task.h"
#include "ai_space.h"
#include "patrol_path_storage.h"
#include "patrol_path.h"
#include "patrol_point.h"

#include "level_graph.h"
#include "game_graph.h"

void CALifeSmartTerrainTask::setup_patrol_point				(const shared_str &patrol_path_name, const u32 &patrol_point_index) const
{
	VERIFY					(!m_patrol_point);
	m_patrol_path_name = patrol_path_name;
	m_patrol_point_index = patrol_point_index;
	m_patrol_storage = &ai().patrol_paths_raw();
	m_patrol_revision = m_patrol_storage->Revision();
	m_patrol_path = m_patrol_storage->shared_path(patrol_path_name, true);
	if (m_patrol_path == nullptr)
		return;

	const CPatrolPath::CVertex* point = m_patrol_path->vertex(patrol_point_index);
	if (point == nullptr)
	{
		Msg("! CALifeSmartTerrainTask: point %u is unavailable in patrol path '%s'", patrol_point_index, patrol_path_name.c_str());
		m_patrol_path.reset();
		return;
	}
	m_patrol_point = &point->data();
}

void CALifeSmartTerrainTask::RefreshPatrolPoint() const
{
	if (m_patrol_path_name.size() == 0)
		return;
	const CPatrolPathStorage* Storage = &ai().patrol_paths_raw();
	if (m_patrol_storage == Storage && m_patrol_revision == Storage->Revision())
		return;

	m_patrol_point = nullptr;
	m_patrol_path.reset();
	m_patrol_storage = nullptr;
	setup_patrol_point(m_patrol_path_name, m_patrol_point_index);
}

GameGraph::_GRAPH_ID CALifeSmartTerrainTask::game_vertex_id		() const
{
	RefreshPatrolPoint();
	if (m_patrol_point == nullptr)
		return m_game_vertex_id;
	if (ai().get_game_graph() == nullptr || ai().get_cross_table() == nullptr)
		return m_game_vertex_id;
	if (m_game_vertex_id == GameGraph::_GRAPH_ID(-1))	{
		VERIFY3					(ai().game_graph().valid_vertex_id(patrol_point().game_vertex_id()),*m_patrol_path_name,*m_patrol_point->name());
		return					(patrol_point().game_vertex_id());
	}
	else {
		VERIFY(ai().game_graph().valid_vertex_id(m_game_vertex_id));
		return m_game_vertex_id;
	}
}

u32	CALifeSmartTerrainTask::level_vertex_id						() const
{
	RefreshPatrolPoint();
	if (m_patrol_point == nullptr)
		return m_level_vertex_id;
	if (ai().get_game_graph() == nullptr || ai().get_cross_table() == nullptr)
		return m_level_vertex_id;
	if (m_level_vertex_id == u32(-1))	{
		VERIFY3					(ai().game_graph().valid_vertex_id(patrol_point().game_vertex_id()),*m_patrol_path_name,*m_patrol_point->name());
		return					(patrol_point().level_vertex_id());
	}
	else {

		VERIFY2(ai().game_graph().valid_vertex_id(m_game_vertex_id), make_string<const char*>("Vertex [%d] is not valid!!!", m_game_vertex_id));
		return m_level_vertex_id;
	}
}

Fvector CALifeSmartTerrainTask::position() const
{
	RefreshPatrolPoint();
	if (m_level_vertex_id == u32(-1))
	{
		if (m_patrol_point != nullptr)
			return patrol_point().position();
	}
	else if (ai().level_graph().valid_vertex_id(m_level_vertex_id) &&
		(!ai().game_graph().valid_vertex_id(m_game_vertex_id) || ai().game_graph().vertex(m_game_vertex_id)->level_id() == ai().level_graph().level_id()))
	{
		return ai().level_graph().vertex_position(m_level_vertex_id);
	}

	if (ai().game_graph().valid_vertex_id(m_game_vertex_id))
		return ai().game_graph().vertex(m_game_vertex_id)->level_point();
	if (m_patrol_point != nullptr)
		return patrol_point().position();
	return Fvector().set(0.f, 0.f, 0.f);
}
