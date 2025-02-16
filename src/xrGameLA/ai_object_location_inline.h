////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_object_location.h
//	Created 	: 27.11.2003
//  Modified 	: 27.11.2003
//	Author		: Dmitriy Iassenev
//	Description : AI object location
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "ai_space.h"
#include "level_graph.h"
#include "game_graph.h"

IC	CAI_ObjectLocation::CAI_ObjectLocation()
{
	init();
}

IC	void CAI_ObjectLocation::reinit()
{
	init();
}

IC	const GameGraph::_GRAPH_ID CAI_ObjectLocation::game_vertex_id() const
{
	return					(m_game_vertex_id);
}

IC	const u32 CAI_ObjectLocation::level_vertex_id() const
{
	return					(m_level_vertex_id);
}

IC	void CAI_ObjectLocation::game_vertex(_GRAPH_ID const& game_vertex_id) {
	VERIFY(ai().game_graph().valid_vertex_id(game_vertex_id));
	m_game_vertex_id = game_vertex_id;
}

IC	void CAI_ObjectLocation::level_vertex(u32 const& level_vertex_id) {
	VERIFY(ai().level_graph().valid_vertex_id(level_vertex_id));
	m_level_vertex_id = level_vertex_id;
}
