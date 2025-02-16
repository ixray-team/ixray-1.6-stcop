#include "stdafx.h"
#include "dynamic_patrol_point.h"
#include "patrol_point.h"
#include "level_graph.h"
#include "level_graph.h"
#include "game_level_cross_table.h"
#include "game_graph.h"
#include "../xrCore/object_broker.h"

CDynamicPatrolPoint::CDynamicPatrolPoint(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, const CPatrolPath *path, const Fvector &position, u32 level_vertex_id, u32 flags, shared_str name) :
	inherited(level_graph, cross, game_graph, path, position, level_vertex_id, flags, name)
{
}


CDynamicPatrolPoint::CDynamicPatrolPoint(const CPatrolPath *path, CDynamicPatrolPoint *old) : inherited(path)
{
	if (old != nullptr)
	{
#ifdef DEBUG
		old->initialized(true);
#endif
		set_name						(*old->name());
		set_position					(old->position());
		set_level_vertex_id				(old->level_vertex_id());
		set_game_vertex_id				(old->game_vertex_id());
		set_flags						(old->flags());
	}
}

CDynamicPatrolPoint::~CDynamicPatrolPoint()
{
}

CPatrolPoint &CDynamicPatrolPoint::load_raw(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream)
{
	if (!(level_graph && level_graph->valid_vertex_position(m_position)))
		m_level_vertex_id	= u32(-1);

#ifdef DEBUG
	m_initialized		= true;
#endif

	correct_position	(level_graph, cross, game_graph);
	return				(*this);
}
