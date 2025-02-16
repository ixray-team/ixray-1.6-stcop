#include "stdafx.h"
#include "dynamic_patrol_path.h"
#include "dynamic_patrol_point.h"
#include "levelgamedef.h"


CDynamicPatrolPath::CDynamicPatrolPath(shared_str name) : inherited(name), m_vertices_count(0), m_name(name)
{
}

CDynamicPatrolPath::CDynamicPatrolPath(CDynamicPatrolPath *old)
{
	m_vertices_count = old->m_vertices_count;
	m_dvertices = old->m_dvertices;
	m_dedges = old->m_dedges;
	m_name = old->m_name;
}

CDynamicPatrolPath::~CDynamicPatrolPath()
{
	
}

CPatrolPath	&CDynamicPatrolPath::load_raw(const CLevelGraph *level_graph, const CGameLevelCrossTable *cross,
												const CGameGraph *game_graph, IReader &stream)
{
	R_ASSERT(m_dvertices.size());
	
	u32 i = 0;
	for (POINTS_STORAGE::iterator it = m_dvertices.begin(), last = m_dvertices.end(); it != last; ++it)
	{
		add_vertex(CDynamicPatrolPoint(this, (*it).first).load_raw(level_graph, cross, game_graph, stream), i);
		i++;
	}
	for (LINKS_STORAGE::iterator it = m_dedges.begin(), last = m_dedges.end(); it != last; ++it)
	{
		u16 vertex0 = it->first;
		u16 vertex1 = it->second;
		add_edge(vertex0, vertex1, 1.0f);
	}
	
	
	return			(*this);
}

