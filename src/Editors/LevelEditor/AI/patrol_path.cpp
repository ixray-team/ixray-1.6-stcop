////////////////////////////////////////////////////////////////////////////
//	Module 		: patrol_path.cpp
//	Created 	: 15.06.2004
//  Modified 	: 15.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Patrol path
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "patrol_path.h"

LPCSTR TEST_PATROL_PATH_NAME		= "val_dogs_nest4_centre";

CPatrolPath::CPatrolPath			(shared_str name)
{
#ifdef DEBUG
	m_name			= name;
#endif
}

CPatrolPath	&CPatrolPath::load_raw	(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream)
{
	R_ASSERT		(stream.find_chunk(WAYOBJECT_CHUNK_POINTS));
	u32				vertex_count = stream.r_u16();
	for (u32 i=0; i<vertex_count; ++i)
		add_vertex	(CPatrolPoint(this).load_raw(level_graph,cross,game_graph,stream),i);

	R_ASSERT		(stream.find_chunk(WAYOBJECT_CHUNK_LINKS));
	u32				edge_count = stream.r_u16();
	for (u32 i=0; i < edge_count; ++i) {
		u16			vertex0 = stream.r_u16();
		u16			vertex1 = stream.r_u16();
		float		probability = stream.r_float();
		add_edge	(vertex0,vertex1,probability);
	}

	return			(*this);
}

CPatrolPath::~CPatrolPath	()
{
}

CPatrolPath& CPatrolPath::load_editor(const ILevelGraph* level_graph, const IGameLevelCrossTable* cross, const IGameGraph* game_graph, CWayObject* object)
{
	u32				vertex_count = object->m_WayPoints.size();
	int l_cnt = 0;
	for (u32 i = 0; i < vertex_count; ++i)
	{
		add_vertex(CPatrolPoint(this).load_editor(level_graph, cross, game_graph, object, i), i);
	}

	for (auto it = object->m_WayPoints.begin(); it != object->m_WayPoints.end(); it++) {
		CWayPoint* W = *it;
		int from = it - object->m_WayPoints.begin();
		for (WPLIt l_it = W->m_Links.begin(); l_it != W->m_Links.end(); l_it++) {
			WPIt to = std::find(object->m_WayPoints.begin(), object->m_WayPoints.end(), (*l_it)->way_point); R_ASSERT(to != object->m_WayPoints.end());
			u16			vertex0 = (u16)from;
			u16			vertex1 = (u16)(to - object->m_WayPoints.begin());
			float		probability = (*l_it)->probability;
			add_edge(vertex0, vertex1, probability);
		}
	}
	return			(*this);
}

#ifdef DEBUG
void CPatrolPath::load		(IReader &stream)
{
	inherited::load	(stream);
	vertex_iterator	I = vertices().begin();
	vertex_iterator	E = vertices().end();
	for ( ; I != E; ++I)
		(*I).second->data().path	(this);
}
#endif
