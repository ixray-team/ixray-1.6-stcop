////////////////////////////////////////////////////////////////////////////
//	Module 		: patrol_path_storage.cpp
//	Created 	: 15.06.2004
//  Modified 	: 15.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Patrol path storage
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "patrol_path_storage.h"
#include "patrol_path.h"
#include "patrol_point.h"

CPatrolPathStorage::~CPatrolPathStorage		()
{
	delete_data					(m_registry);
}

void CPatrolPathStorage::load_editor(const ILevelGraph* level_graph, const IGameLevelCrossTable* cross, const IGameGraph* game_graph)
{
	for (auto& Obj : Scene->ListObj(OBJCLASS_WAY))
	{
		CWayObject* Way = dynamic_cast<CWayObject*>(Obj);
		shared_str	patrol_name =Way->GetName();
		const_iterator			I = m_registry.find(patrol_name);
		VERIFY3(I == m_registry.end(), "Duplicated patrol path found", *patrol_name);
		m_registry.insert(
			std::make_pair(
				patrol_name,
				&xr_new<CPatrolPath>(
					patrol_name
					)->load_editor(
						level_graph,
						cross,
						game_graph,
						Way
					)
			)
		);
	}

	/*
	
	  F->patrolpath.stream.open_chunk	(WAYOBJECT_CHUNK_VERSION);
        F->patrolpath.stream.w_u16		(WAYOBJECT_VERSION);
        F->patrolpath.stream.close_chunk	();

        F->patrolpath.stream.open_chunk	(WAYOBJECT_CHUNK_NAME);
        F->patrolpath.stream.w_stringZ	(GetName());
        F->patrolpath.stream.close_chunk	();

        int l_cnt		= 0;
        F->patrolpath.stream.open_chunk	(WAYOBJECT_CHUNK_POINTS);
        F->patrolpath.stream.w_u16		((u16)m_WayPoints.size());
        for (WPIt it=m_WayPoints.begin(); it!=m_WayPoints.end(); it++){
            CWayPoint* W = *it;
            F->patrolpath.stream.w_fvector3	(W->m_vPosition);
            F->patrolpath.stream.w_u32		(W->m_Flags.get());
	        F->patrolpath.stream.w_stringZ	(*W->m_Name?*W->m_Name:"");
            l_cnt		+= W->m_Links.size();
        }
        F->patrolpath.stream.close_chunk	();

        F->patrolpath.stream.open_chunk	(WAYOBJECT_CHUNK_LINKS);
        F->patrolpath.stream.w_u16		((u16)l_cnt);
        for (auto it=m_WayPoints.begin(); it!=m_WayPoints.end(); it++){
            CWayPoint* W= *it;
            int from	= it-m_WayPoints.begin();
            for (WPLIt l_it=W->m_Links.begin(); l_it!=W->m_Links.end(); l_it++){
                WPIt to= std::find(m_WayPoints.begin(),m_WayPoints.end(),(*l_it)->way_point); R_ASSERT(to!=m_WayPoints.end());
                F->patrolpath.stream.w_u16	((u16)from);
                F->patrolpath.stream.w_u16	((u16)(to-m_WayPoints.begin()));
	            F->patrolpath.stream.w_float	((*l_it)->probability);
            }
        }
        F->patrolpath.stream.close_chunk	();
	*/

}

void CPatrolPathStorage::load_raw			(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream)
{
	IReader						*chunk = stream.open_chunk(WAY_PATROLPATH_CHUNK);

	if (!chunk)
		return;
		
	u32							chunk_iterator;
	
	
	chunk->close				();
}

void CPatrolPathStorage::load				(IReader &stream)
{
	IReader						*chunk;

	chunk						= stream.open_chunk(0);
	u32							size = chunk->r_u32();
	chunk->close				();

	m_registry.clear			();

	PATROL_REGISTRY::value_type	pair;

	chunk						= stream.open_chunk(1);
	for (u32 i=0; i<size; ++i) {
		IReader					*chunk1;
		chunk1					= chunk->open_chunk(i);

		IReader					*chunk2;
		chunk2					= chunk1->open_chunk(0);
        load_data				(pair.first,*chunk2);
		chunk2->close			();

		chunk2					= chunk1->open_chunk(1);
        load_data				(pair.second,*chunk2);
		chunk2->close			();

		chunk1->close			();

		const_iterator			I = m_registry.find(pair.first);
		VERIFY3					(I == m_registry.end(),"Duplicated patrol path found ",*pair.first);
		
#ifdef DEBUG
		pair.second->name		(pair.first);
#endif

		m_registry.insert		(pair);
	}

	chunk->close				();
}

void CPatrolPathStorage::save				(IWriter &stream)
{
	stream.open_chunk			(0);
	stream.w_u32				(m_registry.size());
	stream.close_chunk			();

	stream.open_chunk			(1);

	PATROL_REGISTRY::iterator	I = m_registry.begin();
	PATROL_REGISTRY::iterator	E = m_registry.end();
	for (int i=0; I != E; ++I, ++i) {
		stream.open_chunk		(i);

		stream.open_chunk		(0);
        save_data				((*I).first,stream);
		stream.close_chunk		();

		stream.open_chunk		(1);
        save_data				((*I).second,stream);
		stream.close_chunk		();

		stream.close_chunk		();
	}

	stream.close_chunk			();
}
