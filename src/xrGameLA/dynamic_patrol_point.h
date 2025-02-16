#pragma once
#ifndef __DYNAMIC_PATROL_POINT_H__
#define __DYNAMIC_PATROL_POINT_H__

#include "patrol_point.h"

class CDynamicPatrolPoint : public CPatrolPoint
{
	private:
		typedef CPatrolPoint inherited;
	public:
										CDynamicPatrolPoint		(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, const CPatrolPath *path, const Fvector &position, u32 level_vertex_id, u32 flags, shared_str name);
										CDynamicPatrolPoint		(const CPatrolPath *path, CDynamicPatrolPoint *old);
										CDynamicPatrolPoint		() { };
		virtual							~CDynamicPatrolPoint	();

		
		virtual	CPatrolPoint			&load_raw				(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream);
#ifdef DEBUG
		IC		void					initialized				(bool value);
#endif
		IC		void					set_name				(LPCSTR name);
		IC		void					set_position			(Fvector3 pos);
		IC		void					set_level_vertex_id		(u32 lvid);
		IC		void					set_game_vertex_id		(GameGraph::_GRAPH_ID gvid);
		IC		void					set_flags				(u32 flags);
		IC		void					set_parent				(const CPatrolPath *path);
};

#include "dynamic_patrol_point_inline.h"

#endif
