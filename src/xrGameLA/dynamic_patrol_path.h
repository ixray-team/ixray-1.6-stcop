#pragma once
#ifndef __DYNAMIC_PATROL_PATH_H__
#define __DYNAMIC_PATROL_PATH_H__

#include "patrol_path.h"
#include "script_export_space.h"

class CDynamicPatrolPoint;

class CDynamicPatrolPath : public CPatrolPath
{
	private:
		typedef CPatrolPath							inherited;
		typedef xr_map<CDynamicPatrolPoint*, u16>	POINTS_STORAGE;
		typedef xr_map<u16, u16>					LINKS_STORAGE;
	public:

									CDynamicPatrolPath		(shared_str name = "");
									CDynamicPatrolPath		(CDynamicPatrolPath *old);
		virtual						~CDynamicPatrolPath		();
		virtual		CPatrolPath		&load_raw				(const CLevelGraph *level_graph, const CGameLevelCrossTable *cross, 
																const CGameGraph *game_graph, IReader &stream);
		
		IC			void			AddPoint				(CDynamicPatrolPoint *point);
		IC			void			AddLink					(CDynamicPatrolPoint *p0, CDynamicPatrolPoint *p1);
		IC			shared_str		GetName					() const;
	private:
		POINTS_STORAGE				m_dvertices;
		LINKS_STORAGE				m_dedges;
		u16							m_vertices_count;
		shared_str					m_name;
	public:
		DECLARE_SCRIPT_REGISTER_FUNCTION;
};

add_to_type_list(CDynamicPatrolPath)
#undef script_type_list
#define script_type_list save_type_list(CDynamicPatrolPath)


#include "dynamic_patrol_path_inline.h"

#endif

