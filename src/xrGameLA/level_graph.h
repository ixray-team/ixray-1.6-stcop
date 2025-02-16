////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.h
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "..\XrEngine\AI\level_graph.h"
#include "../Include/xrRender/DebugShader.h"

class CLevelGraph:public ILevelGraph
{
	IReader* m_reader;
private:
	friend class CRenumbererConverter;

public:
	CLevelGraph();
	virtual			~CLevelGraph();

#ifdef DEBUG
private:
	debug_shader			sh_debug;

private:
	int					m_current_level_id;
	bool				m_current_actual;
	Fvector				m_current_center;
	Fvector				m_current_radius;

public:
			void		setup_current_level		(const int &level_id);

private:
			Fvector		convert_position		(const Fvector &position);
			void		draw_edge				(const int &vertex_id0, const int &vertex_id1);
			void		draw_vertex				(const int &vertex_id);
			void		draw_stalkers			(const int &vertex_id);
			void		draw_objects			(const int &vertex_id);
			void		update_current_info		();

private:
			void		draw_nodes				();
			void		draw_restrictions		();
			void		draw_covers				();
			void		draw_game_graph			();
			void		draw_objects			();
			void		draw_debug_node			();

public:
			void		render					();
#	endif
};

#ifdef DEBUG
#	ifndef AI_COMPILER
		extern BOOL	g_bDebugNode;
		extern u32	g_dwDebugNodeSource;
		extern u32	g_dwDebugNodeDest;
#	endif
#endif