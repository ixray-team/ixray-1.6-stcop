////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.cpp
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "level_graph.h"
#include "profiler.h"
#include "../XrEngine/Editor/XrEditorSceneInterface.h"
LPCSTR LEVEL_GRAPH_NAME = "level.ai";
CLevelGraph::CLevelGraph		()
{
	VERIFY(Device.IsEditorMode() == false);
#ifndef AI_COMPILER
#ifdef DEBUG
	sh_debug->create				("debug\\ai_nodes","$null");
#endif
	string_path					file_name;
#ifndef MASTER_GOLD
	if (strstr(Core.Params, "-editor_scene"))
	{
		FS.update_path(file_name, "$level$", "level.ai.temp");
	}
	else
#endif
	{
		FS.update_path(file_name, "$level$", LEVEL_GRAPH_NAME);
	}
	
#else
	string256					file_name;
	strconcat					(sizeof(file_name), file_name, filename, LEVEL_GRAPH_NAME);
#endif
	m_reader					= FS.r_open	(file_name);

	// m_header & data
	m_header					= (CHeader*)m_reader->pointer();
	R_ASSERT					(header().version() == XRAI_CURRENT_VERSION);
	m_reader->advance			(sizeof(CHeader));
	m_nodes						= (CVertex*)m_reader->pointer();
	m_row_length				= iFloor((header().box().max.z - header().box().min.z)/header().cell_size() + EPS_L + 1.5f);
	m_column_length				= iFloor((header().box().max.x - header().box().min.x)/header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign		(header().vertex_count(),true);
	unpack_xz					(vertex_position(header().box().max),m_max_x,m_max_z);

#ifdef DEBUG
#	ifndef AI_COMPILER
		m_current_level_id		= -1;
		m_current_actual		= false;
		m_current_center		= Fvector().set(flt_max,flt_max,flt_max);
		m_current_radius		= Fvector().set(flt_max,flt_max,flt_max);
#	endif
#endif
}

CLevelGraph::~CLevelGraph		()
{
	VERIFY(Device.IsEditorMode() == false);
		FS.r_close(m_reader);
}
