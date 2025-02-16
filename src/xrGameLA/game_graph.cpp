////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph_inline.h
//	Created 	: 18.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Game graph inline functions
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "game_graph.h"

CGameGraph::CGameGraph											()
{
	VERIFY(!Device.IsEditorMode());
	string_path						file_name;
	FS.update_path					(file_name,"$game_data$",GRAPH_NAME);

	m_reader						= FS.r_open(file_name);
	VERIFY							(m_reader);
	m_header.load					(m_reader);
	R_ASSERT2						(header().version() == XRAI_SOC_CURRENT_VERSION,"Graph version mismatch!");
	m_nodes							= (CVertex*)m_reader->pointer();
	m_edges = (BYTE*)m_nodes;
	m_current_level_some_vertex_id	= _GRAPH_ID(-1);
	m_current_level_cross_table     = nullptr;
	m_enabled.assign				(header().vertex_count(),true);
}


IC CGameGraph::~CGameGraph											()
{
	VERIFY(!Device.IsEditorMode());
	FS.r_close(m_reader);
}


void CGameGraph::set_current_level(u32 level_id)
{
	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	for (_GRAPH_ID i = 0, n = header().vertex_count(); i < n; ++i) {
		if (level_id != vertex(i)->level_id())
			continue;

		m_current_level_some_vertex_id = i;
		break;
	}

	VERIFY(valid_vertex_id(m_current_level_some_vertex_id));
}

