#include "StdAfx.h"
#include "game_graph.h"
CGameGraph::CGameGraph(const IReader& _stream)
{
	VERIFY(!Device.IsEditorMode());
	IReader& stream = const_cast<IReader&>(_stream);
	m_header.load(&stream);
	R_ASSERT2(header().version() == XRAI_CURRENT_VERSION, "Graph version mismatch!");
	m_nodes = (CVertex*)stream.pointer();
	m_edges = (BYTE*)stream.pointer();
	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	m_enabled.assign(header().vertex_count(), true);
	u8* temp = (u8*)(m_nodes + header().vertex_count());
	temp += header().edge_count() * sizeof(CGameGraph::CEdge);
	m_cross_tables = (u32*)(((CLevelPoint*)temp) + header().death_point_count());
	m_current_level_cross_table = 0;
}

CGameGraph::~CGameGraph()
{
	VERIFY(Device.IsEditorMode() == false);
	xr_delete(m_current_level_cross_table);
}

void CGameGraph::set_current_level(u32  level_id)
{
	xr_delete(m_current_level_cross_table);
	u32* current_cross_table = m_cross_tables;
	auto	I = header().levels().begin();
	auto	E = header().levels().end();
	for (; I != E; ++I) {
		if (level_id != (*I).first) {
			current_cross_table = (u32*)((u8*)current_cross_table + *current_cross_table);
			continue;
		}

		m_current_level_cross_table = xr_new<CGameLevelCrossTable>(current_cross_table + 1, *current_cross_table);
		break;
	}

	VERIFY(m_current_level_cross_table);

	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	for (_GRAPH_ID i = 0, n = header().vertex_count(); i < n; ++i) {
		if (level_id != vertex(i)->level_id())
			continue;

		m_current_level_some_vertex_id = i;
		break;
	}

	VERIFY(valid_vertex_id(m_current_level_some_vertex_id));
}