#include "stdafx.h"
#include "game_graph_editor.h"

CGameGraphEditor::CGameGraphEditor()
{
	m_current_level_cross_table = nullptr;
	m_nodes = nullptr;
	m_edges = nullptr;
}

CGameGraphEditor::~CGameGraphEditor()
{
	xr_free(m_nodes);
	xr_free(m_edges);
	xr_delete(m_current_level_cross_table);
}

void CGameGraphEditor::set_current_level(u32 level_id)
{
	R_ASSERT(level_id == 0);
}

void CGameGraphEditor::realloc(const CHeader& new_header)
{
	m_header = new_header;
	m_nodes = (CVertex*)xr_realloc(m_nodes, sizeof(CVertex) * new_header.m_vertex_count);
	if (new_header.m_edge_count)
	{
		m_edges = (BYTE*)xr_realloc(m_edges, sizeof(CEdge) * new_header.m_edge_count);
	}
	else
	{
		xr_free(m_edges);
	}
	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	m_enabled.assign(header().vertex_count(), true);
}

void CGameGraphEditor::clear()
{
	xr_free(m_nodes);
	xr_free(m_edges);
	xr_delete(m_current_level_cross_table);
	m_current_level_cross_table = nullptr;
	m_nodes = nullptr;
	m_edges = nullptr;
}

bool CGameGraphEditor::empty() const
{
	return m_nodes==nullptr;
}

void CGameGraphEditor::set_cross_table(IGameLevelCrossTable* cross_table)
{
	xr_delete(m_current_level_cross_table);
	m_current_level_some_vertex_id = 0;
	m_current_level_cross_table = cross_table;
}

void CGameGraphEditor::save(IWriter& stream)
{
	m_header.save(&stream);
	size_t EdgeOffset =header().vertex_count() * sizeof(CVertex);
	for (size_t i = 0; i < header().vertex_count(); i++)
	{
		m_nodes[i].dwEdgeOffset += EdgeOffset;
	}

	stream.w(m_nodes, header().vertex_count() * sizeof(CVertex));

	stream.w(m_edges, header().edge_count() * sizeof(IGameGraph::CEdge));

	GameGraph::LEVEL_MAP::const_iterator	I = header().levels().begin();
	GameGraph::LEVEL_MAP::const_iterator	E = header().levels().end();
	for (; I != E; ++I) 
	{
		u32						size = m_current_level_cross_table->header().level_vertex_count();
		stream.w_u32(size * sizeof(IGameLevelCrossTable::CCell) + sizeof(IGameLevelCrossTable::CHeader) + 1);
		stream.w(&m_current_level_cross_table->header(), sizeof(IGameLevelCrossTable::CHeader));
		for (size_t i = 0; i < size; i++)
		{
			stream.w(&m_current_level_cross_table->vertex(i), sizeof(IGameLevelCrossTable::CCell));
		}
	}
	for (size_t i = 0; i < header().vertex_count(); i++)
	{
		m_nodes[i].dwEdgeOffset -= EdgeOffset;
	}
}
