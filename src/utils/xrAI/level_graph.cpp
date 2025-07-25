////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.cpp
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "level_graph.h"

LPCSTR LEVEL_GRAPH_NAME = "level.ai";

CLevelGraph::CLevelGraph(LPCSTR filename)
{
	string256					file_name;
	xr_strconcat(file_name, filename, LEVEL_GRAPH_NAME);
	m_reader = FS.r_open(file_name);

	// m_header & data
	m_header = (CHeader*)m_reader->pointer();
	R_ASSERT(header().version() > XRAI_MINIMAL_VERSION || header().version() < XRAI_CURRENT_VERSION);
	m_reader->advance(sizeof(CHeader));

	switch (header().version())
	{
		case XRAI_MINIMAL_VERSION: // ver 10 - CS/CoP format
		{
			NodeCompressed10* Src = (NodeCompressed10*)m_reader->pointer();
			m_nodes = new CVertex[header().vertex_count()];

			for (u32 i = 0; i < header().vertex_count(); ++i)
			{
				for (u8 j = 0; j < 4; ++j)
				{
					u32 link_value = Src[i].link(j);
					m_nodes[i].UncompressedNode.link(j, link_value);
				}

				// Остальные поля
				m_nodes[i].UncompressedNode.high = Src[i].high;
				m_nodes[i].UncompressedNode.low = Src[i].low;
				m_nodes[i].UncompressedNode.plane = Src[i].plane;

				m_nodes[i].UncompressedNode.p.xz(Src[i].p.xz());
				m_nodes[i].UncompressedNode.p.y(Src[i].p.y());
			}
			break;
		}
		case XRAI_CURRENT_VERSION: // ver 11 - 25-bit format
		{
			NodeCompressed* compressed_nodes = (NodeCompressed*)m_reader->pointer();
			m_nodes = new CVertex[header().vertex_count()];

			for (size_t i = 0; i < header().vertex_count(); ++i)
			{
				for (u8 link_idx = 0; link_idx < 4; ++link_idx)
				{
					u32 old_link = compressed_nodes[i].link(link_idx);
					m_nodes[i].UncompressedNode.link(link_idx, old_link);
				}

				m_nodes[i].UncompressedNode.high = compressed_nodes[i].high;
				m_nodes[i].UncompressedNode.low = compressed_nodes[i].low;
				m_nodes[i].UncompressedNode.plane = compressed_nodes[i].plane;

				m_nodes[i].UncompressedNode.p.xz(compressed_nodes[i].p.xz());
				m_nodes[i].UncompressedNode.p.y(compressed_nodes[i].p.y());
			}
			break;
		}
	}

	m_row_length = iFloor((header().box().max.z - header().box().min.z) / header().cell_size() + EPS_L + 1.5f);
	m_column_length = iFloor((header().box().max.x - header().box().min.x) / header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign(header().vertex_count(), true);
	unpack_xz(vertex_position(header().box().max), m_max_x, m_max_z);
}

CLevelGraph::~CLevelGraph()
{
	FS.r_close(m_reader);
	xr_delete(m_nodes);
}
