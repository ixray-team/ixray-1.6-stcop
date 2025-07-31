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

	if (header().version() == XRAI_MINIMAL_VERSION)
	{
		NodeCompressed10* temp = (NodeCompressed10*)m_reader->pointer();
		m_nodes = new CVertex[header().vertex_count()];

		for (u32 i = 0; i < header().vertex_count(); ++i)
		{
			std::memcpy(&m_nodes[i].high, &temp[i].high, sizeof(temp[i].high) + sizeof(temp[i].low) + sizeof(temp[i].plane) + sizeof(temp[i].p));

			for (u8 j = 0; j < 4; ++j)
			{
				m_nodes[i].link(j, temp[i].link(j));
			}
			m_nodes[i].light(temp[i].light());
		}
	}
	else
	{
		m_nodes = (CVertex*)m_reader->pointer();
	}

	m_row_length = iFloor((header().box().max.z - header().box().min.z) / header().cell_size() + EPS_L + 1.5f);
	m_column_length = iFloor((header().box().max.x - header().box().min.x) / header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign(header().vertex_count(), true);
	unpack_xz(vertex_position(header().box().max), m_max_x, m_max_z);
}

CLevelGraph::~CLevelGraph		()
{
	FS.r_close					(m_reader);
}
