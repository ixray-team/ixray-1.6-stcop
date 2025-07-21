////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.cpp
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "level_graph.h"
#include "../xrEngine/Editor/XrEditorSceneInterface.h"

LPCSTR LEVEL_GRAPH_NAME = "level.ai";
CLevelGraph::CLevelGraph()
{
	VERIFY(Device.IsEditorMode() == false);
	string_path file_name;

#ifndef AI_COMPILER
	FS.update_path(file_name, "$level$", LEVEL_GRAPH_NAME);
#else
	strconcat(sizeof(file_name), file_name, filename, LEVEL_GRAPH_NAME);
#endif

	m_reader = FS.r_open(file_name);

	// m_header & data
	m_header = (CHeader*)m_reader->pointer();
	const u32 AIVersion = header().version();
	R_ASSERT(AIVersion >= XRAI_MINIMAL_VERSION && AIVersion <= XRAI_CURRENT_VERSION);
	m_reader->advance(sizeof(CHeader));

	switch (AIVersion)
	{
		case XRAI_MINIMAL_VERSION:
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
			break;
		}
		case XRAI_CURRENT_VERSION:
		{
			m_nodes = (CVertex*)m_reader->pointer();
			break;
		}
	}

	m_row_length				= iFloor((header().box().max.z - header().box().min.z)/header().cell_size() + EPS_L + 1.5f);
	m_column_length				= iFloor((header().box().max.x - header().box().min.x)/header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign		(header().vertex_count(),true);
	unpack_xz					(vertex_position(header().box().max),m_max_x,m_max_z);

#ifdef DEBUG
#	ifndef AI_COMPILER
	sh_debug->create("debug\\ai_nodes", "$null");
	m_current_level_id = -1;
	m_current_actual = false;
	m_current_center = Fvector().set(flt_max,flt_max,flt_max);
	m_current_radius = Fvector().set(flt_max,flt_max,flt_max);
#	endif
#endif
}

CLevelGraph::~CLevelGraph()
{
	if (m_header->version() < XRAI_CURRENT_VERSION)
	{
		xr_delete(m_nodes);
	}

	VERIFY(Device.IsEditorMode() == false);
	FS.r_close(m_reader);
}
