////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph_inline.h
//	Created 	: 18.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Game graph inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC CGameGraph::CGameGraph									(LPCSTR file_name, u32 current_version)
{
	m_reader						= FS.r_open(file_name);
	VERIFY							(m_reader);
	m_header.load					(m_reader);
	R_ASSERT2						(header().version() == XRAI_CURRENT_VERSION,"Graph version mismatch!");
	m_nodes							= (CVertex*)m_reader->pointer();
	m_current_level_some_vertex_id	= _GRAPH_ID(-1);
	m_enabled.assign				(header().vertex_count(),true);
	u8								*temp = (u8*)(m_nodes + header().vertex_count());
	temp							+= header().edge_count()*sizeof(CGameGraph::CEdge);
	m_cross_tables					= (u32*)(((CLevelPoint*)temp) + header().death_point_count());
	m_current_level_cross_table		= 0;
}

IC CGameGraph::CGameGraph											(const IReader &_stream)
{
	IReader							&stream = const_cast<IReader&>(_stream);
	m_header.load					(&stream);
	R_ASSERT2						(header().version() == XRAI_CURRENT_VERSION,"Graph version mismatch!");
	m_nodes							= (CVertex*)stream.pointer();
	m_current_level_some_vertex_id	= _GRAPH_ID(-1);
	m_enabled.assign				(header().vertex_count(),true);
	u8								*temp = (u8*)(m_nodes + header().vertex_count());
	temp							+= header().edge_count()*sizeof(CGameGraph::CEdge);
	m_cross_tables					= (u32*)(((CLevelPoint*)temp) + header().death_point_count());
	m_current_level_cross_table		= 0;
}


IC	void CGameGraph::set_current_level								(u32 const level_id)
{
	xr_delete					(m_current_level_cross_table);
	u32							*current_cross_table = m_cross_tables;
	GameGraph::LEVEL_MAP::const_iterator	I = header().levels().begin();
	GameGraph::LEVEL_MAP::const_iterator	E = header().levels().end();
	for ( ; I != E; ++I) {
		if (level_id != (*I).first) {
			current_cross_table	= (u32*)((u8*)current_cross_table + *current_cross_table);
			continue;
		}

		m_current_level_cross_table	= xr_new<CGameLevelCrossTable>(current_cross_table + 1,*current_cross_table);
		break;
	}

	VERIFY						(m_current_level_cross_table);

	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	for (_GRAPH_ID i=0, n = header().vertex_count(); i<n; ++i) {
		if (level_id != vertex(i)->level_id())
			continue;

		m_current_level_some_vertex_id	= i;
		break;
	}

	VERIFY						(valid_vertex_id(m_current_level_some_vertex_id));
}


IC void CGameGraph::save								(IWriter &stream)
{
	m_header.save				(&stream);
	
	u8							*buffer = (u8*)m_nodes;
	stream.w					(buffer,header().vertex_count()*sizeof(CVertex));
	buffer						+= header().vertex_count()*sizeof(CVertex);

	stream.w					(buffer,header().edge_count()*sizeof(CGameGraph::CEdge));
	buffer						+= header().edge_count()*sizeof(CGameGraph::CEdge);

	stream.w					(buffer,header().death_point_count()*sizeof(CLevelPoint));
	buffer						+= header().death_point_count()*sizeof(CLevelPoint);

	VERIFY						((u8*)m_cross_tables == buffer);
	GameGraph::LEVEL_MAP::const_iterator	I = header().levels().begin();
	GameGraph::LEVEL_MAP::const_iterator	E = header().levels().end();
	for ( ; I != E; ++I) {
		u32						size = *(u32*)buffer;
		stream.w				(buffer,size);
		buffer					+= size;
	}
}