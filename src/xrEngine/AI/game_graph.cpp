#include "stdafx.h"
#include "game_graph.h"
IGameGraph::IGameGraph()
{
}
IGameGraph::~IGameGraph()
{
}
void IGameGraph::save(IWriter& stream)
{
	m_header.save(&stream);

	u8* buffer = (u8*)m_nodes;
	stream.w(buffer, header().vertex_count() * sizeof(CVertex));
	buffer += header().vertex_count() * sizeof(CVertex);

	stream.w(buffer, header().edge_count() * sizeof(IGameGraph::CEdge));
	buffer += header().edge_count() * sizeof(IGameGraph::CEdge);

	stream.w(buffer, header().death_point_count() * sizeof(CLevelPoint));
	buffer += header().death_point_count() * sizeof(CLevelPoint);

	VERIFY((u8*)m_cross_tables == buffer);

	GameGraph::LEVEL_MAP::const_iterator	I = header().levels().begin();
	GameGraph::LEVEL_MAP::const_iterator	E = header().levels().end();
	for (; I != E; ++I) {
		u32						size = *(u32*)buffer;
		stream.w(buffer, size);
		buffer += size;
	}
}