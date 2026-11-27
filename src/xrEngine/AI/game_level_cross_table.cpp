#include "stdafx.h"
#include "game_level_cross_table.h"

IGameLevelCrossTable::IGameLevelCrossTable()
{
}

IGameLevelCrossTable::~IGameLevelCrossTable()
{
}

const IGameLevelCrossTable::CHeader& IGameLevelCrossTable::header() const
{
	return				(m_tCrossTableHeader);
}

const IGameLevelCrossTable::CCell& IGameLevelCrossTable::vertex(u32 level_vertex_id) const
{
	// Out-of-range ids happen for entities outside the AI map or on a degraded
	// graph: answer the invalid cell instead of asserting.
	static constexpr CCell s_invalidCell = { GameGraph::_GRAPH_ID(-1), 0.f };
	if (level_vertex_id >= header().level_vertex_count())
		return			(s_invalidCell);
	return				(m_tpaCrossTable[level_vertex_id]);
}