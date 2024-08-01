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
	VERIFY(level_vertex_id < header().level_vertex_count());
	return				(m_tpaCrossTable[level_vertex_id]);
}