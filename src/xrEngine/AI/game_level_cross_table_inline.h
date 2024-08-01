#include "game_level_cross_table.h"
////////////////////////////////////////////////////////////////////////////
//	Module 		: game_level_cross_table_inline.h
//	Created 	: 20.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Cross table between game and level graphs inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once


IC const IGameLevelCrossTable::CCell &IGameLevelCrossTable::vertex(u32 level_vertex_id) const
{
	VERIFY				(level_vertex_id < header().level_vertex_count());
	return				(m_tpaCrossTable[level_vertex_id]);
}

IC	u32	IGameLevelCrossTable::CHeader::version() const
{
	return				(dwVersion);
}

IC	u32	IGameLevelCrossTable::CHeader::level_vertex_count() const
{
	return				(dwNodeCount);
}

IC	u32	IGameLevelCrossTable::CHeader::game_vertex_count() const
{
	return				(dwGraphPointCount);
}

IC	const xrGUID &IGameLevelCrossTable::CHeader::level_guid	() const
{
	return				(m_level_guid);
}

IC	const xrGUID &IGameLevelCrossTable::CHeader::game_guid	() const
{
	return				(m_game_guid);
}

IC	GameGraph::_GRAPH_ID IGameLevelCrossTable::CCell::game_vertex_id() const
{
	return				(tGraphIndex);
}

IC	float IGameLevelCrossTable::CCell::distance() const
{
	return				(fDistance);
}

IC	const IGameLevelCrossTable::CHeader &IGameLevelCrossTable::header() const
{
	return				(m_tCrossTableHeader);
}
