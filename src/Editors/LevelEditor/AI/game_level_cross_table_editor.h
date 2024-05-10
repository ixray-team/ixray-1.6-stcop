////////////////////////////////////////////////////////////////////////////
//	Module 		: game_level_cross_table.h
//	Created 	: 20.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Cross table between game and level graphs
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "game_level_cross_table.h"

class CGameLevelCrossTableEditor:public IGameLevelCrossTable
{
public:
	CGameLevelCrossTableEditor();
	virtual ~CGameLevelCrossTableEditor();
	void realloc(CHeader& new_header);
	IC		const CCell& vertex(u32 level_vertex_id) const { IGameLevelCrossTable::vertex(level_vertex_id); }
	IC		CCell& vertex(u32 level_vertex_id) { return m_tpaCrossTable[level_vertex_id]; }
};
