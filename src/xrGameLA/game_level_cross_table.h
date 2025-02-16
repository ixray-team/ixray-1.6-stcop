////////////////////////////////////////////////////////////////////////////
//	Module 		: game_level_cross_table.h
//	Created 	: 20.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Cross table between game and level graphs
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "..\XrEngine\game_level_cross_table.h"

#define CROSS_TABLE_NAME					"level.gct"

#define CROSS_TABLE_CHUNK_VERSION			0
#define CROSS_TABLE_CHUNK_DATA				1

class CGameLevelCrossTable :public IGameLevelCrossTable
{
	IReader* m_tpCrossTableVFS;
	IReader* m_chunk;
public:
						CGameLevelCrossTable();
	virtual				~CGameLevelCrossTable();
};
