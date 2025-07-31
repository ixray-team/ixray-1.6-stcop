#pragma once
#include "../xrEngine/AI/game_level_cross_table.h"

class CGameLevelCrossTable :public  IGameLevelCrossTable
{

public:
	IReader* m_tpCrossTableVFS = nullptr;

public:
	CGameLevelCrossTable(const void* buffer, const u32& buffer_size);
	CGameLevelCrossTable(LPCSTR fName);

	virtual ~CGameLevelCrossTable();
};