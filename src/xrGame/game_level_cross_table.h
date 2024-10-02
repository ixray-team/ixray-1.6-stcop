#pragma once
#include "..\XrEngine\AI\game_level_cross_table.h"

class CGameLevelCrossTable :public  IGameLevelCrossTable
{

public:
	IReader* m_tpCrossTableVFS = nullptr;

public:
	CGameLevelCrossTable(const void* buffer, const u32& buffer_size);
#ifdef AI_COMPILER
	CGameLevelCrossTable(LPCSTR fName);
#endif

	virtual ~CGameLevelCrossTable();
};