#include "StdAfx.h"
#include "game_level_cross_table.h"
CGameLevelCrossTable::CGameLevelCrossTable(const void* buffer, const u32& buffer_size)
{
	VERIFY(Device.IsEditorMode() == false);
	Memory.mem_copy(&m_tCrossTableHeader, buffer, sizeof(m_tCrossTableHeader));
	buffer = (const u8*)buffer + sizeof(m_tCrossTableHeader);

	R_ASSERT2(m_tCrossTableHeader.version() == XRAI_CURRENT_VERSION, "Cross table version mismatch!");

	m_tpaCrossTable = (CCell*)buffer;
}

CGameLevelCrossTable::~CGameLevelCrossTable()
{
}
