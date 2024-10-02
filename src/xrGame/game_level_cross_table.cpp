#include "StdAfx.h"
#include "game_level_cross_table.h"

#ifdef AI_COMPILER
CGameLevelCrossTable::CGameLevelCrossTable(LPCSTR fName)
{
	m_tpCrossTableVFS = FS.r_open(fName);
	R_ASSERT2(m_tpCrossTableVFS, "Can't open cross table!");

	IReader* chunk = m_tpCrossTableVFS->open_chunk(CROSS_TABLE_CHUNK_VERSION);
	R_ASSERT2(chunk, "Cross table is corrupted!");
	chunk->r(&m_tCrossTableHeader, sizeof(m_tCrossTableHeader));
	chunk->close();

	R_ASSERT2(m_tCrossTableHeader.version() == XRAI_CURRENT_VERSION, "Cross table version mismatch!");

	IReader* m_chunk = m_tpCrossTableVFS->open_chunk(CROSS_TABLE_CHUNK_DATA);
	R_ASSERT2(m_chunk, "Cross table is corrupted!");
	m_tpaCrossTable = (CCell*)m_chunk->pointer();
}
#endif // AI_COMPILER

CGameLevelCrossTable::CGameLevelCrossTable(const void* buffer, const u32& buffer_size)
{
	VERIFY(DevicePtr == nullptr || Device.IsEditorMode() == false);
	Memory.mem_copy(&m_tCrossTableHeader, buffer, sizeof(m_tCrossTableHeader));
	buffer = (const u8*)buffer + sizeof(m_tCrossTableHeader);

	R_ASSERT2(m_tCrossTableHeader.version() == XRAI_CURRENT_VERSION, "Cross table version mismatch!");

	m_tpaCrossTable = (CCell*)buffer;
}

CGameLevelCrossTable::~CGameLevelCrossTable()
{
	if (!m_tpCrossTableVFS)
		return;

	FS.r_close(m_tpCrossTableVFS);
};