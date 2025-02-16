////////////////////////////////////////////////////////////////////////////
//	Module 		: game_level_cross_table_inline.h
//	Created 	: 20.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Cross table between game and level graphs inline functions
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "game_level_cross_table.h"

CGameLevelCrossTable::CGameLevelCrossTable()
{
	VERIFY(Device->IsEditorMode() == false);
	string_path			fName;
	FS.update_path		(fName,"$level$",CROSS_TABLE_NAME);

	m_tpCrossTableVFS	= FS.r_open(fName);
	R_ASSERT2			(m_tpCrossTableVFS,"Can't open cross table!");
	
	IReader				*chunk = m_tpCrossTableVFS->open_chunk(CROSS_TABLE_CHUNK_VERSION);
	R_ASSERT2			(chunk,"Cross table is corrupted!");
	chunk->r			(&m_tCrossTableHeader,sizeof(m_tCrossTableHeader));
	chunk->close		();
	
	R_ASSERT2			(m_tCrossTableHeader.version() == XRAI_SOC_CURRENT_VERSION,"Cross table version mismatch!");

	m_chunk				= m_tpCrossTableVFS->open_chunk(CROSS_TABLE_CHUNK_DATA);
	R_ASSERT2			(m_chunk,"Cross table is corrupted!");
	m_tpaCrossTable		= (CCell*)m_chunk->pointer();
};

CGameLevelCrossTable::~CGameLevelCrossTable	()
{
	VERIFY(Device->IsEditorMode() == false);
	VERIFY				(m_chunk);
	m_chunk->close		();
	
	VERIFY				(m_tpCrossTableVFS);
	FS.r_close			(m_tpCrossTableVFS);
};
