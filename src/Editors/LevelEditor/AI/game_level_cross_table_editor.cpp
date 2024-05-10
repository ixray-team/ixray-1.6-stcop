#include "stdafx.h"
#include "game_level_cross_table_editor.h"

CGameLevelCrossTableEditor::CGameLevelCrossTableEditor()
{
	m_tpaCrossTable = nullptr;
}

CGameLevelCrossTableEditor::~CGameLevelCrossTableEditor()
{
	xr_free(m_tpaCrossTable);
}

void CGameLevelCrossTableEditor::realloc(CHeader& new_header)
{
	m_tCrossTableHeader = new_header;
	m_tpaCrossTable = (CCell*)xr_realloc(m_tpaCrossTable,sizeof(CCell)* m_tCrossTableHeader.level_vertex_count());
}
