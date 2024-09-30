////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph.h
//	Created 	: 18.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Game graph class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "game_graph_space.h"
#include "../../xrScripts/script_export_space.h"
#include "../../xrGame/game_level_cross_table.h"
#include "../../xrEngine/AI/game_graph.h"

class CGameGraph:public IGameGraph
{
public:

	IC 								CGameGraph				(LPCSTR file_name, u32 current_version = XRAI_CURRENT_VERSION);
	IC								CGameGraph				(const IReader &stream);
	IC		void					save					(IWriter &stream);
	IC		void					set_current_level		(u32 level_id);
			virtual					~CGameGraph()
			{
				xr_delete(m_current_level_cross_table);
				FS.r_close(m_reader);
			}
private:
	IReader*m_reader = nullptr;
};

#include "game_graph_inline.h"