////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph.h
//	Created 	: 18.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Game graph class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrEngine/AI/game_graph.h"
#include "../xrScripts/script_export_space.h"
#include "game_level_cross_table.h"

class CGameGraph:public IGameGraph
{
private:
	friend class CRenumbererConverter;
public:
	CGameGraph(const IReader &stream);
	virtual ~CGameGraph();
	virtual void set_current_level(u32 const level_id);
	DECLARE_SCRIPT_REGISTER_FUNCTION
};