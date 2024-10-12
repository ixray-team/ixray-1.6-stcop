////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_space_inline.h
//	Created 	: 12.11.2003
//  Modified 	: 25.11.2003
//	Author		: Dmitriy Iassenev
//	Description : AI space class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../xrScripts/script_engine.h"

IC	IGameGraph					&CAI_Space::game_graph				() const
{
	VERIFY					(m_game_graph);
	return					(*m_game_graph);
}

IC	IGameGraph					*CAI_Space::get_game_graph			() const
{
	return					(m_game_graph);
}

IC	ILevelGraph		&CAI_Space::level_graph							() const
{
	VERIFY					(m_level_graph);
	return					(*m_level_graph);
}

IC	const ILevelGraph	*CAI_Space::get_level_graph					() const
{
	return					(m_level_graph);
}

IC	CEF_Storage					&CAI_Space::ef_storage				() const
{
	VERIFY					(m_ef_storage);
	return					(*m_ef_storage);
}

IC	const CALifeSimulator		&CAI_Space::alife					() const
{
	VERIFY					(m_alife_simulator);
	return					(*m_alife_simulator);
}

IC	const CALifeSimulator		*CAI_Space::get_alife				() const
{
	return					(m_alife_simulator);
}

IC	const CCoverManager			&CAI_Space::cover_manager			() const
{
	VERIFY					(m_cover_manager);
	return					(*m_cover_manager);
}

IC	CScriptEngine				&CAI_Space::script_engine			() const
{
	VERIFY					(g_pScriptEngine);
	return					(*g_pScriptEngine);
}

IC	const CPatrolPathStorage &CAI_Space::patrol_paths				() const
{
	VERIFY					(m_patrol_path_storage);
	return					(*m_patrol_path_storage);
}

IC	moving_objects &CAI_Space::moving_objects						() const
{
	VERIFY					(m_moving_objects);
	return					(*m_moving_objects);
}

IC	doors::manager& CAI_Space::doors								() const
{
	VERIFY					(m_doors_manager);
	return					(*m_doors_manager);
}

IC	CAI_Space &ai													()
{
	if (!g_ai_space) {
		g_ai_space			= new CAI_Space();
		g_ai_space->init	();
	}
	return					(*g_ai_space);
}

IC CPatrolPathStorage& CAI_Space::patrol_paths_raw()
{
	VERIFY(m_patrol_path_storage);
	return *m_patrol_path_storage;
}