////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_space_inline.h
//	Created 	: 12.11.2003
//  Modified 	: 25.11.2003
//	Author		: Dmitriy Iassenev
//	Description : AI space class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../xrScripts/script_engine.h"
#include "../xrEngine/IGame_ObjectPool.h"
#include "level_graph.h"

namespace ai_space_detail
{
	class CEmptyGameGraph final : public IGameGraph
	{
	public:
		CEmptyGameGraph()
		{
			m_header.m_version = 0;
			m_header.m_vertex_count = 0;
			m_header.m_edge_count = 0;
			m_header.m_death_point_count = 0;
			m_header.m_levels.clear();
			m_nodes = nullptr;
			m_edges = nullptr;
			m_cross_tables = nullptr;
			m_current_level_some_vertex_id = _GRAPH_ID(-1);
			m_enabled.clear();
			m_emptyCrossTable.m_tpaCrossTable = nullptr;
			m_emptyCrossTable.m_tCrossTableHeader.dwNodeCount = 0;
			m_emptyCrossTable.m_tCrossTableHeader.dwGraphPointCount = 0;
			m_current_level_cross_table = &m_emptyCrossTable;
		}
		void set_current_level(u32) override {}

	private:
		IGameLevelCrossTable m_emptyCrossTable;
	};
}

IC	IGameGraph					&CAI_Space::game_graph				() const
{
	if (m_game_graph)
		return					(*m_game_graph);

	if (!EmptyGameGraph)
		EmptyGameGraph.reset(new ai_space_detail::CEmptyGameGraph());
	return *EmptyGameGraph;
}

IC	IGameGraph					*CAI_Space::get_game_graph			() const
{
	return m_game_graph_enabled ? m_game_graph : nullptr;
}

IC	ILevelGraph		&CAI_Space::level_graph							() const
{
	if (m_level_graph)
		return					(*m_level_graph);

	if (!EmptyLevelGraph)
		EmptyLevelGraph = new CLevelGraph(CLevelGraph::EmptyTag{});
	return *EmptyLevelGraph;
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

IC	CALifeSimulator		*CAI_Space::get_alife				() const
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
	VERIFY2					(!IGame_ObjectPool::IsParallelLoad(), "Primary-thread work in a parallel CObject::Load: queue it with IGame_ObjectPool::RunOnPrimaryThread");
	if (m_script_engine)
		return *m_script_engine;
	VERIFY(g_pScriptEngine);
	return *g_pScriptEngine;
}

IC	const CPatrolPathStorage &CAI_Space::patrol_paths				() const
{
	VERIFY					(m_patrol_path_storage);
	return					(*m_patrol_path_storage);
}

IC	CStalkerAnimationDataStorage& CAI_Space::animation_data() const
{
	VERIFY(AnimationData);
	return *AnimationData;
}

IC	moving_objects &CAI_Space::moving_objects						() const
{
	VERIFY					(m_moving_objects);
	return					(*m_moving_objects);
}

IC	CAI_Space &ai													()
{
	if (g_pGameLevel != nullptr && g_pGameLevel->Services.AI != nullptr)
		return *g_pGameLevel->Services.AI;
	if (!g_ai_space) {
		g_ai_space			= new CAI_Space();
		g_ai_space->init	();
	}
	if (g_pGameLevel != nullptr && !g_pGameLevel->IsParallel())
		g_pGameLevel->Services.AI = g_ai_space;
	return					(*g_ai_space);
}

IC CPatrolPathStorage& CAI_Space::patrol_paths_raw()
{
	VERIFY(m_patrol_path_storage);
	return *m_patrol_path_storage;
}
