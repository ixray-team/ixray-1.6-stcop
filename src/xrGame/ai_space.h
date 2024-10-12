////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_space.h
//	Created 	: 12.11.2003
//  Modified 	: 12.11.2003
//	Author		: Dmitriy Iassenev
//	Description : AI space class
////////////////////////////////////////////////////////////////////////////

#pragma once

class CGameGraph;
class CGameLevelCrossTable;
class CLevelGraph;
class CGraphEngine;
class CEF_Storage;
class CALifeSimulator;
class CCoverManager;
class CScriptEngine;
class CPatrolPathStorage;
class moving_objects;

namespace doors {
	class manager;
} // namespace doors

class CAI_Space {
private:
	friend class CALifeSimulator;
	friend class CALifeGraphRegistry;
	friend class CALifeSpawnRegistry;
	friend class CALifeSpawnRegistry;
	friend class CLevel;

private:
	IGameGraph							*m_game_graph;
	ILevelGraph							*m_level_graph;
	CEF_Storage							*m_ef_storage;
	CALifeSimulator						*m_alife_simulator;
	CCoverManager						*m_cover_manager;
	CPatrolPathStorage					*m_patrol_path_storage;
	moving_objects						*m_moving_objects;
	doors::manager						*m_doors_manager;

private:
			void						load					(LPCSTR level_name);
			void						unload					(bool reload = false);
			void						patrol_path_storage_raw	(IReader &stream);
			void						patrol_path_storage		(IReader &stream);
			void						patrol_path_storage_from_editor();
			void						set_alife				(CALifeSimulator *alife_simulator);
			void						game_graph				(IGameGraph *game_graph);
			void						destroy_game_graph(); // for MP

public:
										CAI_Space				();
	virtual								~CAI_Space				();
			void						init					();
	IC		IGameGraph					&game_graph				() const;
	IC		IGameGraph					*get_game_graph			() const;
	IC		ILevelGraph					&level_graph			() const;
	IC		const ILevelGraph			*get_level_graph		() const;
			const IGameLevelCrossTable	&cross_table			() const;
			const IGameLevelCrossTable	*get_cross_table		() const;
	IC		const CPatrolPathStorage	&patrol_paths			() const;
	IC		CEF_Storage					&ef_storage				() const;
	IC		const CALifeSimulator		&alife					() const;
	IC		const CALifeSimulator		*get_alife				() const;
	IC		const CCoverManager			&cover_manager			() const;
	IC		CScriptEngine				&script_engine			() const;
	IC		moving_objects				&moving_objects			() const;
	IC		doors::manager&				doors					() const;
	IC		CPatrolPathStorage&			patrol_paths_raw		();

#ifdef DEBUG
			void						validate				(const u32			level_id) const;
#endif
};

IC	CAI_Space	&ai	();

extern CAI_Space *g_ai_space;

#include "ai_space_inline.h"