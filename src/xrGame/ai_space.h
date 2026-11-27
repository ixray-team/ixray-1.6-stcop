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
class CStalkerAnimationDataStorage;
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
	// Lazily created (see doors()): mutable for the const accessor.
	mutable doors::manager				*m_doors_manager;
	CScriptEngine*						m_script_engine;
	xr_shared_ptr<CScriptEngine>		SharedScriptEngine;
	CStalkerAnimationDataStorage*		AnimationData = nullptr;
	bool								m_owns_script_engine;
	bool								m_alife_borrowed = false;
	bool								m_game_graph_enabled = true;
	bool								m_cross_table_enabled = true;
	IGameLevelCrossTable				m_empty_cross_table;
	xr_unique_ptr<IReader> GameGraphData;
	xr_vector<u8> GameGraphBytes;
	mutable xr_unique_ptr<IGameGraph> EmptyGameGraph;
	mutable CLevelGraph* EmptyLevelGraph = nullptr;

private:
			void						load					(const char* level_name);
			void						unload					(bool reload = false);
			void						unload_level_graph		();
			void						patrol_path_storage_raw	(IReader &stream);
			void						patrol_path_storage		(IReader &stream);
			void						patrol_path_storage		(CPatrolPathStorage *Storage);
			void						patrol_path_storage_from_editor();
			void						set_alife				(CALifeSimulator *alife_simulator);
			void						game_graph				(IGameGraph *game_graph);
			void						destroy_game_graph(); // for MP

public:
										CAI_Space				();
			void						StartLevelPreload		(const shared_str& LevelName);
			void						DropLevelPreload		();
	virtual								~CAI_Space				();
			void						init					(bool dedicatedScriptEngine = false);
			void						SetAlifeBorrowed(CALifeSimulator* simulator);
			void						SetSharedScriptEngine(xr_shared_ptr<CScriptEngine> engine) { SharedScriptEngine = std::move(engine); }
			xr_shared_ptr<CScriptEngine>	GetSharedScriptEngine() const { return SharedScriptEngine; }
			bool						LoadGameGraph(IReader& stream);
			bool						BindCurrentLevel(const char* levelName);
			void						UnloadGameGraph();
			void						SetGameGraphEnabled(bool enabled);
			void						SetCrossTableEnabled(bool enabled) { m_cross_table_enabled = enabled; }
	IC		CScriptEngine* get_script_engine() const { return m_script_engine; }
	IC		IGameGraph					&game_graph				() const;
	IC		IGameGraph					*get_game_graph			() const;
	IC		ILevelGraph					&level_graph			() const;
	IC		const ILevelGraph			*get_level_graph		() const;
			const IGameLevelCrossTable	&cross_table			() const;
			const IGameLevelCrossTable	*get_cross_table		() const;
	IC		const CPatrolPathStorage	&patrol_paths			() const;
	IC		CEF_Storage					&ef_storage				() const;
	IC		const CALifeSimulator		&alife					() const;
	IC		CALifeSimulator				*get_alife				() const;
	IC		const CCoverManager			&cover_manager			() const;
	IC		CScriptEngine				&script_engine			() const;
	IC		moving_objects				&moving_objects			() const;
	IC		CStalkerAnimationDataStorage&	animation_data		() const;
	// Out-of-line (ai_space.cpp): lazily created, needs the full type.
			doors::manager&				doors					() const;
	IC		CPatrolPathStorage&			patrol_paths_raw		();

#ifdef DEBUG
			void						validate				(const u32			level_id) const;
#endif
};

IC	CAI_Space	&ai	();

extern CAI_Space *g_ai_space;

#include "ai_space_inline.h"
