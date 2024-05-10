////////////////////////////////////////////////////////////////////////////
//	Module 		: level_spawn_constructor.h
//	Created 	: 16.10.2004
//  Modified 	: 16.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Level spawn constructor
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "xrThread.h"
#include "spawn_constructor_space.h"

class ILevelGraph;
class IGameLevelCrossTable;
class CGameSpawnConstructor;
class ISE_ALifeCreatureActor;
class CGraphEngineEditor;
class ISE_Abstract;
class ISE_ALifeObject;
class ISE_ALifeGraphPoint;
//class ISE_SpawnGroup;
class ISE_ALifeAnomalousZone;
class CSpaceRestrictorWrapper;
class CPatrolPathStorage;
class ISE_ALifeDynamicObject;

class CLevelSpawnConstructor  {
public:
	typedef SpawnConstructorSpace::LEVEL_POINT_STORAGE			LEVEL_POINT_STORAGE;
	typedef  xr_vector<ISE_ALifeLevelChanger*>					LEVEL_CHANGER_STORAGE;
	typedef xr_vector<ISE_ALifeObject*>							SPAWN_STORAGE;
	typedef xr_vector<ISE_ALifeGraphPoint*>						GRAPH_POINT_STORAGE;
	typedef xr_vector<CSpaceRestrictorWrapper*>					SPACE_RESTRICTORS;
//	typedef xr_vector<ISE_Abstract*>							GROUP_OBJECTS;
//	typedef xr_map<shared_str,GROUP_OBJECTS*>					SPAWN_GRPOUP_OBJECTS;
//	typedef xr_map<shared_str,ISE_SpawnGroup*>					SPAWN_GROUPS;

private:
	IGameGraph::SLevel					m_level;
	SPAWN_STORAGE						m_spawns;
	LEVEL_POINT_STORAGE					m_level_points;
	GRAPH_POINT_STORAGE					m_graph_points;
	SPACE_RESTRICTORS					m_space_restrictors;
//	SPAWN_GRPOUP_OBJECTS				m_spawn_objects;
//	SPAWN_GROUPS						m_spawn_groups;
	CGameSpawnConstructor				*m_game_spawn_constructor;
	ISE_ALifeCreatureActor				*m_actor;
	ILevelGraph							*m_level_graph;
	CGraphEngineEditor						*m_graph_engine;
	LEVEL_CHANGER_STORAGE				m_level_changers;
	bool								m_no_separator_check;

private:
	const IGameLevelCrossTable			*m_cross_table;

protected:
			void						init								();
			bool						load_objects						();
//			void						fill_spawn_groups					();
			bool						correct_objects						();
			void						generate_artefact_spawn_positions	();
			void						correct_level_changers				();
			bool						verify_space_restrictors			();
			void						fill_level_changers					();
			ISE_Abstract				*create_object						(IReader				*chunk);
			void						add_graph_point						(ISE_Abstract			*abstract);
//			void						add_spawn_group						(ISE_Abstract			*abstract);
			void						add_story_object					(ISE_ALifeDynamicObject *dynamic_object);
			void						add_space_restrictor				(ISE_ALifeDynamicObject *dynamic_object);
			void						add_free_object						(ISE_Abstract			*abstract);
//			void						add_group_object					(ISE_Abstract			*abstract, shared_str group_section, bool);
//			void						add_group_object					(ISE_Abstract			*abstract, shared_str group_section);
			void						add_level_changer					(ISE_Abstract			*abstract);
			void						update_artefact_spawn_positions		();
//	IC		void						normalize_probability				(ISE_ALifeAnomalousZone *zone);
//	IC		void						free_group_objects					();
	IC		const IGameGraph			&game_graph							() const;
	IC		const ILevelGraph			&level_graph						() const;
	IC		const IGameLevelCrossTable	&cross_table						() const;
	IC		CGraphEngineEditor				&graph_engine						() const;
	IC		LEVEL_CHANGER_STORAGE		&level_changers						() const;
	IC		u32							level_id							(shared_str level_name) const;

public:
	IC									CLevelSpawnConstructor				(const IGameGraph::SLevel &level, CGameSpawnConstructor *game_spawn_constructor, bool no_separator_check);
	virtual								~CLevelSpawnConstructor				();
	virtual bool						Execute								();
	IC		ISE_ALifeCreatureActor		*actor								() const;
	IC		const IGameGraph::SLevel	&level								() const;
			bool						update								();
	IC		CGameSpawnConstructor		&game_spawn_constructor				() const;
private:
	static void					ThreadGenerateArtefactSpawnPositionStartup(void* args);
	void						generate_artefact_spawn_positions_worker();
	volatile UINT32				m_generate_artefact_spawn_positions_worker_counter;
	xrCriticalSection			m_generate_artefact_spawn_positions_worker_mutex;
};

#include "level_spawn_constructor_inline.h"