////////////////////////////////////////////////////////////////////////////
//	Module 		: game_spawn_constructor.cpp
//	Created 	: 16.10.2004
//  Modified 	: 16.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Game spawn constructor
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "game_spawn_constructor.h"
#include "../../XrEngine/object_broker.h"
#include "level_spawn_constructor.h"
#include "../../xrServerEntities/xrServer_Objects_ALife_All.h"
#include "server_entity_wrapper.h"
#include "graph_engine_editor.h"
#include "patrol_path_storage.h"
#include "..\XrAPI\xrGameManager.h"

extern LPCSTR GAME_CONFIG;
extern LPCSTR generate_temp_file_name			(LPCSTR header0, LPCSTR header1, string_path& buffer);

#define NO_MULTITHREADING


CGameSpawnConstructor::~CGameSpawnConstructor	()
{
	delete_data						(m_level_spawns);
	delete_data						(m_spawn_graph);
//	xr_delete						(m_game_graph);
	xr_delete						(m_patrol_path_storage);
}

IC	shared_str CGameSpawnConstructor::actor_level_name()
{
	string256						temp;
	return							(
		strconcat(
			sizeof(temp),
			temp,
			*game_graph().header().level(
				game_graph().vertex(
					m_actor->CastALifeObject()->m_tGraphID
				)->level_id()).name(),
			".spawn"
		)
	);
}

extern void read_levels			(CInifile *ini, xr_set<CLevelInfo> &m_levels, bool rebuild_graph, xr_vector<LPCSTR> *);
void fill_needed_levels	(LPSTR levels, xr_vector<LPCSTR> &result);

bool CGameSpawnConstructor::load_spawns	(LPCSTR name, bool no_separator_check)
{
	m_spawn_id							= 0;

	// init spawn graph
	m_spawn_graph						= xr_new<SPAWN_GRAPH>();
	
	// init ini file
//	m_game_info							= xr_new<CInifile>(INI_FILE);
//	R_ASSERT							(m_game_info->section_exist("levels"));

	// init patrol path storage
	m_patrol_path_storage				= xr_new<CPatrolPathStorage>();
	xr_vector<LPCSTR>					needed_levels;
	string4096							levels_string;
	xr_strcpy							(levels_string,name);
	strlwr								(levels_string);
	//fill_needed_levels					(levels_string,needed_levels);

	// fill level info
	/*read_levels(
		&game_info(),
		m_levels,
		false,
		&needed_levels
	);*/

	// init game graph
//	generate_temp_file_name				("game_graph","",m_game_graph_id);
//	xrMergeGraphs						(m_game_graph_id,name,false);

	m_levels.insert(CLevelInfo(0, Scene->m_LevelOp.m_FNLevelPath, Fvector().set(0, 0, 0),"UwU"));
	m_game_graph = Scene->GetGameGraph();

	// load levels
	GameGraph::SLevel					level;
	LEVEL_INFO_STORAGE::const_iterator	I = m_levels.begin();
	LEVEL_INFO_STORAGE::const_iterator	E = m_levels.end();
	for ( ; I != E; ++I) {
		level.m_offset					= (*I).m_offset;
		level.m_name					= (*I).m_name;
		level.m_id						= (*I).m_id;
		Msg								("%s %2d %s","level",level.id(),*(*I).m_name);
		m_level_spawns.push_back		(xr_new<CLevelSpawnConstructor>(level,this,no_separator_check));
	}

	string256							temp;
	if (m_level_spawns.empty())
	{
		xr_sprintf(temp, "! There are no valid levels (with AI-map and graph) in the section 'levels' in the '%s' to build spawn file from!", "UwU");
		Msg(temp);
		return false;
	}
	return true;
}

bool CGameSpawnConstructor::process_spawns	()
{
	LEVEL_SPAWN_STORAGE::iterator		I = m_level_spawns.begin();
	LEVEL_SPAWN_STORAGE::iterator		E = m_level_spawns.end();
	for ( ; I != E; ++I)
#ifdef NO_MULTITHREADING
		if (!(*I)->Execute())
		{
			return false;
		}
#else
		m_thread_manager.start			(*I);
	m_thread_manager.wait				();
#endif

	I									= m_level_spawns.begin();
	for ( ; I != E; ++I)
		if (!(*I)->update())
		{
			return false;
		}

	if (!verify_level_changers())
		return false;
	if (!verify_spawns())
		return false;
	return true;
}

bool CGameSpawnConstructor::verify_spawns			(ALife::_SPAWN_ID spawn_id)
{
	xr_vector<ALife::_SPAWN_ID>::iterator	J = std::find(m_temp0.begin(),m_temp0.end(),spawn_id);
	if (J != m_temp0.end())
	{
		Msg("! RECURSIVE Spawn group chain found in spawn", m_spawn_graph->vertex(spawn_id)->data()->object().name_replace());
		return false;
	}
	m_temp0.push_back						(spawn_id);

	SPAWN_GRAPH::CVertex					*vertex = m_spawn_graph->vertex(spawn_id);
	SPAWN_GRAPH::const_iterator				I = vertex->edges().begin();
	SPAWN_GRAPH::const_iterator				E = vertex->edges().end();
	for (; I != E; ++I)
		if (!verify_spawns((*I).vertex_id()))
			return false;
	return true;
}

bool CGameSpawnConstructor::verify_spawns			()
{
	SPAWN_GRAPH::const_vertex_iterator		I = m_spawn_graph->vertices().begin();
	SPAWN_GRAPH::const_vertex_iterator		E = m_spawn_graph->vertices().end();
	for ( ; I != E; ++I) {
		m_temp0.clear						();
		if (!verify_spawns((*I).second->vertex_id()))
			return false;
	}
	return true;
}

bool CGameSpawnConstructor::verify_level_changers	()
{
	if (m_level_changers.empty())
		return true;

	Msg										("List of the level changers which are invalid for some reasons");
	LEVEL_CHANGER_STORAGE::const_iterator	I = m_level_changers.begin();
	LEVEL_CHANGER_STORAGE::const_iterator	E = m_level_changers.end();
	for ( ; I != E; ++I)
		Msg									("%s",(*I)->CastAbstract()->name_replace());

	//VERIFY2									(m_level_changers.empty(),"Some of the level changers setup incorrectly");
	return true;
}

bool CGameSpawnConstructor::save_spawn				(LPCSTR name, LPCSTR output)
{
	CMemoryWriter					stream;
	switch (xrGameManager::GetGame())
	{
	case EGame::SHOC:
		m_spawn_header.m_version = XRAI_SOC_CURRENT_VERSION;
		break;
	default:
		m_spawn_header.m_version = XRAI_CURRENT_VERSION;
		break;
	}
	m_spawn_header.m_guid			= generate_guid();
	m_spawn_header.m_graph_guid		= game_graph().header().guid();
	m_spawn_header.m_spawn_count	= spawn_graph().vertex_count();
	m_spawn_header.m_level_count	= (u32)m_level_spawns.size();
	
	stream.open_chunk				(0);
	stream.w_u32					(m_spawn_header.m_version);
	save_data						(m_spawn_header.m_guid,stream);
	save_data						(m_spawn_header.m_graph_guid,stream);
	stream.w_u32					(m_spawn_header.m_spawn_count);
	stream.w_u32					(m_spawn_header.m_level_count);
	stream.close_chunk				();
	
	stream.open_chunk				(1);
	save_data						(spawn_graph(),stream);
	stream.close_chunk				();

	stream.open_chunk				(2);
	save_data						(m_level_points,stream);
	stream.close_chunk				();

	stream.open_chunk				(3);
	save_data						(m_patrol_path_storage,stream);
	stream.close_chunk				();

	stream.open_chunk				(4);
	m_game_graph->save				(stream);
	stream.close_chunk				();

	return	stream.save_to(*spawn_name(output));
}

bool CGameSpawnConstructor::save_spawn(LPCSTR name, CMemoryWriter& stream)
{
	switch (xrGameManager::GetGame())
	{
	case EGame::SHOC:
		m_spawn_header.m_version = XRAI_SOC_CURRENT_VERSION;
		break;
	default:
		m_spawn_header.m_version = XRAI_CURRENT_VERSION;
		break;
	}

	m_spawn_header.m_guid = generate_guid();
	m_spawn_header.m_graph_guid = game_graph().header().guid();
	m_spawn_header.m_spawn_count = spawn_graph().vertex_count();
	m_spawn_header.m_level_count = (u32)m_level_spawns.size();

	stream.open_chunk(0);
	stream.w_u32(m_spawn_header.m_version);
	save_data(m_spawn_header.m_guid, stream);
	save_data(m_spawn_header.m_graph_guid, stream);
	stream.w_u32(m_spawn_header.m_spawn_count);
	stream.w_u32(m_spawn_header.m_level_count);
	stream.close_chunk();

	stream.open_chunk(1);
	save_data(spawn_graph(), stream);
	stream.close_chunk();

	stream.open_chunk(2);
	save_data(m_level_points, stream);
	stream.close_chunk();

	stream.open_chunk(3);
	save_data(m_patrol_path_storage, stream);
	stream.close_chunk();
	return true;
}

shared_str CGameSpawnConstructor::spawn_name	(LPCSTR output)
{
	string_path					file_name;
	if (!output)
		FS.update_path			(file_name,"$game_spawn$",*actor_level_name());
	else {
		actor_level_name		();
		string_path				out;
		strconcat				(sizeof(out),out,output,".spawn");
		FS.update_path			(file_name,"$game_spawn$",out);
	}
	return						(file_name);
}

void CGameSpawnConstructor::add_story_object	(ALife::_STORY_ID id, ISE_ALifeDynamicObject *object, LPCSTR level_name)
{
	if (id == INVALID_STORY_ID)
		return;

	auto		I = m_story_objects.find(id);
	if (I != m_story_objects.end()) {
		Msg						("Object %s, story id %d",object->CastAbstract()->name_replace(), object->CastALifeObject()->m_story_id);
		Msg						("Object %s, story id %d",(*I).second->CastAbstract()->name_replace(),(*I).second->CastALifeObject()->m_story_id);
		VERIFY3					(I == m_story_objects.end(),"There are several objects which has the same unique story ID, level ",level_name);
	}
	
	m_story_objects.insert		(std::make_pair(id,object));
}

void CGameSpawnConstructor::add_object				(ISE_Abstract *object)
{
	m_critical_section.Enter	();
	object->m_tSpawnID			= spawn_id();
	spawn_graph().add_vertex	(xr_new<CServerEntityWrapper>(object),object->m_tSpawnID);
	m_critical_section.Leave	();
}

void CGameSpawnConstructor::remove_object			(ISE_Abstract *object)
{
	spawn_graph().remove_vertex	(object->m_tSpawnID);
}

bool CGameSpawnConstructor::process_actor			(LPCSTR start_level_name)
{
	m_actor							= 0;
	
	LEVEL_SPAWN_STORAGE::iterator	I = m_level_spawns.begin();
	LEVEL_SPAWN_STORAGE::iterator	E = m_level_spawns.end();
	for ( ; I != E; ++I) {
		if (!(*I)->actor())
			continue;

		Msg							("Actor is on the level %s",*game_graph().header().level(game_graph().vertex((*I)->actor()->CastALifeObject()->m_tGraphID)->level_id()).name());
		
		if (m_actor)
		{
			Msg("! There must be the SINGLE level with ACTOR!");
			return false;
		}
		
		m_actor						= (*I)->actor();
	}

	if (!m_actor)
	{
		Msg("! There is no ACTOR spawn point!");
		return false;
	}

	//if (!start_level_name)
	//	return true;

	//if (!xr_strcmp(*actor_level_name(),start_level_name))
	//	return true;

	//const IGameGraph::SLevel		&level = game_graph().header().level(start_level_name);
	//GameGraph::_GRAPH_ID				dest = GameGraph::_GRAPH_ID(-1);
	//GraphEngineSpace::CGameLevelParams	evaluator(level.id());
	//CGraphEngineEditor					*graph_engine = xr_new<CGraphEngineEditor>(game_graph().header().vertex_count());

	//bool							failed = !graph_engine->search(game_graph(),m_actor->CastALifeObject()->m_tGraphID,GameGraph::_GRAPH_ID(-1),0,evaluator);
	//if (failed) {
	//	Msg							("! Cannot build path via game graph from the current level to the level %s!",start_level_name);
	//	float						min_dist = flt_max;
	//	Fvector						current = game_graph().vertex(m_actor->CastALifeObject()->m_tGraphID)->game_point();
	//	GameGraph::_GRAPH_ID			n = game_graph().header().vertex_count();
	//	for (GameGraph::_GRAPH_ID i=0; i<n; ++i) {
	//		if (game_graph().vertex(i)->level_id() == level.id()) {
	//			float				distance = game_graph().vertex(i)->game_point().distance_to_sqr(current);
	//			if (distance < min_dist) {
	//				min_dist		= distance;
	//				dest			= i;
	//			}
	//		}
	//	}
	//	if (!game_graph().vertex(dest)) {
	//		Msg						("! There is no game vertices on the level %s, cannot jump to the specified level",start_level_name);
	//		return false;
	//	}
	//}
	//else
	//	dest						= (GameGraph::_GRAPH_ID)evaluator.selected_vertex_id();

	//m_actor->CastALifeObject()->m_tGraphID				= dest;
	//m_actor->CastALifeObject()->m_tNodeID				= game_graph().vertex(dest)->level_vertex_id();
	//m_actor->CastAbstract()->o_Position				= game_graph().vertex(dest)->level_point();

	//xr_delete						(graph_engine);
	return true;
}

CGameSpawnConstructor::CGameSpawnConstructor()
{
}

bool CGameSpawnConstructor::build(LPCSTR name, LPCSTR output, LPCSTR start, bool no_separator_check)
{
	Msg("Start build spawn");
	if (!load_spawns(name, no_separator_check))
		return false;
	if (!process_spawns())
		return false;
	if (!process_actor(start))
		return false;
	if (!save_spawn(name, output))
		return false;
	return true;
}

bool CGameSpawnConstructor::build(LPCSTR name, CMemoryWriter& output, LPCSTR start, bool no_separator_check)
{
	Msg("Start build spawn");
	if (!load_spawns(name, no_separator_check))
		return false;
	if (!process_spawns())
		return false;
	if (!process_actor(start))
		return false;
	if (!save_spawn(name, output))
		return false;
	return true;
}

void clear_temp_folder	()
{
	string_path		query;
	FS.update_path	(query,"$app_data_root$","temp\\*.*");
	_finddata_t		file;
	intptr_t		handle = _findfirst(query, &file);
	if (handle == intptr_t(-1))
		return;

	typedef xr_vector<shared_str>	FILES;
	FILES			files;
	do {
		if (file.attrib & _A_SUBDIR)
			continue;

		files.push_back		(file.name);
	}
    while (!_findnext(handle, &file));

	_findclose		(handle);

	FILES::const_iterator	I = files.begin();
	FILES::const_iterator	E = files.end();
	for ( ; I != E; ++I) {
		if (DeleteFile(**I))
			Msg		("file %s is successfully deleted",**I);
		else
			Msg		("cannot delete file %s",**I);
	}
}