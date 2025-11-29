////////////////////////////////////////////////////////////////////////////
//	Module 		: game_spawn_constructor.cpp
//	Created 	: 16.10.2004
//  Modified 	: 16.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Game spawn constructor
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "game_spawn_constructor.h"
#include "level_spawn_constructor.h"
#include "../../xrServerEntities/xrServer_Objects.h"
#include "../../xrServerEntities/xrServer_Objects_Abstract.h"
#include "../../xrServerEntities/xrServer_Objects_ALife_All.h"
#include "../../xrServerEntities/server_entity_wrapper.h"
#include "graph_engine_editor.h"
#include "patrol_path_storage.h"
#include "../../../xrCore/Save/MemoryBuffer.h"
#include "../../../xrCore/Save/SaveManager.h"

extern const char* GAME_CONFIG;

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
		xr_strconcat(
			temp,
			*game_graph().header().level(
				game_graph().vertex(
					smart_cast<CSE_ALifeObject*>(m_actor)->m_tGraphID
				)->level_id()).name(),
			".spawn"
		)
	);
}

extern void read_levels			(CInifile *ini, xr_set<CLevelInfo> &m_levels, bool rebuild_graph, xr_vector<const char*> *);
void fill_needed_levels	(LPSTR levels, xr_vector<const char*> &result);

bool CGameSpawnConstructor::load_spawns	(const char* name, bool no_separator_check)
{
	m_spawn_id							= 0;

	// init spawn graph
	m_spawn_graph						= new SPAWN_GRAPH();
	
	// init patrol path storage
	m_patrol_path_storage				= new CPatrolPathStorage();
	xr_vector<const char*>					needed_levels;
	string4096							levels_string;
	xr_strcpy							(levels_string,name);
	strlwr								(levels_string);

	// init game graph
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
		m_level_spawns.push_back		(new CLevelSpawnConstructor(level,this,no_separator_check));
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
	if (!(*I)->Execute())
	{
		return false;
	}
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
		Msg									("%s",(*I)->name_replace());

	//VERIFY2									(m_level_changers.empty(),"Some of the level changers setup incorrectly");
	return true;
}

bool CGameSpawnConstructor::save_spawn				(const char* name, const char* output)
{
	CMemoryWriter					stream;
	m_spawn_header.m_version = XRAI_CURRENT_VERSION;
	m_spawn_header.m_guid			= generate_guid();
	m_spawn_header.m_graph_guid		= game_graph().header().guid();
	m_spawn_header.m_spawn_count	= spawn_graph().vertex_count();
	m_spawn_header.m_level_count	= (u32)m_level_spawns.size();
	
	stream.open_chunk(SpawnFileChunks::Header);
	stream.w_u32(m_spawn_header.m_version);
	save_data(m_spawn_header.m_guid,stream);
	save_data(m_spawn_header.m_graph_guid,stream);
	stream.w_u32(m_spawn_header.m_spawn_count);
	stream.w_u32(m_spawn_header.m_level_count);
	stream.close_chunk();

	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		stream.open_chunk(SpawnFileChunks::SpawnGraphNew);
		auto& graph = spawn_graph();

		stream.open_chunk(GraphAbstractChunks::VerticesNum);
		stream.w_u32(graph.vertex_count());
		stream.close_chunk();

		stream.open_chunk(GraphAbstractChunks::VerticesData);
		auto I = graph.vertices().begin();
		auto E = graph.vertices().end();
		SSaveTask dummy;
		for (int i=0; I != E; ++I)
		{
			stream.open_chunk(i);
			{
				stream.open_chunk(GraphAbstractVertexChunks::ID);
				save_data(I->second->vertex_id(),stream);
				stream.close_chunk();

				stream.open_chunk(GraphAbstractVertexChunks::Data);

				auto& obj = I->second->data()->object();
				auto SaveObjPtr = CSaveManager::GetInstance().EditorBeginSave();
				auto& SaveObj = *SaveObjPtr;
				shared_str temp = obj.name();
				SaveObj << temp;
				obj.Spawn_Serialize(SaveObj, true);
				obj.UPDATE_Serialize(SaveObj);
				CMemoryBuffer buff;
				buff.Write(ESaveVariableType::t_chunk);
				SaveObj.Write(&buff, &dummy);
				buff.Write((IWriter*)(&stream));
				xr_delete(SaveObjPtr);
				
				stream.close_chunk();
			}
			stream.close_chunk();
		}
		stream.close_chunk();

		stream.open_chunk(GraphAbstractChunks::Edges);
		{
			for (auto& Vertex : graph.vertices())
			{
				if (Vertex.second->edges().empty())
				{
					continue;
				}
				save_data(Vertex.second->vertex_id(),stream);

				stream.w_u32(Vertex.second->edges().size());
				for (auto& Edge : Vertex.second->edges())
				{
					save_data(Edge.vertex_id(),stream);
					save_data(Edge.weight(),stream);
				}
			}
		}
		stream.close_chunk();
		
		stream.close_chunk();
		
	} else
	{
		stream.open_chunk(SpawnFileChunks::SpawnGraphOld);
		save_data(spawn_graph(),stream);
		stream.close_chunk();
	}

	stream.open_chunk(SpawnFileChunks::LevelPoints);
	save_data(m_level_points,stream);
	stream.close_chunk();

	stream.open_chunk(SpawnFileChunks::PatrolPathStorage);
	save_data(m_patrol_path_storage,stream);
	stream.close_chunk();

	stream.open_chunk(SpawnFileChunks::GameGraph);
	m_game_graph->save(stream);
	stream.close_chunk();

	return stream.save_to(*spawn_name(output));
}

bool CGameSpawnConstructor::save_spawn(const char* name, CMemoryWriter& stream)
{
	m_spawn_header.m_version = XRAI_CURRENT_VERSION;
	m_spawn_header.m_guid = generate_guid();
	m_spawn_header.m_graph_guid = game_graph().header().guid();
	m_spawn_header.m_spawn_count = spawn_graph().vertex_count();
	m_spawn_header.m_level_count = (u32)m_level_spawns.size();

	stream.open_chunk(SpawnFileChunks::Header);
	stream.w_u32(m_spawn_header.m_version);
	save_data(m_spawn_header.m_guid, stream);
	save_data(m_spawn_header.m_graph_guid, stream);
	stream.w_u32(m_spawn_header.m_spawn_count);
	stream.w_u32(m_spawn_header.m_level_count);
	stream.close_chunk();

	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		stream.open_chunk(SpawnFileChunks::SpawnGraphNew);
		auto& graph = spawn_graph();

		stream.open_chunk(GraphAbstractChunks::VerticesNum);
		stream.w_u32(graph.vertex_count());
		stream.close_chunk();

		stream.open_chunk(GraphAbstractChunks::VerticesData);
		auto I = graph.vertices().begin();
		auto E = graph.vertices().end();
		SSaveTask dummy;
		for (int i=0; I != E; ++I)
		{
			stream.open_chunk(i);
			{
				stream.open_chunk(GraphAbstractVertexChunks::ID);
				save_data(I->second->vertex_id(),stream);
				stream.close_chunk();

				stream.open_chunk(GraphAbstractVertexChunks::Data);

				auto& obj = I->second->data()->object();
				auto SaveObjPtr = CSaveManager::GetInstance().EditorBeginSave();
				auto& SaveObj = *SaveObjPtr;
				shared_str temp = obj.name();
				SaveObj << temp;
				obj.Spawn_Serialize(SaveObj, true);
				obj.UPDATE_Serialize(SaveObj);
				CMemoryBuffer buff;
				buff.Write(ESaveVariableType::t_chunk);
				SaveObj.Write(&buff, &dummy);
				buff.Write((IWriter*)(&stream));
				xr_delete(SaveObjPtr);
				
				stream.close_chunk();
			}
			stream.close_chunk();
		}
		stream.close_chunk();

		stream.open_chunk(GraphAbstractChunks::Edges);
		{
			for (auto& Vertex : graph.vertices())
			{
				if (Vertex.second->edges().empty())
				{
					continue;
				}
				save_data(Vertex.second->vertex_id(),stream);

				stream.w_u32(Vertex.second->edges().size());
				for (auto& Edge : Vertex.second->edges())
				{
					save_data(Edge.vertex_id(),stream);
					save_data(Edge.weight(),stream);
				}
			}
		}
		stream.close_chunk();
		
		stream.close_chunk();
		
	} else
	{
		stream.open_chunk(SpawnFileChunks::SpawnGraphOld);
		save_data						(spawn_graph(),stream);
		stream.close_chunk				();
	}

	stream.open_chunk(2);
	save_data(m_level_points, stream);
	stream.close_chunk();

	stream.open_chunk(3);
	save_data(m_patrol_path_storage, stream);
	stream.close_chunk();
	return true;
}

shared_str CGameSpawnConstructor::spawn_name	(const char* output)
{
	string_path					file_name;
	if (!output)
		FS.update_path			(file_name,"$game_spawn$",*actor_level_name());
	else {
		actor_level_name		();
		string_path				out;
		xr_strcpy				(out,output);
		xr_strcat				(out,".spawn");
		FS.update_path			(file_name,"$game_spawn$",out);
	}
	return						(file_name);
}

void CGameSpawnConstructor::add_story_object	(ALife::_STORY_ID id, CSE_ALifeDynamicObject *object, const char* level_name)
{
	if (id == INVALID_STORY_ID)
		return;

	auto		I = m_story_objects.find(id);
	if (I != m_story_objects.end()) {
		Msg						("Object %s, story id %d",object->name_replace(), object->m_story_id);
		Msg						("Object %s, story id %d",(*I).second->name_replace(),(*I).second->m_story_id);
		VERIFY3					(I == m_story_objects.end(),"There are several objects which has the same unique story ID, level ",level_name);
	}
	
	m_story_objects.insert		(std::make_pair(id,object));
}

void CGameSpawnConstructor::add_object				(CSE_Abstract *object)
{
 	object->m_tSpawnID			= spawn_id();
	spawn_graph().add_vertex	(new CServerEntityWrapper(object),object->m_tSpawnID);
}

void CGameSpawnConstructor::remove_object			(CSE_Abstract *object)
{
	spawn_graph().remove_vertex	(object->m_tSpawnID);
}

bool CGameSpawnConstructor::process_actor			(const char* start_level_name)
{
	m_actor							= 0;
	
	LEVEL_SPAWN_STORAGE::iterator	I = m_level_spawns.begin();
	LEVEL_SPAWN_STORAGE::iterator	E = m_level_spawns.end();
	for ( ; I != E; ++I) {
		if (!(*I)->actor())
			continue;

		Msg							("Actor is on the level %s",*game_graph().header().level(game_graph().vertex((*I)->actor()->m_tGraphID)->level_id()).name());
		
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

	return true;
}

CGameSpawnConstructor::CGameSpawnConstructor()
{
}

bool CGameSpawnConstructor::build(const char* name, const char* output, const char* start, bool no_separator_check)
{

	string_path spawn_src;
	string_path spawn_dst;

	xr_sprintf(spawn_src, "%s.spawn", name);
	FS.update_path(spawn_src, "$game_spawn$", spawn_src);
	FS.update_path(spawn_dst, "$game_spawn$", "editor.spawn");

	if (FS.exist(spawn_src))
	{
		Msg("PIE: use existing spawn: %s", spawn_src);

		if (FS.exist(spawn_dst))
			FS.file_delete(spawn_dst);

		FS.file_copy(spawn_src, spawn_dst);
		return true;
	}

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

bool CGameSpawnConstructor::build(const char* name, CMemoryWriter& output, const char* start, bool no_separator_check)
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
		if (DeleteFileA(**I))
			Msg		("file %s is successfully deleted",**I);
		else
			Msg		("cannot delete file %s",**I);
	}
}