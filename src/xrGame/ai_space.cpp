////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_space.h
//	Created 	: 12.11.2003
//  Modified 	: 12.11.2003
//	Author		: Dmitriy Iassenev
//	Description : AI space class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "game_graph.h"
#include "level_graph.h"
#include "ImUtils/AIGridEditor.h"
#include "graph_engine.h"
#include "ef_storage.h"
#include "ai_space.h"
#include "cover_manager.h"
#include "cover_point.h"
#include "../xrScripts/script_engine.h"
#include "../xrEngine/IGame_Persistent.h"
#include "patrol_path_storage.h"
#include "alife_simulator.h"
#include "moving_objects.h"
#include "stalker_animation_data_storage.h"
#include "doors_manager.h"
#include "../../xrCore/AnimNotify/AnimNotify.h"
#include "AnimNotify/AnimNotifyGame.h"
#include "../xrCore/AnimNotify/AnimNotifyRegistry.h"
#include "../xrServerEntities/object_factory.h"
CAI_Space *g_ai_space = 0;

namespace
{
	class CAILevelPreload final
	{
	public:
		~CAILevelPreload() { Drop(); }

		void Start(const shared_str& Name)
		{
			Drop();
			LevelName = Name;
			Task.run([this]()
			{
				PROF_EVENT("AI Level Preload");
				Graph.reset(new CLevelGraph());
				if (!HasAIGridEditorPatch())
				{
					Covers.reset(new CCoverManager());
					Covers->compute_static_cover(*Graph);
				}
			});
		}

		bool Take(const char* Name, xr_unique_ptr<CLevelGraph>& OutGraph, xr_unique_ptr<CCoverManager>& OutCovers)
		{
			Task.wait();
			const bool IsSame = Graph != nullptr && 0 == xr_strcmp(LevelName.c_str(), Name);
			if (IsSame)
			{
				OutGraph = std::move(Graph);
				OutCovers = std::move(Covers);
			}
			Reset();
			return IsSame;
		}

		void Drop()
		{
			Task.wait();
			Reset();
		}

	private:
		void Reset()
		{
			Graph.reset();
			Covers.reset();
			LevelName = nullptr;
		}

		xr_task_group Task;
		shared_str LevelName;
		xr_unique_ptr<CLevelGraph> Graph;
		xr_unique_ptr<CCoverManager> Covers;
	};

	CAILevelPreload LevelPreload;
}

CAI_Space::CAI_Space				()
{
	m_ef_storage			= 0;
	m_game_graph			= 0;
	m_cover_manager			= 0;
	m_level_graph			= 0;
	m_alife_simulator		= 0;
	m_patrol_path_storage	= 0;
	m_moving_objects		= 0;
	m_doors_manager			= 0;
	m_script_engine			= nullptr;
	m_owns_script_engine	= false;
	m_empty_cross_table.m_tpaCrossTable = nullptr;
	m_empty_cross_table.m_tCrossTableHeader.dwNodeCount = 0;
	m_empty_cross_table.m_tCrossTableHeader.dwGraphPointCount = 0;
}

void CAI_Space::init				(bool dedicatedScriptEngine)
{
#ifndef NO_SINGLE
	VERIFY					(!m_ef_storage);
	m_ef_storage			= new CEF_Storage();

	VERIFY					(!m_cover_manager);
	m_cover_manager			= new CCoverManager();

	VERIFY					(!m_patrol_path_storage);
	m_patrol_path_storage	= new CPatrolPathStorage();

	VERIFY					(!m_moving_objects);
	m_moving_objects		= new ::moving_objects();

	VERIFY(!AnimationData);
	AnimationData = new CStalkerAnimationDataStorage();

#endif //#ifndef NO_SINGLE

	if (dedicatedScriptEngine)
	{
		if (!m_owns_script_engine)
		{
			SharedScriptEngine = xr_shared_ptr<CScriptEngine>(new CScriptEngine(), [](CScriptEngine* engine)
			{
				object_factory().release_script_engine(engine);
				xr_delete(engine);
			});
			m_script_engine = SharedScriptEngine.get();
			g_pScriptEngine = m_script_engine;
			object_factory();
			script_engine().init();
			m_owns_script_engine = true;
		}
	}
	else if (SharedScriptEngine != nullptr)
	{
		m_script_engine = SharedScriptEngine.get();
	}
	else if (g_pScriptEngine == nullptr)
	{
		SharedScriptEngine = xr_shared_ptr<CScriptEngine>(new CScriptEngine(), [](CScriptEngine* engine)
		{
			object_factory().release_script_engine(engine);
			xr_delete(engine);
		});
		m_script_engine = SharedScriptEngine.get();
		g_pScriptEngine = m_script_engine;
		script_engine().init();
		m_owns_script_engine = true;
	}
	else
	{
		m_script_engine = g_pScriptEngine;
	}

	if (!IAnimNotifyHandler::IsValid())
		IAnimNotifyHandler::SetHandler(new CAnimNotifyHandler());
	CAnimNotifyRegistry::GetInstance();

#ifndef NO_SINGLE
	extern SCRIPTS_API string4096 g_ca_stdout;
	setvbuf					(stderr,g_ca_stdout,_IOFBF,sizeof(g_ca_stdout));
#endif //#ifndef NO_SINGLE
}

CAI_Space::~CAI_Space				()
{
	LevelPreload.Drop();
	CScriptEngine* previousScriptEngine = g_pScriptEngine;
	CAI_Space* previousAiSpace = g_ai_space;
	const bool lastScriptEngineOwner = SharedScriptEngine != nullptr && SharedScriptEngine.use_count() == 1;
	const bool releasesScriptEngine = (m_owns_script_engine && m_script_engine != nullptr) || lastScriptEngineOwner;
	CScriptEngine* releasedScriptEngine = releasesScriptEngine ? m_script_engine : nullptr;
	if (releasedScriptEngine != nullptr)
	{
		if (g_pGamePersistent != nullptr && g_pGamePersistent->FrameworkScriptEngine == releasedScriptEngine)
		{
			g_pGamePersistent->ixr_framework_onframe.reset();
			g_pGamePersistent->FrameworkScriptEngine = nullptr;
		}
		g_pScriptEngine = releasedScriptEngine;
		g_ai_space = this;
	}

	unload();
	SharedScriptEngine.reset();
	m_script_engine = nullptr;
	m_owns_script_engine = false;

	if (releasesScriptEngine)
	{
		g_pScriptEngine = previousScriptEngine == releasedScriptEngine ? nullptr : previousScriptEngine;
		g_ai_space = previousAiSpace == this ? nullptr : previousAiSpace;
	}

	xr_delete				(m_doors_manager);
	xr_delete				(m_moving_objects);
	xr_delete				(m_patrol_path_storage);
	xr_delete				(m_cover_manager);
	xr_delete				(m_ef_storage);
	xr_delete				(AnimationData);
	xr_delete				(EmptyLevelGraph);
	if (m_alife_simulator == nullptr || m_alife_borrowed)
		xr_delete(m_game_graph);
	VERIFY					(!m_game_graph);
}

doors::manager& CAI_Space::doors() const
{
	// Lazily created: the AI-space load builds it, but that load only runs for
	// levels with an AI-map; degraded/sandbox levels may never have one while
	// monsters still reach the doors manager through the movement code.
	if (m_doors_manager == nullptr)
		m_doors_manager = new doors::manager(level_graph().header().box());
	return					(*m_doors_manager);
}

void CAI_Space::StartLevelPreload(const shared_str& LevelName)
{
	LevelPreload.Start(LevelName);
}

void CAI_Space::DropLevelPreload()
{
	LevelPreload.Drop();
}

void CAI_Space::load				(const char* level_name)
{
	PROF_EVENT(__FUNCTION__);
	unload					(true);

#ifdef DEBUG
	Memory.mem_compact		();
	//u32						mem_usage = Memory.mem_usage();
	CTimer					timer;
	timer.Start				();
#endif

	xr_unique_ptr<CCoverManager> PreloadedCovers;
	if (get_game_graph())
	{
		const IGameGraph::SLevel* currentLevel = game_graph().header().level(level_name, true);
		if (currentLevel != nullptr)
			game_graph().set_current_level(currentLevel->id());
		else
			Msg("! CAI_Space::load: level '%s' is absent from the game graph; loading its AI-map without graph binding", level_name);

		xr_unique_ptr<CLevelGraph> PreloadedGraph;
		if (g_pGameLevel == nullptr || !g_pGameLevel->IsParallel())
			LevelPreload.Take(level_name, PreloadedGraph, PreloadedCovers);

#ifndef MASTER_GOLD
		if(Device.IsEditorMode())
			m_level_graph = EditorScene->GetLevelGraph();
		else
#endif
		m_level_graph = PreloadedGraph ? PreloadedGraph.release() : new CLevelGraph();
		if (level_graph().has_nodes() && currentLevel != nullptr && get_cross_table() != nullptr)
		{
			if (cross_table().header().level_guid() != level_graph().header().guid() ||
				cross_table().header().game_guid() != game_graph().header().guid() ||
				currentLevel->guid() != level_graph().header().guid())
			{
				Msg("! CAI_Space::load: graph/cross-table/AI-map identity mismatch for '%s'; disabling graph binding", level_name);
				m_cross_table_enabled = false;
			}
			else
			{
				LoadAIGridEditorPatch();
#ifdef DEBUG
				if (!xr_strcmp(currentLevel->name(), level_name))
					validate(currentLevel->id());
#endif
				level_graph().level_id(currentLevel->id());
			}
		}
		else if (!level_graph().has_nodes())
		{
			Msg("! CAI_Space::load: level graph for '%s' is empty, skipping game graph binding", level_name);
		}
	}
	else
	{
		// No game graph (degraded/sandbox load): the AI-map stands alone.
#ifndef MASTER_GOLD
		if(Device.IsEditorMode())
			m_level_graph = EditorScene->GetLevelGraph();
		else
#endif
		{
			Msg("! CAI_Space::load: no game graph present, loading AI-map without graph binding");
			m_level_graph = new CLevelGraph();
		}
	}

	if (ai().get_level_graph())
	{
		Msg("* CAI_Space::load: graph loaded, nodes=%u; computing covers", ai().level_graph().header().vertex_count());
		if (PreloadedCovers)
			m_cover_manager->TakeStaticCover(*PreloadedCovers);
		else
			m_cover_manager->compute_static_cover();
		Msg("* CAI_Space::load: covers done");
		m_moving_objects->on_level_load			();

		xr_delete				(m_doors_manager);
		m_doors_manager			= new ::doors::manager( ai().level_graph().header().box() );
		Msg("* CAI_Space::load: doors manager created");
	}

#ifdef DEBUG
	//Msg						("* Loading ai space is successfully completed (%.3fs, %7.3f Mb)",timer.GetElapsed_sec(),float(Memory.mem_usage() - mem_usage)/1048576.0);
#endif
}

void CAI_Space::unload_level_graph()
{
	xr_delete(m_doors_manager);
	if (!Device.IsEditorMode())
		xr_delete(m_level_graph);
	m_cover_manager->clear(true);
}

void CAI_Space::unload				(bool reload)
{
	// Shared engines retain their per-level script state.
	if (m_owns_script_engine)
		script_engine().unload	();

	xr_delete				(m_doors_manager);

	if(!Device.IsEditorMode()) xr_delete(m_level_graph);

	m_cover_manager->clear(reload);
	if (!reload)
	{
		m_patrol_path_storage->unload();
	}
}

#ifdef DEBUG
void CAI_Space::validate			(const u32 level_id) const
{
	VERIFY					(level_graph().header().vertex_count() == cross_table().header().level_vertex_count());
	for (GameGraph::_GRAPH_ID i=0, n = game_graph().header().vertex_count(); i<n; ++i)
		if ((level_id == game_graph().vertex(i)->level_id()) && 
			(!level_graph().valid_vertex_id(game_graph().vertex(i)->level_vertex_id()) ||
			(cross_table().vertex(game_graph().vertex(i)->level_vertex_id()).game_vertex_id() != i) ||
			!level_graph().inside(game_graph().vertex(i)->level_vertex_id(),game_graph().vertex(i)->level_point()))) {
			Msg				("! Graph doesn't correspond to the cross table");
			R_ASSERT2		(false,"Graph doesn't correspond to the cross table");
		}

//	Msg						("death graph point id : %d",cross_table().vertex(455236).game_vertex_id());

	for (u32 i=0, n=game_graph().header().vertex_count(); i<n; ++i) {
		if (level_id != game_graph().vertex(i)->level_id())
			continue;

		CGameGraph::const_spawn_iterator	I, E;
		game_graph().begin_spawn			(i,I,E);
//		Msg									("vertex [%d] has %d death points",i,game_graph().vertex(i)->death_point_count());
		for ( ; I != E; ++I) {
			VERIFY							(cross_table().vertex((*I).level_vertex_id()).game_vertex_id() == i);
		}
	}
	

//	Msg						("* Graph corresponds to the cross table");
}
#endif

void CAI_Space::patrol_path_storage_raw	(IReader &stream)
{
	VERIFY(m_patrol_path_storage);
	m_patrol_path_storage->load_raw	(get_level_graph(),get_cross_table(),get_game_graph(),stream);
}

void CAI_Space::patrol_path_storage		(IReader &stream)
{
	VERIFY(m_patrol_path_storage);
	m_patrol_path_storage->load		(stream);
}

void CAI_Space::patrol_path_storage(CPatrolPathStorage* Storage)
{
	if (m_patrol_path_storage != Storage)
	{
		xr_delete(m_patrol_path_storage);
		m_patrol_path_storage = Storage;
	}
	if (m_patrol_path_storage == nullptr)
		m_patrol_path_storage = new CPatrolPathStorage();
}

void CAI_Space::patrol_path_storage_from_editor()
{
	if (g_dedicated_server)
		return;

	VERIFY(m_patrol_path_storage);
	m_patrol_path_storage->unload();
}

void CAI_Space::set_alife				(CALifeSimulator *alife_simulator)
{
	VERIFY					((!m_alife_simulator && alife_simulator) || (m_alife_simulator && !alife_simulator));
	m_alife_simulator		= alife_simulator;
	m_alife_borrowed = false;

	VERIFY					(!alife_simulator || !m_game_graph);
	if (alife_simulator)
		return;

	VERIFY					(m_game_graph);
	m_game_graph			= 0;
}

void CAI_Space::SetAlifeBorrowed(CALifeSimulator* simulator)
{
	if (m_alife_simulator == simulator)
		return;
	if (simulator == nullptr && m_alife_borrowed)
	{
		m_alife_simulator = nullptr;
		m_alife_borrowed = false;
		return;
	}
	set_alife(simulator);
	m_alife_borrowed = simulator != nullptr;
}

void CAI_Space::game_graph				(IGameGraph *game_graph)
{
	VERIFY(game_graph);
	VERIFY(!m_game_graph);
	m_game_graph = game_graph;
	m_game_graph_enabled = true;
	GameGraphData.reset();
	GameGraphBytes.clear();
}

void CAI_Space::SetGameGraphEnabled(bool enabled)
{
	m_game_graph_enabled = enabled;
}

void CAI_Space::destroy_game_graph()
{
	// Pavel: фикс краша при перезаходе клиентом в мп
	xr_delete(m_game_graph);
	GameGraphData.reset();
	GameGraphBytes.clear();
}

bool CAI_Space::LoadGameGraph(IReader& stream)
{
	if ((m_alife_simulator != nullptr && !m_alife_borrowed) || stream.elapsed() < sizeof(u8))
		return false;
	const u8 version = *static_cast<const u8*>(stream.pointer());
	if (!CHECK_SPAWN_VERSION(version))
	{
		Msg("! CAI_Space::LoadGameGraph: unsupported graph version %u", version);
		return false;
	}
	stream.rewind();
	const intptr_t size = stream.elapsed();
	xr_delete(m_game_graph);
	GameGraphData.reset();
	GameGraphBytes.clear();
	GameGraphBytes.resize(size);
	CopyMemory(GameGraphBytes.data(), stream.pointer(), size);
	GameGraphData.reset(new IReader(GameGraphBytes.data(), size, 0));
	m_game_graph = new CGameGraph(*GameGraphData);
	m_game_graph_enabled = true;
	m_cross_table_enabled = true;
	return true;
}

bool CAI_Space::BindCurrentLevel(const char* levelName)
{
	if (get_game_graph() == nullptr)
		return false;
	const IGameGraph::SLevel* level = game_graph().header().level(levelName, true);
	if (level == nullptr)
		return false;
	game_graph().set_current_level(level->id());
	return get_cross_table() != nullptr;
}

void CAI_Space::UnloadGameGraph()
{
	m_game_graph_enabled = false;
	m_cross_table_enabled = false;
	if (m_alife_simulator == nullptr || m_alife_borrowed)
		xr_delete(m_game_graph);
	GameGraphData.reset();
	GameGraphBytes.clear();
}

const IGameLevelCrossTable &CAI_Space::cross_table() const
{
	if (const IGameLevelCrossTable* table = get_cross_table())
		return *table;
	return m_empty_cross_table;
}

const IGameLevelCrossTable *CAI_Space::get_cross_table() const
{
	if (!m_game_graph_enabled || !m_cross_table_enabled || m_game_graph == nullptr || !m_game_graph->has_cross_table())
		return nullptr;
	return &m_game_graph->cross_table();
}
