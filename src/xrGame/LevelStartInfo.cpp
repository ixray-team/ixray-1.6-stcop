#include "StdAfx.h"
#include "LevelStartInfo.h"
#include "saved_game_wrapper.h"
#include "game_graph.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "xrMessages.h"

extern int g_start_game_vertex_id;

namespace
{
	bool FindActorGraphVertex(IReader& Spawn, GameGraph::_GRAPH_ID& Vertex)
	{
		IReader* Graph = Spawn.open_chunk(1);
		if (Graph == nullptr)
		{
			return false;
		}

		bool IsFound = false;
		if (IReader* Vertices = Graph->open_chunk(1))
		{
			const CLASS_ID ActorClass = TEXT2CLSID("S_ACTOR");
			u32 Id = 0;
			for (IReader* VertexChunk = Vertices->open_chunk_iterator(Id); VertexChunk != nullptr; VertexChunk = Vertices->open_chunk_iterator(Id, VertexChunk))
			{
				IReader* Data = VertexChunk->open_chunk(1);
				IReader* SpawnData = Data ? Data->open_chunk(0) : nullptr;
				if (SpawnData != nullptr)
				{
					NET_Packet Packet;
					Packet.B.data.resize(SpawnData->r_u16());
					SpawnData->r(Packet.B.data.data(), u32(Packet.B.data.size()));
					SpawnData->close();

					u16 MessageId = 0;
					Packet.r_begin(MessageId);
					string64 Section;
					Packet.r_stringZ(Section);
					if (MessageId == M_SPAWN && pSettings->line_exist(Section, "class") && pSettings->r_clsid(Section, "class") == ActorClass)
					{
						if (CSE_Abstract* Object = F_entity_Create(Section))
						{
							Object->Spawn_Read(Packet);
							if (CSE_ALifeCreatureActor* Actor = smart_cast<CSE_ALifeCreatureActor*>(Object))
							{
								Vertex = Actor->m_tGraphID;
								IsFound = true;
							}
							F_entity_Destroy(Object);
						}
					}
				}

				if (Data != nullptr)
				{
					Data->close();
				}

				if (IsFound)
				{
					VertexChunk->close();
					break;
				}
			}
			Vertices->close();
		}
		Graph->close();
		return IsFound;
	}

	bool FindLevelName(IReader& Spawn, GameGraph::_GRAPH_ID Vertex, shared_str& LevelName)
	{
		IReader* Chunk = Spawn.open_chunk(4);
		const bool IsGraphFile = (Chunk == nullptr);
		if (IsGraphFile)
		{
			string_path GraphPath;
			FS.update_path(GraphPath, "$game_data$", GRAPH_NAME);
			Chunk = FS.r_open(GraphPath);
		}

		if (Chunk == nullptr)
		{
			return false;
		}

		bool IsFound = false;
		{
			CGameGraph Graph(*Chunk);
			if (Vertex < Graph.header().vertex_count())
			{
				const GameGraph::_LEVEL_ID Level = Graph.vertex(Vertex)->level_id();
				if (Graph.header().level_exist(Level))
				{
					LevelName = Graph.header().level(Level).name();
					IsFound = true;
				}
			}
		}

		if (IsGraphFile)
		{
			FS.r_close(Chunk);
		}
		else
		{
			Chunk->close();
		}
		return IsFound;
	}
}

bool LevelStartInfo::Find(const IGame_Persistent::params& Params, shared_str& LevelName)
{
	PROF_EVENT("LevelStartInfo::Find");
	if (Device.IsEditorMode() || g_start_game_vertex_id != 0)
	{
		return false;
	}

	if (0 != xr_strcmp(Params.m_game_type, "single") || 0 != xr_strcmp(Params.m_alife, "alife"))
	{
		return false;
	}

	const bool IsTransition = !Params.m_new_or_load[0] && CSavedGameWrapper::valid_saved_game(Params.m_game_or_spawn);
	if (0 == xr_strcmp(Params.m_new_or_load, "load") || IsTransition)
	{
		if (!CSavedGameWrapper::valid_saved_game(Params.m_game_or_spawn))
		{
			return false;
		}

		CSavedGameWrapper Save(Params.m_game_or_spawn);
		if (Save.level_id() == CSavedGameWrapper::_LEVEL_ID(-1) || !Save.level_name() || !Save.level_name()[0])
		{
			return false;
		}
		LevelName = Save.level_name();
		return true;
	}

	if (0 != xr_strcmp(Params.m_new_or_load, "new") || EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		return false;
	}

	string_path SpawnPath;
	if (!FS.exist(SpawnPath, "$game_spawn$", Params.m_game_or_spawn, ".spawn"))
	{
		return false;
	}

	IReader* Spawn = FS.r_open(SpawnPath);
	if (Spawn == nullptr)
	{
		return false;
	}

	GameGraph::_GRAPH_ID Vertex = 0;
	const bool IsFound = FindActorGraphVertex(*Spawn, Vertex) && FindLevelName(*Spawn, Vertex, LevelName);
	FS.r_close(Spawn);
	return IsFound;
}
