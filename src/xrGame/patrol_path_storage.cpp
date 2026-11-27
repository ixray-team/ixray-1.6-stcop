////////////////////////////////////////////////////////////////////////////
//	Module 		: patrol_path_storage.cpp
//	Created 	: 15.06.2004
//  Modified 	: 15.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Patrol path storage
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "patrol_path_storage.h"
#include "patrol_path.h"
#include "patrol_point.h"
#include "LevelGameDef.h"
#include "ai_space.h"
#include "level_graph.h"
#include "game_graph.h"

CPatrolPathStorage::~CPatrolPathStorage()
{
}

void CPatrolPathStorage::unload()
{
	++RevisionValue;
	m_registry.clear();
}

void CPatrolPathStorage::load_raw(const ILevelGraph* level_graph, const IGameLevelCrossTable* cross, const IGameGraph* game_graph, IReader& stream)
{
	unload();
	IReader* chunk = stream.open_chunk(WAY_PATROLPATH_CHUNK);
	if (!chunk)
		return;

	u32 chunkIterator;
	for (IReader* subChunk = chunk->open_chunk_iterator(chunkIterator); subChunk; subChunk = chunk->open_chunk_iterator(chunkIterator, subChunk))
	{
		R_ASSERT(subChunk->find_chunk(WAYOBJECT_CHUNK_VERSION));
		R_ASSERT(subChunk->r_u16() == WAYOBJECT_VERSION);
		R_ASSERT(subChunk->find_chunk(WAYOBJECT_CHUNK_NAME));
		shared_str patrolName;
		subChunk->r_stringZ(patrolName);
		VERIFY3(m_registry.find(patrolName) == m_registry.end(), "Duplicated patrol path found", *patrolName);
		xr_shared_ptr<CPatrolPath> path = xr_make_shared<CPatrolPath>(patrolName);
		path->load_raw(level_graph, cross, game_graph, *subChunk);
		m_registry.insert(std::make_pair(patrolName, std::move(path)));
	}
	chunk->close();
}

void CPatrolPathStorage::load(IReader& stream)
{
	unload();
	xr_unique_ptr<IReader> header(stream.open_chunk(0));
	if (!header)
	{
		Msg("! CPatrolPathStorage::load: count chunk is missing, storage stays empty");
		return;
	}
	const u32 size = header->r_u32();
	xr_unique_ptr<IReader> records(stream.open_chunk(1));
	if (!records)
	{
		Msg("! CPatrolPathStorage::load: data chunk is missing, storage stays empty");
		return;
	}

	xr_vector<PATROL_REGISTRY::value_type> entries;
	xr_vector<xr_unique_ptr<CPatrolPath>> paths;
	xr_hash_set<shared_str> keys;
	entries.reserve(size);
	paths.reserve(size);
	keys.reserve(size);
	for (u32 index = 0; index < size; ++index)
	{
		xr_unique_ptr<IReader> record(records->open_chunk(index));
		if (!record)
		{
			Msg("! CPatrolPathStorage::load: record %u is missing, storage load stopped", index);
			return;
		}
		xr_unique_ptr<IReader> name(record->open_chunk(0));
		xr_unique_ptr<IReader> graph(record->open_chunk(1));
		if (!name || !graph)
		{
			Msg("! CPatrolPathStorage::load: record %u has incomplete data, storage load stopped", index);
			return;
		}
		shared_str key;
		load_data(key, *name);
		auto path = xr_make_unique<CPatrolPath>();
		load_data(*path, *graph);
		if (!keys.insert(key).second)
		{
			Msg("! CPatrolPathStorage::load: duplicated patrol path '%s' was skipped", key.c_str());
			continue;
		}
#ifdef DEBUG
		path->name(key);
#endif
		entries.emplace_back(key, path.get());
		paths.push_back(std::move(path));
	}

	PATROL_REGISTRY registry(entries.begin(), entries.end());
	m_registry.swap(registry);
	for (auto& path : paths)
		path.release();
}

void CPatrolPathStorage::save(IWriter& stream)
{
	stream.open_chunk(0);
	stream.w_u32(m_registry.size());
	stream.close_chunk();
	stream.open_chunk(1);
	PATROL_REGISTRY::iterator I = m_registry.begin();
	PATROL_REGISTRY::iterator E = m_registry.end();
	for (int i = 0; I != E; ++I, ++i)
	{
		stream.open_chunk(i);
		stream.open_chunk(0);
		save_data((*I).first, stream);
		stream.close_chunk();
		stream.open_chunk(1);
		save_data(const_cast<CPatrolPath*>((*I).second.get()), stream);
		stream.close_chunk();
		stream.close_chunk();
	}
	stream.close_chunk();
}

void CPatrolPathStorage::remove_path(shared_str patrolName)
{
	++RevisionValue;
	m_registry.erase(patrolName);
}

void CPatrolPathStorage::add_path(shared_str patrolName, CPatrolPath* path)
{
	remove_path(patrolName);
	++RevisionValue;
	m_registry.insert(std::make_pair(patrolName, xr_shared_ptr<const CPatrolPath>(path)));
}
