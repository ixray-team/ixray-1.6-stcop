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

CPatrolPathStorage::~CPatrolPathStorage		()
{
	delete_data					(m_registry);
}

void CPatrolPathStorage::unload()
{
	for (auto& [key, value] : m_registry)
	{
		xr_delete(value);
	}
	m_registry.clear();
}

void CPatrolPathStorage::load_raw			(const ILevelGraph *level_graph, const IGameLevelCrossTable *cross, const IGameGraph *game_graph, IReader &stream)
{
	IReader						*chunk = stream.open_chunk(WAY_PATROLPATH_CHUNK);

	if (!chunk)
		return;
		
	u32 chunk_iterator;
	for (IReader *sub_chunk = chunk->open_chunk_iterator(chunk_iterator); sub_chunk; sub_chunk = chunk->open_chunk_iterator(chunk_iterator,sub_chunk)) {
		R_ASSERT				(sub_chunk->find_chunk(WAYOBJECT_CHUNK_VERSION));
		R_ASSERT				(sub_chunk->r_u16() == WAYOBJECT_VERSION);
		R_ASSERT				(sub_chunk->find_chunk(WAYOBJECT_CHUNK_NAME));

		shared_str				patrol_name;
		sub_chunk->r_stringZ	(patrol_name);
		const_iterator			I = m_registry.find(patrol_name);
		VERIFY3					(I == m_registry.end(),"Duplicated patrol path found",*patrol_name);
		m_registry.insert		(
			std::make_pair(
				patrol_name,
				&(new CPatrolPath(patrol_name))->load_raw
				(
					level_graph,
					cross,
					game_graph,
					*sub_chunk
				)
			)
		);
	}
	
	chunk->close();
}

void CPatrolPathStorage::load(IReader& stream)
{
    xr_unique_ptr<IReader> header(stream.open_chunk(0));
    const u32 size = header->r_u32();
    unload();

    xr_vector<PATROL_REGISTRY::value_type> entries;
    xr_vector<xr_unique_ptr<CPatrolPath>> paths;
    xr_hash_set<shared_str> keys;
    entries.reserve(size);
    paths.reserve(size);
    keys.reserve(size);
    xr_unique_ptr<IReader> records(stream.open_chunk(1));
    for (u32 index = 0; index < size; ++index)
    {
        xr_unique_ptr<IReader> record(records->open_chunk(index));
        xr_unique_ptr<IReader> name(record->open_chunk(0));
        shared_str key;
        load_data(key, *name);
        xr_unique_ptr<IReader> graph(record->open_chunk(1));
        auto path = xr_make_unique<CPatrolPath>();
        load_data(*path, *graph);
        const bool inserted = keys.insert(key).second;
        VERIFY3(inserted, "Duplicated patrol path found ", key.c_str());
        if (!inserted) continue;
#ifdef DEBUG
        path->name(key);
#endif
        entries.emplace_back(key, path.get());
        paths.push_back(std::move(path));
    }

    PATROL_REGISTRY registry(entries.begin(), entries.end());
    m_registry.swap(registry);
    for (auto& path : paths) path.release();
}

void CPatrolPathStorage::save				(IWriter &stream)
{
	stream.open_chunk			(0);
	stream.w_u32				(m_registry.size());
	stream.close_chunk			();

	stream.open_chunk			(1);

	PATROL_REGISTRY::iterator	I = m_registry.begin();
	PATROL_REGISTRY::iterator	E = m_registry.end();
	for (int i=0; I != E; ++I, ++i) {
		stream.open_chunk		(i);

		stream.open_chunk		(0);
        save_data				((*I).first,stream);
		stream.close_chunk		();

		stream.open_chunk		(1);
        save_data				((*I).second,stream);
		stream.close_chunk		();

		stream.close_chunk		();
	}

	stream.close_chunk			();
}
void CPatrolPathStorage::remove_path(shared_str patrol_name) 
{
	m_registry.erase(patrol_name);
}


void CPatrolPathStorage::add_path(shared_str patrol_name, CPatrolPath *path) 
{
	remove_path(patrol_name);
	m_registry.insert(std::make_pair(patrol_name, path));
}
