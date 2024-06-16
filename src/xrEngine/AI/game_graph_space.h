////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph_space.h
//	Created 	: 18.02.2003
//  Modified 	: 11.12.2004
//	Author		: Dmitriy Iassenev
//	Description : Game graph namespace
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrCore/associative_vector.h"
#include "../xrLevel.h"

namespace GameGraph 
{
	typedef u16	_GRAPH_ID;
	typedef u8	_LEVEL_ID;
	typedef u8	_LOCATION_ID;

	enum {
		LOCATION_TYPE_COUNT = 4,
		LOCATION_COUNT		= (u32(1) << (8*sizeof(_LOCATION_ID))),
	};


	class SLevel
	{
#ifndef MASTER_GOLD
	public:
#endif
		shared_str				m_name;
		Fvector					m_offset;
		_LEVEL_ID				m_id;
		shared_str				m_section;
		xrGUID					m_guid;

	public:
		IC const shared_str &name		() const
		{
			return				(m_name);
		}

		IC const Fvector &offset		() const
		{
			return				(m_offset);
		}

		IC const _LEVEL_ID &id			() const
		{
			return				(m_id);
		}

		IC const shared_str &section	() const
		{
			return				(m_section);
		}

		IC const xrGUID &guid			() const
		{
			return				(m_guid);
		}

		IC void load					(IReader *reader);
		IC void save					(IWriter *writer);

		friend class CGameGraph;
	};

	typedef associative_vector<_LEVEL_ID,SLevel>		LEVEL_MAP;

#pragma pack(push,1)
	class CEdge
	{
#ifndef MASTER_GOLD
	public:
#endif
		_GRAPH_ID					m_vertex_id;
		float						m_path_distance;
	public:
		IC	const _GRAPH_ID& vertex_id() const;
		IC	const float& distance() const;
	};

	class CVertex 
	{
#ifndef MASTER_GOLD
	public:
#endif
		Fvector						tLocalPoint;
		Fvector						tGlobalPoint;
		u32							tLevelID:8;
		u32							tNodeID:24;
		u8							tVertexTypes[LOCATION_TYPE_COUNT];
		u32							dwEdgeOffset;
		u32							dwPointOffset;
		u8							tNeighbourCount;
		u8							tDeathPointCount;
	public:
		IC	const Fvector			&level_point		() const;
		IC	const Fvector			&game_point			() const;
		IC	_LEVEL_ID				level_id			() const;
		IC	u32						level_vertex_id		() const;
		IC	const u8				*vertex_type		() const;
		IC	const u8				&edge_count			() const;
		IC	const u32				&edge_offset		() const;
		IC	const u8				&death_point_count	() const;
		IC	const u32				&death_point_offset	() const;
		friend class CGameGraph;
	};


	class CHeader 
	{
#ifndef MASTER_GOLD
	public:
#endif
		u8							m_version;
		_GRAPH_ID					m_vertex_count;
		u32							m_edge_count;
		u32							m_death_point_count;
		xrGUID						m_guid;
		LEVEL_MAP					m_levels;

	public:
		IC	const u8				&version			() const;
		IC	_LEVEL_ID				level_count			() const;
		IC	const _GRAPH_ID			&vertex_count		() const;
		IC	const u32				&edge_count			() const;
		IC	const u32				&death_point_count	() const;
		IC	const xrGUID			&guid				() const;
		IC	const LEVEL_MAP			&levels				() const;
		IC	const SLevel			&level				(const _LEVEL_ID &id) const;
		IC	const SLevel			&level				(LPCSTR level_name) const;
		IC	const SLevel			*level				(LPCSTR level_name, bool) const;
		IC	void					load				(IReader *reader);
		IC	void					save				(IWriter *reader);
		
		IC bool level_exist(const _LEVEL_ID& id) const;
		IC bool level_exist(pcstr level_name) const;
		friend class CGameGraph;
	};
#pragma pack(pop)

	class CLevelPoint
	{
	#ifndef MASTER_GOLD
	public:
	#endif
		Fvector		tPoint;
		u32			tNodeID;
		float		fDistance;	
	public:
		IC const Fvector			&level_point		() const
		{
			return				(tPoint);
		}

		IC u32						level_vertex_id		() const
		{
			return				(tNodeID);
		}

		IC float					distance			() const
		{
			return				(fDistance);
		}
	};

	struct STerrainPlace{
		svector<_LOCATION_ID,LOCATION_TYPE_COUNT>	tMask;
	};

	using TERRAIN_VECTOR = xr_vector<STerrainPlace>;
	using TERRAIN_IT = TERRAIN_VECTOR::iterator;
}