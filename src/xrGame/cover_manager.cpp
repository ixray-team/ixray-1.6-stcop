////////////////////////////////////////////////////////////////////////////
//	Module 		: cover_manager.cpp
//	Created 	: 24.03.2004
//  Modified 	: 24.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Cover manager class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "level_graph.h"
#include "cover_manager.h"
#include "ai_space.h"
#include "cover_point.h"
#include "object_broker.h"
#include "smart_cover.h"
#include "smart_cover_loophole.h"
#include "smart_cover_storage.h"
#include "smart_cover_object.h"

#define MIN_COVER_VALUE 16

CCoverManager::CCoverManager				()
{
	m_covers				= 0;
	m_smart_covers_storage	= 0;
	m_smart_covers_actual	= false;
}

CCoverManager::~CCoverManager				()
{
	clear					();
	xr_delete				(m_covers);
	xr_delete				(m_smart_covers_storage);
}

IC	bool CCoverManager::edge_vertex		(const ILevelGraph& Graph, u32 index) const
{
	ILevelGraph::CVertex	*v = Graph.vertex(index);
	return					(
		(!Graph.valid_vertex_id(v->link(0)) && (v->high_cover(0) < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(1)) && (v->high_cover(1) < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(2)) && (v->high_cover(2) < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(3)) && (v->high_cover(3) < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(0)) && (v->low_cover(0)  < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(1)) && (v->low_cover(1)  < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(2)) && (v->low_cover(2)  < MIN_COVER_VALUE)) ||
		(!Graph.valid_vertex_id(v->link(3)) && (v->low_cover(3)  < MIN_COVER_VALUE))
	);
}

IC	bool CCoverManager::cover			(const ILevelGraph& Graph, ILevelGraph::CVertex *v, u32 index0, u32 index1) const
{
	return					(
		Graph.valid_vertex_id(v->link(index0)) &&
		Graph.valid_vertex_id(Graph.vertex(v->link(index0))->link(index1)) &&
		m_temp[Graph.vertex(v->link(index0))->link(index1)]
	);
}

IC	bool CCoverManager::critical_point	(const ILevelGraph& Graph, ILevelGraph::CVertex *v, u32 index, u32 index0, u32 index1) const
{
	return					(
		!Graph.valid_vertex_id(v->link(index)) &&
		(
			!Graph.valid_vertex_id(v->link(index0)) || 
			!Graph.valid_vertex_id(v->link(index1)) ||
			cover(Graph,v,index0,index) || 
			cover(Graph,v,index1,index)
		)
	);
}

IC	bool CCoverManager::critical_cover	(const ILevelGraph& Graph, u32 index) const
{
	ILevelGraph::CVertex	*v = Graph.vertex(index);
	return					(
		critical_point(Graph,v,0,1,3) || 
		critical_point(Graph,v,2,1,3) || 
		critical_point(Graph,v,1,0,2) || 
		critical_point(Graph,v,3,0,2)
	);
}

void CCoverManager::compute_static_cover	()
{
	compute_static_cover	(ai().level_graph());
}

void CCoverManager::compute_static_cover	(const ILevelGraph& Graph)
{
	clear					();
	xr_delete				(m_covers);
	m_covers				= new CPointQuadTree(Graph.header().box(),Graph.header().cell_size()*.5f,8*65536,4*65536);
	m_temp.assign			(Graph.header().vertex_count(), 0);

	const u32 LevelVertexCount = Graph.header().vertex_count();

	xr_parallel_for(0u, LevelVertexCount, [&](u32 range)
	{
		const ILevelGraph::CVertex& vertex = *Graph.vertex(range);
		if (vertex.high_cover(0) + vertex.high_cover(1) + vertex.high_cover(2) + vertex.high_cover(3))
		{
			m_temp[range] = edge_vertex(Graph, range);
			return;
		}

		if (Graph.header().version() != XRAI_SOC_VERSION) // If using SoC spawn, ignore low covers.
		{
			if (vertex.low_cover(0) + vertex.low_cover(1) + vertex.low_cover(2) + vertex.low_cover(3))
			{
				m_temp[range] = edge_vertex(Graph, range);
				return;
			}
		}

		m_temp[range] = 0;
	});

	for (u32 i = 0; i < LevelVertexCount; ++i)
		if (m_temp[i] && critical_cover(Graph, i))
			m_covers->insert(new CCoverPoint(Graph.vertex_position(Graph.vertex(i)),i));

	VERIFY					(!m_smart_covers_storage);
	m_smart_covers_storage	= new smart_cover::storage();
}

void CCoverManager::TakeStaticCover			(CCoverManager& Source)
{
	clear					();
	xr_delete				(m_covers);
	VERIFY					(!m_smart_covers_storage);
	m_covers				= Source.m_covers;
	m_smart_covers_storage	= Source.m_smart_covers_storage;
	Source.m_covers			= nullptr;
	Source.m_smart_covers_storage = nullptr;
	Source.m_temp.clear		();
}

void CCoverManager::clear_covers			(PointVector &covers)
{
	PointVector::iterator	I = covers.begin();
	PointVector::iterator	E = covers.end();
	for ( ; I != E; ++I) {
		if (!(*I)->m_is_smart_cover) {
			xr_delete		(*I);
			continue;
		}

		smart_cover::cover	*cover = (smart_cover::cover*)*I;
		xr_delete			(cover);
	}
	
	covers.clear			();
}

void CCoverManager::RefreshStaticCoverForEditor()
{
	// Keep smart-cover objects/storage and existing cover pointers alive: NPC
	// evaluators can still hold them until their next scheduled update.
	const auto& Graph = ai().level_graph();
	PointVector Previous;
	if (m_covers)
	{
		m_covers->all(Previous);
	}
	xr_hash_map<u32, CCoverPoint*> Existing;
	for (CCoverPoint* Point : EditorRetiredCovers)
	{
		Existing[Point->level_vertex_id()] = Point;
	}
	EditorRetiredCovers.clear();
	xr_delete(m_covers);
	m_covers = new CPointQuadTree(Graph.header().box(), Graph.header().cell_size() * .5f, 8 * 65536, 4 * 65536);
	for (CCoverPoint* Point : Previous)
	{
		if (Point->m_is_smart_cover)
		{
			m_covers->insert(Point);
		}
		else
		{
			Existing[Point->level_vertex_id()] = Point;
		}
	}
	m_temp.resize(Graph.header().vertex_count());
	for (u32 I = 0; I < Graph.header().vertex_count(); ++I)
	{
		const auto& Vertex = *Graph.vertex(I);
		bool HasCover = false;
		for (u8 D = 0; D < 4; ++D)
		{
			HasCover |= Vertex.high_cover(D) != 0 || (Graph.header().version() != XRAI_SOC_VERSION && Vertex.low_cover(D) != 0);
		}
		m_temp[I] = Graph.IsEditorNodeEnabled(I) && HasCover && edge_vertex(Graph, I);
	}
	for (u32 I = 0; I < Graph.header().vertex_count(); ++I)
	{
		if (!m_temp[I] || !critical_cover(Graph, I))
		{
			continue;
		}
		auto Found = Existing.find(I);
		CCoverPoint* Point = Found == Existing.end() ? new CCoverPoint(Graph.vertex_position(I), I) : Found->second;
		Point->m_position = Graph.vertex_position(I);
		m_covers->insert(Point);
		if (Found != Existing.end())
		{
			Existing.erase(Found);
		}
	}
	for (const auto& Entry : Existing)
	{
		EditorRetiredCovers.push_back(Entry.second);
	}
	m_nearest.clear();
}

void CCoverManager::clear					()
{
	clear_covers(EditorRetiredCovers);
	if (!get_covers())
		return;

	covers().all			(m_nearest);
	clear_covers			(m_nearest);
	m_covers->clear			();
	xr_delete				(m_smart_covers_storage);
	m_smart_covers.clear	();
	if (m_covers)
	{
		xr_delete(m_covers);
	}
}

namespace smart_cover {
	struct predicate {
		object const	*m_object;
	
		IC				predicate	(object const &object) :
			m_object	(&object)
		{
		}

		IC	bool		operator()	(CCoverPoint * const &cover) const
		{
			if (cover->m_is_smart_cover)
				return	(true);

			return		(!m_object->inside(cover->position()));
		}
	};
} // namespace smart_cover

void CCoverManager::remove_nearby_covers	(smart_cover::cover const &cover, smart_cover::object const &object) const
{
	m_nearest.clear			();
	typedef smart_cover::description::Loopholes::const_iterator const_iterator;
	const_iterator			I = cover.loopholes().begin();
	const_iterator			E = cover.loopholes().end();
	for ( ; I != E; ++I ) {
		Fvector				position = cover.fov_position(**I);
		m_covers->nearest	(position, object.Radius() + 1.f, m_nearest);

		m_nearest.erase(
			std::remove_if(
				m_nearest.begin(),
				m_nearest.end(),
				smart_cover::predicate(object)
			),
			m_nearest.end()
		);

		typedef PointVector::const_iterator const_iterator_;
		const_iterator_		i = m_nearest.begin();
		const_iterator_		e = m_nearest.end();
		for ( ; i != e; ++i)
			m_covers->remove(*i);

		clear_covers		(m_nearest);
	}
}

CCoverManager::Cover const *CCoverManager::add_smart_cover	(const char* table_name, smart_cover::object const &object, bool const &is_combat_cover, bool const &can_fire, luabind::object const &loopholes) const
{
	Cover					*smart_cover = 
		new Cover(
			object,
			m_smart_covers_storage->description(table_name),
			is_combat_cover,
			can_fire,
			loopholes
		);

	remove_nearby_covers	(*smart_cover, object);
	m_covers->insert		(smart_cover);

	m_smart_covers.push_back(smart_cover);
	m_smart_covers_actual	= false;

	return					(smart_cover);
}

class id_predicate_less {
private:
	typedef	smart_cover::cover	Cover;

public:
	IC 	bool	operator ()		(Cover *lhs, Cover *rhs)
	{
		VERIFY				(lhs);
		VERIFY				(rhs);
		return				(lhs->object().cName()._get() < rhs->object().cName()._get());
	}
	IC 	bool	operator ()		(Cover *cover, shared_str const &id)
	{
		VERIFY				(cover);
		return				(cover->object().cName()._get() < id._get());
	}
};

void CCoverManager::actualize_smart_covers					() const
{
	std::sort(
		m_smart_covers.begin(),
		m_smart_covers.end(),
		id_predicate_less()
	);

	m_smart_covers_actual	= true;
}

CCoverManager::Cover *CCoverManager::smart_cover			(shared_str const &cover_id) const
{
	if (!m_smart_covers_actual)
		actualize_smart_covers();

	SmartCovers::iterator	found = std::lower_bound(
										m_smart_covers.begin(),
										m_smart_covers.end(),
										cover_id,
										id_predicate_less()
	);

	VERIFY2					(
		((found != m_smart_covers.end()) && ((*found)->id()._get() == cover_id._get())),
		make_string<const char*>(
			"smart_cover [%s] not found",
			cover_id.c_str()
		)
	);

	return					(*found);
}
