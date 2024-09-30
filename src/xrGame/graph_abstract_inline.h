////////////////////////////////////////////////////////////////////////////
//	Module 		: graph_inline.h
//	Created 	: 14.01.2004
//  Modified 	: 19.02.2005
//	Author		: Dmitriy Iassenev
//	Description : Graph class template inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

#define TEMPLATE_SPECIALIZATION template <\
	typename _data_type,\
	typename _edge_weight_type,\
	typename _vertex_id_type,\
	typename _edge_data_type\
>

#define CAbstractGraph CGraphAbstract<\
	_data_type,\
	_edge_weight_type,\
	_vertex_id_type,\
	_edge_data_type\
>

TEMPLATE_SPECIALIZATION
IC	CAbstractGraph::CGraphAbstract	()
{
	m_edge_count				= 0;
}

TEMPLATE_SPECIALIZATION
IC	CAbstractGraph::~CGraphAbstract	()
{
	clear						();
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::add_vertex			(const _data_type &data, const _vertex_id_type &vertex_id)
{
	VERIFY						(!vertex(vertex_id));
	m_vertices.insert			(std::make_pair(vertex_id,new CVertex(data,vertex_id,&m_edge_count)));
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::remove_vertex(const _vertex_id_type& vertex_id)
{
	vertex_iterator I = m_vertices.find(vertex_id);
	VERIFY(m_vertices.end() != I);
	auto v = *I;
	delete_data(v);
	m_vertices.erase(I);
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::add_edge			(const _vertex_id_type &vertex_id0, const _vertex_id_type &vertex_id1, const _edge_weight_type &edge_weight)
{
	CVertex						*_vertex0 = vertex(vertex_id0);
	VERIFY						(_vertex0);
	CVertex						*_vertex1 = vertex(vertex_id1);
	VERIFY						(_vertex1);
	_vertex0->add_edge			(_vertex1,edge_weight);
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::add_edge			(const _vertex_id_type &vertex_id0, const _vertex_id_type &vertex_id1, const _edge_weight_type &edge_weight0, const _edge_weight_type &edge_weight1)
{
	add_edge					(vertex_id0,vertex_id1,edge_weight0);
	add_edge					(vertex_id1,vertex_id0,edge_weight1);
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::remove_edge		(const _vertex_id_type &vertex_id0, const _vertex_id_type &vertex_id1)
{
	CVertex						*_vertex = vertex(vertex_id0);
	VERIFY						(_vertex);
	VERIFY						(vertex(vertex_id1));
	_vertex->remove_edge		(vertex_id1);
}

TEMPLATE_SPECIALIZATION
IC	u32 CAbstractGraph::vertex_count		() const
{
	return						((u32)m_vertices.size());
}

TEMPLATE_SPECIALIZATION
IC	u32 CAbstractGraph::edge_count			() const
{
	return						(m_edge_count);
}

TEMPLATE_SPECIALIZATION
IC	bool CAbstractGraph::empty				() const
{
	return						(m_vertices.empty());
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::clear				()
{
	while (!vertices().empty())
		remove_vertex			(vertices().begin()->first);
	VERIFY						(!m_edge_count);
}

TEMPLATE_SPECIALIZATION
IC	const typename CAbstractGraph::CVertex *CAbstractGraph::vertex	(const _vertex_id_type &vertex_id) const
{
	const_vertex_iterator		I = vertices().find(vertex_id);
	if (vertices().end() == I)
		return					(0);
	return						((*I).second);
}

TEMPLATE_SPECIALIZATION
IC	typename CAbstractGraph::CVertex *CAbstractGraph::vertex		(const _vertex_id_type &vertex_id)
{
	vertex_iterator				I = m_vertices.find(vertex_id);
	if (m_vertices.end() ==	I)
		return					(0);
	return						((*I).second);
}

TEMPLATE_SPECIALIZATION
IC	const typename CAbstractGraph::CEdge *CAbstractGraph::edge		(const _vertex_id_type &vertex_id0, const _vertex_id_type &vertex_id1) const
{
	const CVertex				*_vertex = vertex(vertex_id0);
	if (!_vertex)
		return					(0);
	return						(_vertex->edge(vertex_id1));
}

TEMPLATE_SPECIALIZATION
IC	typename CAbstractGraph::CEdge *CAbstractGraph::edge			(const _vertex_id_type &vertex_id0, const _vertex_id_type &vertex_id1)
{
	CVertex						*_vertex = vertex(vertex_id0);
	if (!_vertex)
		return					(0);
	return						(_vertex->edge(vertex_id1));
}

TEMPLATE_SPECIALIZATION
IC	const typename CAbstractGraph::VERTICES &CAbstractGraph::vertices	() const
{
	return						(m_vertices);
}

TEMPLATE_SPECIALIZATION
IC	typename CAbstractGraph::VERTICES &CAbstractGraph::vertices			()
{
	return						(m_vertices);
}

TEMPLATE_SPECIALIZATION
IC	const CAbstractGraph &CAbstractGraph::header						() const
{
	return						(*this);
}

TEMPLATE_SPECIALIZATION
IC	bool CAbstractGraph::operator==		(const CGraphAbstract &obj) const
{
	if (vertex_count() != obj.vertex_count())
		return					(false);

	if (edge_count() != obj.edge_count())
		return					(false);

	return						(equal(vertices(),obj.vertices()));
}

TEMPLATE_SPECIALIZATION
IC	const _edge_weight_type CAbstractGraph::get_edge_weight(const _vertex_id_type vertex_index0, const _vertex_id_type vertex_index1, const_iterator i) const
{
	VERIFY						(edge(vertex_index0,vertex_index1));
	return						((*i).weight());
}

TEMPLATE_SPECIALIZATION
IC	bool CAbstractGraph::is_accessible	(const _vertex_id_type vertex_index) const
{
	return						(true);
}

TEMPLATE_SPECIALIZATION
IC	typename CAbstractGraph::_vertex_id_type const &CAbstractGraph::value	(_vertex_id_type const &vertex_index, const_iterator i) const
{
	return						((*i).vertex_id());
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::begin	(const CVertex *vertex, const_iterator &b, const_iterator &e) const
{
	VERIFY						(vertex);
	b							= vertex->edges().begin();
	e							= vertex->edges().end();
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::begin	(_vertex_id_type const &vertex_index, const_iterator &b, const_iterator &e) const
{
	begin						(vertex(vertex_index), b, e);
}

TEMPLATE_SPECIALIZATION
IC bool CAbstractGraph::Search (_vertex_id_type start_vertex_id, _vertex_id_type dest_vertex_id,xr_vector<_vertex_id_type>& OutPath, _edge_weight_type MaxRange, u32 MaxIterationCount,u32 MaxVisitedNodeCount, _edge_weight_type* LastCost) const
{
	auto IsAccessible = [this](const _vertex_id_type& VertexID)
	{
		if(!is_accessible(VertexID))
		{
			return false;
		}

		return true;
	};

	auto CalcCost = [this](const _vertex_id_type& Node1,const _vertex_id_type& Node2, const_iterator i)
	{
		return get_edge_weight(Node1,Node2,i);
	};

	auto DistanceNode = [this](const CVertex* Node1,const CVertex* Node2)
	{
		return 0;
	};

	thread_local	xr_vector<std::pair<_edge_weight_type, _vertex_id_type>>	TempPriorityNode;
	thread_local	xr_map<_vertex_id_type, _vertex_id_type>					TempCameFrom;
	thread_local	xr_map<_vertex_id_type, _edge_weight_type>					TempCostSoFar;

	TempPriorityNode.clear();
	TempCameFrom.clear();
	TempCostSoFar.clear();
	OutPath.clear();

	const _vertex_id_type& FromID = start_vertex_id;
	const _vertex_id_type& ToID = dest_vertex_id;
		
	if (FromID == ToID)
	{
		OutPath.push_back(start_vertex_id);
		return true;
	}
		
	TempPriorityNode.push_back({0, FromID});
	TempCameFrom.insert({FromID, FromID});
	TempCostSoFar.insert( {FromID, 0});

	
	while (!TempPriorityNode.empty())
	{
		const _vertex_id_type CurrentNodeID = TempPriorityNode.back().second;
		if(LastCost)
		{
			*LastCost = TempPriorityNode.back().first;
		}
		TempPriorityNode.pop_back();
		if (CurrentNodeID == ToID)
		{
			_vertex_id_type NextNode = ToID;
			while (NextNode != FromID)
			{
				OutPath.insert( OutPath.begin(),NextNode);
				NextNode = TempCameFrom[NextNode];
			}
			OutPath.insert( OutPath.begin(),NextNode);
			return true;
		}
		
		const CVertex* Node = vertex(CurrentNodeID);

		const_iterator	i,e;
		begin		(CurrentNodeID,i,e);

		for (; i != e; i++)
		{
			const _vertex_id_type& NeighborID = i->vertex_id();
			if (!IsAccessible(NeighborID)) continue;

			if(MaxIterationCount == 0) continue;
			MaxIterationCount--;

			const CVertex* Neighbor = vertex(NeighborID);
			float NewCost = TempCostSoFar[CurrentNodeID] + CalcCost(CurrentNodeID, NeighborID,i);
			auto TempCostSoFarIterator = TempCostSoFar.find(NeighborID);
			if ((TempCostSoFarIterator != TempCostSoFar.end() &&TempCostSoFarIterator->second > NewCost)|| (TempCostSoFarIterator == TempCostSoFar.end() &&MaxVisitedNodeCount > TempCostSoFar.size()))
			{
				const float Distance = DistanceNode(vertex(ToID), Neighbor);
				if(Distance>MaxRange)
				{
					continue;
				}

				if(TempCostSoFarIterator!=TempCostSoFar.end())
				{
					TempCostSoFarIterator->second = NewCost; 
				}
				else
				{
					TempCostSoFar.insert({NeighborID,NewCost});
				}

				float  priority = NewCost + Distance;
				TempPriorityNode.insert(std::upper_bound(TempPriorityNode.begin(),TempPriorityNode.end(),std::pair<_edge_weight_type,  _vertex_id_type>{priority,NeighborID},[](const std::pair<_edge_weight_type, _vertex_id_type>& Left, const std::pair<_edge_weight_type, _vertex_id_type>& Right) {return Left.first > Right.first; }),{priority,NeighborID});

				
				auto TempCameFromIterator = TempCameFrom.find(NeighborID);
				if(TempCameFromIterator!=TempCameFrom.end())
				{
					TempCameFromIterator->second = CurrentNodeID; 
				}
				else
				{
					TempCameFrom.insert({NeighborID,CurrentNodeID});
				}
			}
		}
	}
	return false;
}

#undef TEMPLATE_SPECIALIZATION
#undef CAbstractGraph

#define TEMPLATE_SPECIALIZATION template <\
	typename _data_type,\
	typename _edge_weight_type,\
	typename _vertex_id_type\
>

#define CAbstractGraph CGraphAbstractSerialize<\
	_data_type,\
	_edge_weight_type,\
	_vertex_id_type\
>

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::save			(IWriter &stream)
{
	stream.open_chunk			(0);
	stream.w_u32				((u32)this->vertices().size());
	stream.close_chunk			();
	
	stream.open_chunk			(1);
	auto I = this->vertices().begin();
	auto E = this->vertices().end();
	for (int i=0; I != E; ++I, ++i) {
		stream.open_chunk		(i);
		{
			stream.open_chunk	(0);
			save_data			((*I).second->vertex_id(),stream);
			stream.close_chunk	();

			stream.open_chunk	(1);
			save_data			((*I).second->data(),stream);
			stream.close_chunk	();
		}
		stream.close_chunk		();
	}
	stream.close_chunk			();

	stream.open_chunk			(2);
	{
		auto I_ = this->vertices().begin();
		auto E_ = this->vertices().end();
		for ( ; I_ != E_; ++I_) {
			if ((*I_).second->edges().empty())
				continue;

			save_data			((*I_).second->vertex_id(),stream);

			stream.w_u32		((u32)(*I_).second->edges().size());
			auto i = (*I_).second->edges().begin();
			auto e = (*I_).second->edges().end();
			for ( ; i != e; ++i) {
				save_data		((*i).vertex_id(),stream);
				save_data		((*i).weight(),stream);
			}
		}
	}
	stream.close_chunk			();
}

TEMPLATE_SPECIALIZATION
IC	void CAbstractGraph::load			(IReader &stream)
{
	this->clear						();

	u32							id;
	_data_type					data;
	_vertex_id_type				vertex_id;
	IReader						*chunk0, *chunk1, *chunk2;

	chunk0						= stream.open_chunk(0);
	chunk0->r_u32				();
	chunk0->close				();

	chunk0						= stream.open_chunk(1);

	for (chunk1 = chunk0->open_chunk_iterator(id); chunk1; chunk1 = chunk0->open_chunk_iterator(id,chunk1)) {
		chunk2					= chunk1->open_chunk(0);
		load_data				(vertex_id,*chunk2);
		chunk2->close			();

		chunk2					= chunk1->open_chunk(1);
		load_data				(data,*chunk2);
		chunk2->close			();

		this->add_vertex				(data,vertex_id);
	}
	chunk0->close				();

	chunk0						= stream.open_chunk(2);
	if (!chunk0)
		return;

	while (!chunk0->eof()) {
		_vertex_id_type			vertex_id0;
		load_data				(vertex_id0,*chunk0);
		
		u32						n = chunk0->r_u32();
		VERIFY					(n);
		for (u32 i=0; i<n; ++i) {
			_vertex_id_type		vertex_id1;
			load_data			(vertex_id1,*chunk0);
			
			_edge_weight_type	edge_weight;
			load_data			(edge_weight,*chunk0);

			this->add_edge			(vertex_id0,vertex_id1,edge_weight);
		}
	}
	chunk0->close				();
}
#undef TEMPLATE_SPECIALIZATION
#undef CAbstractGraph