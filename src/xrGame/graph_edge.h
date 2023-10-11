////////////////////////////////////////////////////////////////////////////
//	Module 		: CEdge.h
//	Created 	: 14.01.2004
//  Modified 	: 19.02.2005
//	Author		: Dmitriy Iassenev
//	Description : Graph edge class template
////////////////////////////////////////////////////////////////////////////

#pragma once

#include <loki/emptytype.h>

template <
	typename _edge_weight_type,
	typename _vertex_type
>
class CEdgeBase {
public:
	typedef _edge_weight_type						_edge_weight_type;
	typedef _vertex_type							_vertex_type;
	typedef typename _vertex_type::_vertex_id_type	_vertex_id_type;

private:
	_edge_weight_type				m_weight;
	_vertex_type					*m_vertex;

public:
	IC								CEdgeBase	(const _edge_weight_type &weight, _vertex_type *vertex);
	IC		const _edge_weight_type	&weight		() const;
	IC		_vertex_type			*vertex		() const;
	IC		const _vertex_id_type	&vertex_id	() const;
};

template <
	typename _edge_weight_type,
	typename _vertex_type,
	typename _edge_data_type
>
class CEdge : public CEdgeBase<_edge_weight_type,_vertex_type> {
private:
	typedef CEdgeBase<_edge_weight_type,_vertex_type>	inherited;

private:
	_edge_data_type					m_data;
	using _vertex_id_type = inherited::_vertex_id_type;

public:
	IC								CEdge		(const _edge_weight_type &weight, _vertex_type *vertex);
	IC		bool					operator==	(const CEdge &obj) const;
	IC		const _edge_data_type	&data		() const;
	IC		_edge_data_type			&data		();

	IC bool operator==(const _vertex_id_type& vertex_id) const {
		return (this->vertex()->vertex_id() == vertex_id);
	}
};

template <
	typename _edge_weight_type,
	typename _vertex_type
>
class CEdge<_edge_weight_type, _vertex_type, Loki::EmptyType> : public CEdgeBase<_edge_weight_type,_vertex_type> {
private:
	typedef CEdgeBase<_edge_weight_type,_vertex_type>	inherited;
	using _vertex_id_type = inherited::_vertex_id_type;

public:
	IC								CEdge		(const _edge_weight_type &weight, _vertex_type *vertex);
	IC		bool					operator==	(const CEdge &obj) const;

	IC bool operator==(const _vertex_id_type& vertex_id) const {
		return (this->vertex()->vertex_id() == vertex_id);
	}
};

#include "graph_edge_inline.h"