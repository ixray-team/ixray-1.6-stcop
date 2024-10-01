////////////////////////////////////////////////////////////////////////////
//	Module 		: path_manager_params.h
//	Created 	: 21.03.2002
//  Modified 	: 03.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Base path manager parameters
////////////////////////////////////////////////////////////////////////////

#pragma once
#undef min
#undef max

template <
	typename _dist_type,
	typename _index_type,
	typename _iteration_type
>
struct SBaseParameters {
	_dist_type		max_range;
	_iteration_type	max_iteration_count;
	u32				max_visited_node_count;

	IC	SBaseParameters(
			_dist_type		max_range				= type_max(_dist_type),
			_iteration_type	_max_iteration_count		= _iteration_type(-1),
#ifndef AI_COMPILER
			u32				max_visited_node_count	= 65500
#else
			u32				max_visited_node_count	= u32(-1)
#endif
		) :
			max_range				(max_range),
			max_iteration_count		(_max_iteration_count),
			max_visited_node_count	(max_visited_node_count)
	{
	}

	IC	bool actual () const
	{
		return		(true);
	}
};


template <
	typename _dist_type,
	typename _index_type,
	typename _iteration_type
>
struct SGameVertex : public SBaseParameters<
	_dist_type,
	_index_type,
	_iteration_type
> {
	typedef GameGraph::TERRAIN_VECTOR	VERTEX_TYPES;

	const VERTEX_TYPES	*m_vertex_types;
	_index_type		m_vertex_id;

	IC	SGameVertex (
			const VERTEX_TYPES		&vertex_types,
			_dist_type				max_range = _dist_type(6000),
			_iteration_type			max_iteration_count = _iteration_type(-1),
			_index_type				max_visited_node_count = _index_type(-1)
		)
		:
		SBaseParameters<
			_dist_type,
			_index_type,
			_iteration_type
		>(
			max_range,
			max_iteration_count,
			max_visited_node_count
		)
	{
		m_vertex_types	= &vertex_types;
	}

	IC	_index_type	selected_vertex_id() const
	{
		return		(m_vertex_id);
	}
};
