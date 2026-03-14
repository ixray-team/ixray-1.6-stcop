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

struct SGameVertex :
	public SBaseParameters<float, u32, u32>
{
	typedef GameGraph::TERRAIN_VECTOR	VERTEX_TYPES;

	const VERTEX_TYPES* m_vertex_types;
	u32 m_vertex_id;

	IC SGameVertex(const VERTEX_TYPES& vertex_types, float max_range = 6000.f, u32 max_iteration_count = u32(-1), u32 max_visited_node_count = u32(-1))
		:
		SBaseParameters<float, u32, u32>
		(
			max_range,
			max_iteration_count,
			max_visited_node_count
		)
	{
		m_vertex_types = &vertex_types;
	}

	IC	u32	selected_vertex_id() const
	{
		return		(m_vertex_id);
	}
};