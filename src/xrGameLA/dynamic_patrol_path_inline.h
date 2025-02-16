#pragma once

#include "dynamic_patrol_point.h"

IC void	CDynamicPatrolPath::AddPoint(CDynamicPatrolPoint *point)
{
	point->set_parent(this);
	m_dvertices.insert(std::make_pair(point, m_vertices_count));
	m_vertices_count++;
}

IC void CDynamicPatrolPath::AddLink(CDynamicPatrolPoint *p0, CDynamicPatrolPoint *p1)
{
	R_ASSERT(m_vertices_count > 1);
	u16 i = m_dvertices[p0];
	u16 j = m_dvertices[p1];
	m_dedges.insert(std::make_pair(i, j));
}

IC shared_str CDynamicPatrolPath::GetName() const
{
	return m_name;
}