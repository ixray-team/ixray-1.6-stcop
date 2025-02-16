#pragma once

#ifdef DEBUG
IC void CDynamicPatrolPoint::initialized(bool value)
{
	m_initialized		= value;
}
#endif

IC void CDynamicPatrolPoint::set_name(LPCSTR name)
{
	m_name				= name;
}

IC void CDynamicPatrolPoint::set_position(Fvector3 pos)
{
	m_position			= pos;
	m_position.y		+= .15f;
}

IC void CDynamicPatrolPoint::set_level_vertex_id(u32 lvid)
{
	m_level_vertex_id	= lvid;
}

IC void CDynamicPatrolPoint::set_game_vertex_id(GameGraph::_GRAPH_ID gvid)
{
	m_game_vertex_id	= gvid;
}

IC void CDynamicPatrolPoint::set_flags(u32 flags)
{
	m_flags				= flags;
}

IC void CDynamicPatrolPoint::set_parent(const CPatrolPath *path)
{
#ifdef DEBUG
	m_path				= path;
	m_initialized		= false;
#endif
}