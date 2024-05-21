#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_GRAPH_H__
#define __XR_LEVEL_GRAPH_H__

#include <string>
#include <vector>
#include "xr_ai_graph.h"

namespace xray_re {

enum {
	GG_VERTEX2215_SIZE	= 0x28,
	GG_VERTEX_SIZE		= 0x2a,

	GG_EDGE2215_SIZE	= 0x8,
	GG_EDGE_SIZE		= 0x6,

	GG_LEVEL_POINT_SIZE	= 0x14,
};

class xr_reader;
class xr_writer;

class xr_level_graph {
public:
			xr_level_graph();
	virtual		~xr_level_graph();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;
	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name) const;

	uint32_t&		version();
	uint32_t		version() const;
	uint32_t		num_levels() const;
	uint32_t		num_vertices() const;
	uint32_t		num_edges() const;
	uint32_t		num_death_points() const;
	xr_guid&		guid();
	const xr_guid&		guid() const;
	const gg_level*		levels() const;
	const gg_vertex*	vertices() const;
	const gg_edge*		edges() const;
	const gg_level_point*	death_points() const;

protected:
	uint32_t		m_version;
	uint32_t		m_num_levels;
	uint32_t		m_num_vertices;
	uint32_t		m_num_edges;
	uint32_t		m_num_death_points;
	xr_guid			m_guid;
	gg_level*		m_levels;
	gg_vertex*		m_vertices;
	gg_edge*		m_edges;
	gg_level_point*		m_death_points;
};

inline uint32_t& xr_level_graph::version() { return m_version; }
inline uint32_t xr_level_graph::version() const { return m_version; }

inline uint32_t xr_level_graph::num_levels() const { return m_num_levels; }
inline uint32_t xr_level_graph::num_vertices() const { return m_num_vertices; }
inline uint32_t xr_level_graph::num_edges() const { return m_num_edges; }
inline uint32_t xr_level_graph::num_death_points() const { return m_num_death_points; }

inline xr_guid& xr_level_graph::guid() { return m_guid; }
inline const xr_guid& xr_level_graph::guid() const { return m_guid; }
inline const gg_level* xr_level_graph::levels() const { return m_levels; }
inline const gg_vertex* xr_level_graph::vertices() const { return m_vertices; }
inline const gg_edge* xr_level_graph::edges() const { return m_edges; }
inline const gg_level_point* xr_level_graph::death_points() const { return m_death_points; }

} // end of namespace xray_re

#endif
