#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_GCT_H__
#define __XR_LEVEL_GCT_H__

#include "xr_ai_cross_table.h"

namespace xray_re {

enum {
	GCT_CHUNK_HEADER	= 0,
	GCT_CHUNK_CELLS		= 1,
};

class xr_level_gct {
public:
			xr_level_gct();
			xr_level_gct(const xr_level_gct& that);
	virtual		~xr_level_gct();

	void		clear();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;
	void		load_v9(xr_reader& r);
	void		save_v9(xr_writer& w) const;
	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name) const;

	uint32_t&	version();
	uint32_t	version() const;
	const xr_guid&	level_guid() const;
	const xr_guid&	game_guid() const;
	uint32_t	num_nodes() const;
	uint32_t	num_graph_points() const;
	gct_cell*	cells();
	const gct_cell*	cells() const;
	uint16_t	graph_id(uint32_t node_id) const;

protected:
	uint32_t	m_version;
	uint32_t	m_num_nodes;
	uint32_t	m_num_graph_points;
	xr_guid		m_level_guid;
	xr_guid		m_game_guid;
	gct_cell*	m_cells;
};

inline uint32_t& xr_level_gct::version() { return m_version; }
inline uint32_t xr_level_gct::version() const { return m_version; }
inline const xr_guid& xr_level_gct::level_guid() const { return m_level_guid; }
inline const xr_guid& xr_level_gct::game_guid() const { return m_game_guid; }
inline uint32_t xr_level_gct::num_nodes() const { return m_num_nodes; }
inline uint32_t xr_level_gct::num_graph_points() const { return m_num_graph_points; }
inline gct_cell* xr_level_gct::cells() { return m_cells; }
inline const gct_cell* xr_level_gct::cells() const { return m_cells; }

inline uint16_t xr_level_gct::graph_id(uint32_t node_id) const
{
	return (node_id < m_num_nodes) ?
			m_cells[node_id].graph_id : AI_GRAPH_BAD_VERTEX;
}

} // end of namespace xray_re

#endif
