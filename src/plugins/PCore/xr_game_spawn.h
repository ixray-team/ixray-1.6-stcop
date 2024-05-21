#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_GAME_SPAWN_H__
#define __XR_GAME_SPAWN_H__

#include <string>
#include <vector>
#include "xr_ai_way.h"
#include "xr_game_graph.h"

namespace xray_re {

struct alife_spawn_header {
	uint32_t	version;
	xr_guid		guid;
	xr_guid		graph_guid;
	uint32_t	count;
	uint32_t	level_count;
};

struct way_point_gs: public way_point {
	uint32_t	node_id;
	uint16_t	graph_id;
};

TYPEDEF_STD_VECTOR(way_point_gs)

struct way_path_gs {
	std::string		name;
	way_point_gs_vec	points;
	way_link_vec		links;
};

TYPEDEF_STD_VECTOR_PTR(way_path_gs)

class cse_abstract;
class xr_reader;
class xr_writer;

class xr_game_spawn {
public:
			xr_game_spawn();
	virtual		~xr_game_spawn();

	void		load(xr_reader& r);
	void		save(xr_writer& w);
	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name);
	bool		load_graph(const char* path, const char* name);
	bool		save_graph(const char* path, const char* name) const;

	void		load_spawns(xr_reader& r);
	void		save_spawns(xr_writer& w);
	void		load_af_slots(xr_reader& r);
	void		save_af_slots(xr_writer& w) const;
	void		load_paths(xr_reader& r);
	void		save_paths(xr_writer& w);

	uint32_t&			version();
	uint32_t			version() const;
	xr_guid&			guid();
	const xr_guid&			guid() const;
	xr_guid&			graph_guid();
	const xr_guid&			graph_guid() const;
	uint32_t&			num_levels();
	uint32_t			num_levels() const;
	std::vector<cse_abstract*>&	spawns();
	const gg_level_point*		af_slots() const;
	way_path_gs_vec&		paths();
	const way_path_gs_vec&		paths() const;
	xr_game_graph&			graph();
	const xr_game_graph&		graph() const;

private:
	uint32_t			m_version;
	xr_guid				m_guid;
	xr_guid				m_graph_guid;
	uint32_t			m_num_levels;
	uint32_t			m_num_af_slots;
	std::vector<cse_abstract*>	m_spawns;
	gg_level_point*			m_af_slots;
	way_path_gs_vec			m_paths;

	xr_game_graph			m_graph;
};

inline uint32_t& xr_game_spawn::version() { return m_version; }
inline uint32_t xr_game_spawn::version() const { return m_version; }
inline xr_guid& xr_game_spawn::guid() { return m_guid; }
inline const xr_guid& xr_game_spawn::guid() const { return m_guid; }
inline xr_guid& xr_game_spawn::graph_guid() { return m_graph_guid; }
inline const xr_guid& xr_game_spawn::graph_guid() const { return m_graph_guid; }
inline uint32_t& xr_game_spawn::num_levels() { return m_num_levels; }
inline uint32_t xr_game_spawn::num_levels() const { return m_num_levels; }
inline std::vector<cse_abstract*>& xr_game_spawn::spawns() { return m_spawns; }
inline const gg_level_point* xr_game_spawn::af_slots() const { return m_af_slots; }
inline way_path_gs_vec& xr_game_spawn::paths() { return m_paths; }
inline const way_path_gs_vec&  xr_game_spawn::paths() const { return m_paths; }

inline xr_game_graph& xr_game_spawn::graph() { return m_graph; }
inline const xr_game_graph& xr_game_spawn::graph() const { return m_graph; }

} // end of namespace xray_re

#endif
