#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_GAME_GRAPH_H__
#define __XR_GAME_GRAPH_H__

#include "xr_level_graph.h"
#include "xr_level_gct.h"

namespace xray_re {

TYPEDEF_STD_VECTOR_PTR(xr_level_gct)

class xr_game_graph: public xr_level_graph {
public:
			xr_game_graph();
	virtual		~xr_game_graph();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;
	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name) const;

	xr_level_gct_vec&	cross_tables();
	const xr_level_gct_vec&	cross_tables() const;

private:
	// v9-v10 addition
	xr_level_gct_vec	m_cross_tables;
};

inline xr_game_graph::xr_game_graph() {}
inline xr_level_gct_vec& xr_game_graph::cross_tables() { return m_cross_tables; }
inline const xr_level_gct_vec& xr_game_graph::cross_tables() const { return m_cross_tables; }

} // end of namespace xray_re

#endif
