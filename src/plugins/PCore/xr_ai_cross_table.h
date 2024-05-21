#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_AI_CROSS_TABLE_H__
#define __XR_AI_CROSS_TABLE_H__

#include "xr_types.h"
#include "xr_guid.h"

namespace xray_re {

const uint16_t AI_GRAPH_BAD_VERTEX = 0xffff;

#pragma pack(push, 1)

// v9 too
struct gct_header_v8 {
	uint32_t	version;
	uint32_t	node_count;
	uint32_t	graph_point_count;
	xr_guid		level_guid;
	xr_guid		game_guid;
};

struct gct_cell_v8 {
	uint16_t	graph_id;
	float		distance;
};
#pragma pack(pop)

// in-memory structure
struct gct_cell {
	uint16_t	graph_id;
	uint16_t	__pad;
	float		distance;
};

class xr_reader;
class xr_writer;

struct gct_cell_io {
	void	operator()(gct_cell& cell, xr_reader& r) const;
	void	operator()(const gct_cell& cell, xr_writer& w) const;
};

} // end of namespace xray_re

#endif
