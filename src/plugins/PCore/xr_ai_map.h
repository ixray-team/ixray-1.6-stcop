#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_AI_MAP_H__
#define __XR_AI_MAP_H__

#include "xr_types.h"
#include "xr_aabb.h"
#include "xr_guid.h"

namespace xray_re {

// on-disk structures just for the record

// v2 (1098, 1114, 1154)
struct ai_map_header_v2 {
	uint32_t	version;
	uint32_t	count;
	float		size;
	float		size_y;
	fbox		aabb;
};

// v8 (2215+), v9 (3120), v10 (3456+)
struct ai_map_header_v8 {
	uint32_t	version;
	uint32_t	count;
	float		size;
	float		size_y;
	fbox		aabb;
	xr_guid		guid;
};

#pragma pack(push, 1)
// v2
struct ai_map_node_link {
	uint8_t		data[3];
};

struct ai_map_node_v2 {
	uint16_t		plane;
	i16box			bbox;		// quantized
	uint8_t			n_ways;
	uint8_t			pos[5];		// not sure
	uint8_t			num_links;
//	ai_map_node_link	links[0];
};

// used in v8-v10
struct ai_map_node_position {
	uint8_t		packed_xz[3];
	uint16_t	packed_y;
};

const uint32_t AI_MAP_BAD_NODE = 0xffffffff;

const uint32_t AI_MAP_BAD_LINK = 0x7fffff;

// v9 is the same
struct ai_map_node_v8 {
	uint8_t			data[12];	// neighbours + light
	uint16_t		cover0:4;
	uint16_t		cover1:4;
	uint16_t		cover2:4;
	uint16_t		cover3:4;
	uint16_t		plane;
	ai_map_node_position	pos;
};

// FIXME: the order of "high" and "low" covers _is_ correct, but I am not
// sure how to re-map single cover field from v8-v9.
struct ai_map_node_v10 {
	uint8_t			data[12];	// neighbours + light(?)
	uint16_t		cover0:4;	// "high" cover
	uint16_t		cover1:4;
	uint16_t		cover2:4;
	uint16_t		cover3:4;
	uint16_t		low_cover0:4;	// "low" cover
	uint16_t		low_cover1:4;
	uint16_t		low_cover2:4;
	uint16_t		low_cover3:4;
	uint16_t		plane;
	ai_map_node_position	pos;
};

#pragma pack(pop)

} // end of namespace xray_re

#endif
