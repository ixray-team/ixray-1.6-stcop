#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_DETAILS_H__
#define __XR_DETAILS_H__

#include <vector>
#include "xr_types.h"

namespace xray_re {

// Common definitions.
const uint32_t DETAILS_VERSION_2 = 2;
const uint32_t DETAILS_VERSION_3 = 3;
const uint32_t DETAILS_VERSION = DETAILS_VERSION_3;

enum {
	FSD_HEADER	= 0,
	FSD_MESHES	= 1,
	FSD_SLOTS	= 2,
};

class xr_reader;
class xr_writer;

// v2, v3 and LevelEditor
struct details_header {
	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	uint32_t	version;
	uint32_t	object_count;
	int32_t		offs_x;
	int32_t		offs_z;
	uint32_t	size_x;
	uint32_t	size_z;
};

#pragma pack(push, 1)
struct detail_palette {
	uint16_t	a0:4;
	uint16_t	a1:4;
	uint16_t	a2:4;
	uint16_t	a3:4;
};

// v2 (1098, 1114, 1154)
// FIXME: move this into xr_level_details.h?
struct detail_slot_v2 {
	float		y_base;
	float		y_top;
	struct {
		uint8_t		id;
		detail_palette	palette;
	}		part[4];
	uint16_t	unk;		// colors?
};
#pragma pack(pop)

// v3 (2215+)
struct detail_slot_v3 {
	uint32_t	y_base:12;	// y_base*0.2f - 200.f
	uint32_t	y_height:8;	// y_height*0.1f + 0.050000001f
	uint32_t	id0:6;
	uint32_t	id1:6;
	uint32_t	id2:6;
	uint32_t	id3:6;
	uint32_t	c_dir:4;	// c_dir*17
	uint32_t	c_hemi:4;	// c_hemi*17
	uint32_t	c_r:4;		// c_r*17
	uint32_t	c_g:4;		// c_g*17
	uint32_t	c_b:4;		// c_b*17
	detail_palette	palette[4];

	float		r_ybase() const;
	float		r_yheight() const;
	unsigned	r_id(unsigned i) const;
};

inline float detail_slot_v3::r_ybase() const { return y_base*0.2f - 200.f; }
inline float detail_slot_v3::r_yheight() const { return y_height*0.1f + 0.050000001f; }

inline unsigned detail_slot_v3::r_id(unsigned i) const
{
	switch (i) {
	case 0: return id0;
	case 1: return id1;
	case 2: return id2;
	default: return id3;
	}
}

} // end of namespace xray_re

#endif
