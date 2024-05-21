#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_VERSION_H__
#define __XR_LEVEL_VERSION_H__

#include "xr_types.h"

namespace xray_re {

enum {
	XRLC_VERSION_5	= 5,	// 1098
	XRLC_VERSION_8	= 8,	// 1114, 1154
	XRLC_VERSION_9	= 9,	// 1472
	XRLC_VERSION_10	= 10,	// 1475
	XRLC_VERSION_11	= 11,	// 1537
	XRLC_VERSION_12	= 12,	// 1580, 1844, 1865
	XRLC_VERSION_13	= 13,	// 2215
	XRLC_VERSION_14	= 14,	// 2945+, 3456+
};

// chunk ids in level, level.geom and level.geomx
enum {
	FSL_HEADER		= 0x1,	// for auto-detection

	// 1098
	FSL5_HEADER		= 0x1,
	FSL5_TEXTURES		= 0x2,
	FSL5_SHADERS		= 0x3,
	FSL5_VISUALS		= 0x4,
	FSL5_VB			= 0x5,
	FSL5_CFORM		= 0x6,
	FSL5_PORTALS		= 0x7,
	FSL5_LIGHT_DYNAMIC	= 0x8,
	FSL5_LIGHT_KEY_FRAMES	= 0x9,	// ???
	FSL5_GLOWS		= 0xa,
	FSL5_SECTORS		= 0xb,

	// 1114, 1154
	FSL8_HEADER		= 0x1,
	FSL8_SHADERS		= 0x2,
	FSL8_VISUALS		= 0x3,
	FSL8_VB			= 0x4,
	FSL8_CFORM		= 0x5,
	FSL8_PORTALS		= 0x6,
	FSL8_LIGHT_DYNAMIC	= 0x7,
	FSL8_GLOWS		= 0x9,
	FSL8_SECTORS		= 0xa,

	// 1472
	FSL9_HEADER		= 0x1,
	FSL9_SHADERS		= 0x2,
	FSL9_VISUALS		= 0x3,
	FSL9_VB_OLD			= 0x4,
	FSL9_CFORM		= 0x5,
	FSL9_PORTALS		= 0x6,
	FSL9_SHADER_CONSTANT= 0x7, //???
	FSL9_LIGHT_DYNAMIC	= 0x8,
	FSL9_GLOWS		= 0x9,
	FSL9_SECTORS		= 0xa,
	FSL9_IB			= 0xB,
	FSL9_VB			= 0xC,

	// 1475
	FSL10_HEADER		= 0x1,
	FSL10_SHADERS		= 0x2,
	FSL10_VISUALS		= 0x3,
	FSL10_PORTALS		= 0x4,
	FSL10_LIGHT_DYNAMIC	= 0x6,
	FSL10_GLOWS			= 0x7,
	FSL10_SECTORS		= 0x8,
	FSL10_IB			= 0x9,
	FSL10_VB			= 0xa,

	// 1580, 1844, 1865
	FSL12_HEADER		= 0x1,
	FSL12_SHADERS		= 0x2,
	FSL12_VISUALS		= 0x3,
	FSL12_PORTALS		= 0x4,
	FSL12_LIGHT_DYNAMIC	= 0x6,
	FSL12_GLOWS			= 0x7,
	FSL12_SECTORS		= 0x8,
	FSL12_IB			= 0x9,
	FSL12_VB			= 0xa,
	FSL12_SWIS			= 0xb,// build 2218

	// 2215+
	FSL13_HEADER		= 0x1,
	FSL13_SHADERS		= 0x2,
	FSL13_VISUALS		= 0x3,
	FSL13_PORTALS		= 0x4,
	FSL13_LIGHT_DYNAMIC	= 0x6,
	FSL13_GLOWS		= 0x7,
	FSL13_SECTORS		= 0x8,
	FSL13_VB		= 0x9,
	FSL13_IB		= 0xa,
	FSL13_SWIS		= 0xb,
};

// not used. just for the record.
// v5, v8
struct fsl_header_v5 {
	uint16_t	xrlc_version;
	uint16_t	xrlc_quality;
	char		name[124];
};

// v13+
struct fsl_header_v13 {
	uint16_t	xrlc_version;
	uint16_t	xrlc_quality;
};

} // end of namespace xray_re

#endif
