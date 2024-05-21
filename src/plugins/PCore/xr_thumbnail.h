#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_THUMBNAIL_H__
#define __XR_THUMBNAIL_H__

#include "xr_types.h"

namespace xray_re {

enum {
	THM_CHUNK_VERSION		= 0x0810,
	THM_CHUNK_DATA			= 0x0811,
	THM_CHUNK_TYPE			= 0x0813,

	THM_CHUNK_OBJECTPARAM		= 0x0816,

	THM_CHUNK_GROUPPARAM		= 0x0001,
};

enum {
	THM_TYPE_OBJECT		= 0,	// also in group
	THM_TYPE_TEXTURE	= 1,
	THM_TYPE_SOUND		= 2,
};

const uint16_t THM_VERSION_OBJECTPARAM = 0x12;
const uint16_t THM_VERSION_GROUPPARAM = 0x01;

} // end of namespace xray_re

#endif
