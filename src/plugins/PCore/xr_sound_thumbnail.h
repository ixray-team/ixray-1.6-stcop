#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SOUND_THUMBNAIL_H__
#define __XR_SOUND_THUMBNAIL_H__

#include "xr_thumbnail.h"

namespace xray_re {

enum {
	THM_CHUNK_SOUNDPARAM		= 0x1000,
	THM_CHUNK_SOUNDPARAM_VOLUME	= 0x1001,
	THM_CHUNK_SOUNDPARAM_AI		= 0x1002,
};

const uint16_t THM_VERSION_SOUNDPARAM = 0x14;

class xr_reader;
class xr_writer;

class xr_sound_thumbnail {
public:
			xr_sound_thumbnail();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	float		quality;
	float		min_dist;
	float		max_dist;
	float		max_ai_dist;
	uint32_t	game_type;
	float		base_volume;
};

} // end of namespace xray_re

#endif
