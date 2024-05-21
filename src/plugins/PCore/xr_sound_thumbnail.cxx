// untested

#include "xr_sound_thumbnail.h"
#include "xr_thumbnail.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_sound_thumbnail::xr_sound_thumbnail():
	quality(0), min_dist(1.f), max_dist(300.f), max_ai_dist(300.f), game_type(0) {}

void xr_sound_thumbnail::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk(THM_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == THM_VERSION_SOUNDPARAM);

	uint32_t type;
	if (!r.r_chunk(THM_CHUNK_TYPE, type))
		xr_not_expected();
	xr_assert(type == THM_TYPE_SOUND);

	if (!r.find_chunk(THM_CHUNK_SOUNDPARAM))
		xr_not_expected();
	quality = r.r_float();
	min_dist = r.r_float();
	max_dist = r.r_float();
	game_type = r.r_u32();
	r.debug_find_chunk();

	if (r.find_chunk(THM_CHUNK_SOUNDPARAM_VOLUME)) {
		base_volume = r.r_float();
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_SOUNDPARAM_AI)) {
		max_ai_dist = r.r_float();
		r.debug_find_chunk();
	}
}

void xr_sound_thumbnail::save(xr_writer& w) const
{
	w.w_chunk<uint16_t>(THM_CHUNK_VERSION, THM_VERSION_SOUNDPARAM);
	w.w_chunk<uint32_t>(THM_CHUNK_TYPE, THM_TYPE_SOUND);

	w.open_chunk(THM_CHUNK_SOUNDPARAM);
	w.w_float(quality);
	w.w_float(min_dist);
	w.w_float(max_dist);
	w.w_u32(game_type);
	w.close_chunk();

	w.open_chunk(THM_CHUNK_SOUNDPARAM_VOLUME);
	w.w_float(base_volume);
	w.close_chunk();

	w.open_chunk(THM_CHUNK_SOUNDPARAM_AI);
	w.w_float(max_ai_dist);
	w.close_chunk();
}
