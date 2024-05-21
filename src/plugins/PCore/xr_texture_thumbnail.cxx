#include "xr_texture_thumbnail.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_texture_thumbnail::xr_texture_thumbnail():
	data(0) {}

xr_texture_thumbnail::~xr_texture_thumbnail()
{
	delete[] data;
}

void xr_texture_thumbnail::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk(THM_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == THM_VERSION_TEXTUREPARAM);

	bool compressed = false;
	if (r.find_chunk(THM_CHUNK_DATA, &compressed)) {
		xr_reader* s = r.open_chunk(THM_CHUNK_DATA);
		data = new uint8_t[s->size()];
		s->r_raw(data, s->size());
		r.close_chunk(s);
	}
	uint32_t thm_type;
	if (!r.r_chunk(THM_CHUNK_TYPE, thm_type))
		xr_not_expected();
	xr_assert(thm_type == THM_TYPE_TEXTURE);

	if (!r.find_chunk(THM_CHUNK_TEXTUREPARAM))
		xr_not_expected();
	fmt = static_cast<et_format>(r.r_u32());
	flags = r.r_u32();
	border_color = r.r_u32();
	fade_color = r.r_u32();
	fade_amount = r.r_u32();
	mip_filter = r.r_u32();
	width = r.r_s32();
	height = r.r_s32();
	r.debug_find_chunk();

	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_TYPE)) {
		type = static_cast<et_type>(r.r_u32());
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_DETAIL)) {
		r.r_sz(detail_name);
		detail_scale = r.r_float();
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_MATERIAL)) {
		material = static_cast<et_material>(r.r_u32());
		material_weight = r.r_float();
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_BUMP)) {
		bump_virtual_height = r.r_float();
		bump_mode = static_cast<et_bump_mode>(r.r_u32());
		if (bump_mode == tbm_reserved)
			bump_mode = tbm_none;
		r.r_sz(bump_name);
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_NMAP)) {
		r.r_sz(ext_normal_map_name);
		r.debug_find_chunk();
	}
	if (r.find_chunk(THM_CHUNK_TEXTUREPARAM_FADE)) {
		fade_delay = r.r_u8();
		r.debug_find_chunk();
	}
}

bool xr_texture_thumbnail::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}
