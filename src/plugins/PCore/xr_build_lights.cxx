#include "xr_build_lights.h"
#include "xr_reader.h"
#include "xr_utils.h"

using namespace xray_re;

xr_build_lights::~xr_build_lights()
{
	delete_elements(m_rgb);
	delete_elements(m_hemi);
	delete_elements(m_sun);
}

void xr_build_lights::load(xr_reader& r)
{
	bool can_be_soc = true, can_be_cs = true;
	for (uint32_t id = BL_CHUNK_RGB; id <= BL_CHUNK_SUN; ++id) {
		if (size_t size = r.find_chunk(id)) {
			if (size % R_LIGHT_SIZE_SOC)
				can_be_soc = false;
			if (size % R_LIGHT_SIZE_CS)
				can_be_cs = false;
		}
	}
	xr_assert(can_be_soc || can_be_cs);
	if (can_be_soc && can_be_cs) {
		msg("can't autodetect build.lights version");
		return;
	}
	read_lights(r, BL_CHUNK_RGB, m_rgb, can_be_cs);
	read_lights(r, BL_CHUNK_HEMI, m_hemi, can_be_cs);
	read_lights(r, BL_CHUNK_SUN, m_sun, can_be_cs);
}

void xr_build_lights::read_lights(xr_reader& r, uint32_t id, r_light_vec& lights, bool cs)
{
	if (xr_reader* s = r.open_chunk(id)) {
		size_t n = s->size() / (cs ? R_LIGHT_SIZE_CS : R_LIGHT_SIZE_SOC);
		lights.reserve(n);
		for (; n > 0; --n) {
			r_light* light = new r_light;
			lights.push_back(light);
			light->type = s->r_u16();
			light->level = s->r_u16();
			s->r_fvector3(light->diffuse);
			s->r_fvector3(light->position);
			s->r_fvector3(light->direction);
			light->range = s->r_float();
			light->range2 = s->r_float();
			light->attenuation0 = s->r_float();
			light->attenuation1 = s->r_float();
			light->attenuation2 = s->r_float();
			light->energy = s->r_float();
			for (uint_fast32_t i = 0; i != 3; ++i)
				s->r_fvector3(light->tri[i]);
			light->unknown = cs ? s->r_u32() : 0;
		}
		r.close_chunk(s);
	}
}
