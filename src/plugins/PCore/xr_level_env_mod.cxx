#include "xr_level_env_mod.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_env_mod::~xr_level_env_mod()
{
	delete_elements(m_env_mods);
}

void xr_level_env_mod::load(xr_reader& r)
{
	if (r.find_chunk(ENV_MOD_CHUNK_VERSION) == sizeof(uint32_t)) {
		m_version = r.r_u32();
		xr_assert(m_version == ENV_MOD_VERSION_23);
	} else {
		m_version = ENV_MOD_VERSION_0;
	}
	// compiler will optimize this, so no explicit (id = version) trick.
	uint32_t id = (m_version == ENV_MOD_VERSION_0) ? 0 : 1;
	for (xr_reader* s; (s = r.open_chunk(id)); ++id) {
		env_mod_data* em = new env_mod_data;
		m_env_mods.push_back(em);
		r.r_fvector3(em->position);
		em->radius = r.r_float();
		em->power = r.r_float();
		em->far_plane = r.r_float();
		r.r_fvector3(em->fog_color);
		em->fog_density = r.r_float();
		r.r_fvector3(em->ambient_color);
		r.r_fvector3(em->sky_color);
		r.r_fvector3(em->hemi_color);
		em->extra = (m_version == ENV_MOD_VERSION_0) ? 0xffff : r.r_u16();
	}
}

void xr_level_env_mod::save(xr_writer& w) const
{
	uint32_t id;
	if (m_version == ENV_MOD_VERSION_0) {
		id = 0;
	} else {
		id = 1;
		w.w_chunk(ENV_MOD_CHUNK_VERSION, m_version);
	}
	for (env_mod_data_vec_cit it = m_env_mods.begin(), end = m_env_mods.end();
			it != end; ++it, ++id) {
		const env_mod_data* em = *it;
		w.open_chunk(id);
		w.w_fvector3(em->position);
		w.w_float(em->radius);
		w.w_float(em->power);
		w.w_float(em->far_plane);
		w.w_fvector3(em->fog_color);
		w.w_float(em->fog_density);
		w.w_fvector3(em->ambient_color);
		w.w_fvector3(em->sky_color);
		w.w_fvector3(em->hemi_color);
		if (m_version != ENV_MOD_VERSION_0)
			w.w_u16(em->extra);
		w.close_chunk();
	}
}

bool xr_level_env_mod::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_env_mod::save(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
