#include "xr_level_ps_static.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_ps_static::~xr_level_ps_static()
{
	delete_elements(m_particles);
}

void xr_level_ps_static::load(xr_reader& r)
{
	if (r.find_chunk(PS_CHUNK_VERSION) == sizeof(uint32_t)) {
		m_version = r.r_u32();
		xr_assert(m_version == PS_VERSION_1);
	} else {
		m_version = PS_VERSION_0;
	}
	// compiler will optimize this, so no explicit (id = version) trick.
	uint32_t id = (m_version == PS_VERSION_0) ? 0 : 1;
	for (xr_reader* s; (s = r.open_chunk(id)); ++id) {
		particle_data* particle = new particle_data;
		m_particles.push_back(particle);
		particle->extra = (m_version == PS_VERSION_0) ? 0xffff : r.r_u16();
		r.r_sz(particle->reference);
		r.r(particle->xform);
		r.close_chunk(s);
	}
}

void xr_level_ps_static::save(xr_writer& w) const
{
	uint32_t id;
	if (m_version == PS_VERSION_0) {
		id = 0;
	} else {
		id = 1;
		w.w_chunk(PS_CHUNK_VERSION, m_version);
	}
	for (particle_data_vec_cit it = m_particles.begin(), end = m_particles.end();
			it != end; ++it, ++id) {
		const particle_data* particle = *it;
		w.open_chunk(id);
		if (m_version != PS_VERSION_0)
			w.w_u16(particle->extra);
		w.w_sz(particle->reference);
		w.w(particle->xform);
		w.close_chunk();
	}
}

bool xr_level_ps_static::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_ps_static::save(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
