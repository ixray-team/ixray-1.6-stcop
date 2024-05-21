#include "xr_level_fog_vol.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_fog_vol::~xr_level_fog_vol()
{
	delete_elements(m_fog_vols);
}

struct read_fog_vol { void operator()(fog_vol_data*& _fog_vol, xr_reader& r) const {
	fog_vol_data* fog_vol = new fog_vol_data;
	_fog_vol = fog_vol;
	r.r_s(fog_vol->ltx);
	r.r(fog_vol->xform);
	if ((fog_vol->num_particles = r.r_u32())) {
		fog_vol->particles = new fmatrix[fog_vol->num_particles];
		r.r_cseq(fog_vol->num_particles, fog_vol->particles);
	}
}};

void xr_level_fog_vol::load(xr_reader& r)
{
	m_version = r.r_u16();
	xr_assert(m_version == FOG_VOL_VERSION_2 || m_version == FOG_VOL_VERSION_3);
	r.r_seq(r.r_u32(), m_fog_vols, read_fog_vol());
}

void xr_level_fog_vol::save(xr_writer& w) const
{
}
