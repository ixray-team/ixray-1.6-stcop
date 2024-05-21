#include "xr_level_version.h"
#include "xr_level_visuals.h"
#include "xr_level_geom.h"
#include "xr_ogf_v3.h"
#include "xr_ogf_v4.h"
#include "xr_reader.h"

using namespace xray_re;

xr_level_visuals::~xr_level_visuals() {}

/*void xr_level_visuals::load_d3d7(xr_reader& r, const xr_level_geom* geom)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		xr_ogf_v3* ogf = new xr_ogf_v3;
		ogf->load_ogf(*s);
		ogf->set_ext_geom(geom->vbufs());
		m_ogfs.push_back(ogf);
		r.close_chunk(s);
	}
}

void xr_level_visuals::load_1865(xr_reader& r, const xr_level_geom* geom)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		xr_ogf_v3* ogf = new xr_ogf_v3;
		ogf->load_ogf(*s);
		ogf->set_ext_geom(geom->vbufs(), geom->ibufs());
		m_ogfs.push_back(ogf);
		r.close_chunk(s);
	}
}

void xr_level_visuals::load_d3d9(xr_reader& r, const xr_level_geom* geom)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		xr_ogf_v4* ogf = new xr_ogf_v4;
		ogf->load_ogf(*s);
		ogf->set_ext_geom(geom->vbufs(), geom->ibufs(), geom->swibufs());
		m_ogfs.push_back(ogf);
		r.close_chunk(s);
	}
}*/

void xr_level_visuals::load_ogfs(xr_reader& r, const xr_level_geom* geom)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		if (!s->find_chunk(OGF_HEADER))
			xr_not_expected();
		ogf_version version = static_cast<ogf_version>(s->r_u8());
		switch (version) {
			case 3: {
				xr_ogf_v3 * ogf = new xr_ogf_v3;
				ogf->load_ogf(*s);
				ogf->set_ext_geom(geom->vbufs(), geom->ibufs());
				m_ogfs.push_back(ogf);
				} break;
			case 4: {
				xr_ogf_v4 * ogf = new xr_ogf_v4;
				ogf->load_ogf(*s);
				ogf->set_ext_geom(geom->vbufs(), geom->ibufs(), geom->swibufs());
				m_ogfs.push_back(ogf);
				} break;
			default:
				xr_not_expected();
		}
		r.close_chunk(s);
	}
}

void xr_level_visuals::load(uint32_t xrlc_version, xr_reader& r, const xr_level_geom* geom)
{
	xr_reader* s = 0;

	if (xrlc_version < XRLC_VERSION_8)
		s = r.open_chunk(FSL5_VISUALS);
	else
		s = r.open_chunk(FSL8_VISUALS);

	xr_assert(s);
	load_ogfs(*s, geom);
	r.close_chunk(s);
}
