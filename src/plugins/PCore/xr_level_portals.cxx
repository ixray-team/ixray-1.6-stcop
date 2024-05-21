#include "xr_level_version.h"
#include "xr_level_portals.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_portals::~xr_level_portals()
{
	delete_elements(m_portals);
}

struct read_portal_v5 { void operator()(portal_data*& _portal, xr_reader& r) const {
	portal_data* portal = new portal_data;
	_portal = portal;
	portal->sector_front = r.r_u16();
	portal->sector_back = r.r_u16();
	portal->vertices.count = r.r_u32();
	for (uint_fast32_t i = 0; i != 6; ++i)
		r.r_fvector3(portal->vertices.array[i]);
}};

void xr_level_portals::load_v5(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_portal_v5) == 0);
	r.r_seq(r.size()/sizeof(fsl_portal_v5), m_portals, read_portal_v5());
}

struct read_portal_v8 { void operator()(portal_data*& _portal, xr_reader& r) const {
	portal_data* portal = new portal_data;
	_portal = portal;
	portal->sector_front = r.r_u16();
	portal->sector_back = r.r_u16();
	for (uint_fast32_t i = 0; i != 6; ++i)
		r.r_fvector3(portal->vertices.array[i]);
	portal->vertices.count = r.r_u32();
}};

void xr_level_portals::load_v8(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_portal_v8) == 0);
	r.r_seq(r.size()/sizeof(fsl_portal_v8), m_portals, read_portal_v8());
}

void xr_level_portals::load(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	if (xrlc_version == XRLC_VERSION_5) {
		s = r.open_chunk(FSL5_PORTALS);
		if (s)
			load_v5(*s);
	} else {
		if (xrlc_version <= XRLC_VERSION_9)
			s = r.open_chunk(FSL8_PORTALS);
		else if (xrlc_version >= XRLC_VERSION_12)
			s = r.open_chunk(FSL13_PORTALS);
		if (s)
			load_v8(*s);
	}
	r.close_chunk(s);
}

void xr_level_portals::save(xr_writer& w) const
{
}
