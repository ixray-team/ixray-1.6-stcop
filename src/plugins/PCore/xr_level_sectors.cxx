#include "xr_level_version.h"
#include "xr_level_sectors.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_sectors::~xr_level_sectors()
{
	delete_elements(m_sectors);
}

struct read_sector_v5 { void operator()(sector_data*& _sector, xr_reader& r) const {
	sector_data* sector = new sector_data;
	_sector = sector;
	if (!r.r_chunk(FSP_ROOT, sector->root))
		xr_not_expected();
	size_t size = r.find_chunk(FSP_PORTALS);
	xr_assert(size == 0 || (size % sizeof(uint16_t) == 0));
	r.r_seq(size/sizeof(uint16_t), sector->portals);
}};

void xr_level_sectors::load_v5(xr_reader& r)
{
	// FIXME: not all fields handled.
	r.r_chunks(m_sectors, read_sector_v5());
}

struct read_sector_v13 { void operator()(sector_data*& _sector, xr_reader& r) const {
	sector_data* sector = new sector_data;
	_sector = sector;
	if (!r.r_chunk(FSP_ROOT, sector->root))
		xr_not_expected();
	size_t size = r.find_chunk(FSP_PORTALS);
	xr_assert(size == 0 || (size % sizeof(uint16_t) == 0));
	r.r_seq(size/sizeof(uint16_t), sector->portals);
}};

void xr_level_sectors::load_v13(xr_reader& r)
{
	r.r_chunks(m_sectors, read_sector_v13());
}

void xr_level_sectors::load(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	if (xrlc_version <= XRLC_VERSION_9) {
		if (xrlc_version == XRLC_VERSION_5)
			s = r.open_chunk(FSL5_SECTORS);
		else
			s = r.open_chunk(FSL8_SECTORS);
		xr_assert(s);
		load_v5(*s);
	} else {
		xr_reader* s = r.open_chunk(FSL13_SECTORS);
		xr_assert(s);
		load_v13(*s);
	}
	r.close_chunk(s);
}

void xr_level_sectors::save(xr_writer& w) const
{
}
