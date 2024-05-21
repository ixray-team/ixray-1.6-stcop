#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SECTORS_H__
#define __XR_LEVEL_SECTORS_H__

#include <vector>
#include "xr_types.h"

namespace xray_re {

enum {
	FSP_PORTALS	= 0x1,
	FSP_ROOT	= 0x2,
};

struct sector_data {
	uint32_t		root;
	std::vector<uint16_t>	portals;
};

TYPEDEF_STD_VECTOR_PTR(sector_data)

class xr_reader;
class xr_writer;

class xr_level_sectors {
public:
				xr_level_sectors(uint32_t xrlc_version, xr_reader& r);
	virtual			~xr_level_sectors();

	void			load(uint32_t xrlc_version, xr_reader& r);
	void			save(xr_writer& w) const;

	const sector_data_vec&	sectors() const;

private:
	void			load_v5(xr_reader& r);
	void			load_v13(xr_reader& r);

private:
	sector_data_vec		m_sectors;
};

inline xr_level_sectors::xr_level_sectors(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline const sector_data_vec& xr_level_sectors::sectors() const { return m_sectors; }

} // end of namespace xray_re

#endif
