#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_PORTALS_H__
#define __XR_LEVEL_PORTALS_H__

#include <vector>
#include "xr_vector3.h"
#include "xr_fixed_vector.h"

namespace xray_re {

// xrLC v5
struct fsl_portal_v5 {
	uint16_t		sector_front;
	uint16_t		sector_back;
	uint32_t		count;
	fvector3		vertices[6];
};

// xrLC v8+
struct fsl_portal_v8 {
	uint16_t		sector_front;
	uint16_t		sector_back;
	_svector<fvector3, 6>	vertices;
};

// currently is the same as v8+ above.
struct portal_data {
	uint16_t		sector_front;
	uint16_t		sector_back;
	_svector<fvector3, 6>	vertices;
};

TYPEDEF_STD_VECTOR_PTR(portal_data)

class xr_reader;
class xr_writer;

class xr_level_portals {
public:
				xr_level_portals(uint32_t xrlc_version, xr_reader& r);
	virtual			~xr_level_portals();

	void			load(uint32_t xrlc_version, xr_reader& r);
	void			save(xr_writer& w) const;

	const portal_data_vec&	portals() const;

private:
	void			load_v5(xr_reader& r);
	void			load_v8(xr_reader& r);	// v8+

private:
	portal_data_vec		m_portals;
};

inline xr_level_portals::xr_level_portals(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline const portal_data_vec& xr_level_portals::portals() const { return m_portals; }

} // end of namespace xray_re

#endif
