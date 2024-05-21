#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_CFORM_H__
#define __XR_LEVEL_CFORM_H__

#include "xr_cform.h"

namespace xray_re {

class xr_level_cform: public xr_cform {
public:
		xr_level_cform(xr_reader& r);
		xr_level_cform(uint32_t xrlc_version, xr_reader& r);

	void	load(uint32_t xrlc_version, xr_reader& r);
};

inline xr_level_cform::xr_level_cform(xr_reader& r) { xr_cform::load(r); }

} // end of namespace xray_re

#endif
