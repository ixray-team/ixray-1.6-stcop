#include "xr_level_version.h"
#include "xr_level_cform.h"
#include "xr_reader.h"

using namespace xray_re;

xr_level_cform::xr_level_cform(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	if (xrlc_version == XRLC_VERSION_5)
		s = r.open_chunk(FSL5_CFORM);
	else if (xrlc_version == XRLC_VERSION_8 || xrlc_version == XRLC_VERSION_9)
		s = r.open_chunk(FSL8_CFORM);
	xr_assert(s);
	xr_cform::load(*s);
	r.close_chunk(s);
}
