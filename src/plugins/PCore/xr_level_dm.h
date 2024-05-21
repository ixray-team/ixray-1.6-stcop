#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_DM_H__
#define __XR_LEVEL_DM_H__

#include "xr_dm.h"

namespace xray_re {

class xr_level_dm: public xr_dm {
protected:
	virtual xr_surface*	create_surface(const xr_raw_surface& raw_surface) const;
};

} // end of namespace xray_re

#endif
