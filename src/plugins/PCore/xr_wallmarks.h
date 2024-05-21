#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_WALLMARKS_H__
#define __XR_WALLMARKS_H__

#include <vector>
#include "xr_types.h"
#include "xr_vector3.h"
#include "xr_color.h"

namespace xray_re {

// Common definitions.
struct wm_vertex {
	fvector3	p;
	rgba32		color;
	float		u, v;
};

TYPEDEF_STD_VECTOR(wm_vertex)

} // end of namespace xray_re

#endif
