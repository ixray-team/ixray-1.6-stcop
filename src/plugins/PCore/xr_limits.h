#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LIMITS_H__
#define __XR_LIMITS_H__

#include <limits>
#include <cfloat>

namespace xray_re {

template<typename T> class xr_numeric_limits: public std::numeric_limits<T> {
public:
	static T	real_min()/* throw()*/;
};

template<> inline float xr_numeric_limits<float>::real_min() { return -FLT_MAX; }
template<> inline double xr_numeric_limits<double>::real_min() { return -DBL_MAX; }

} // end of namespace xray_re

#endif
