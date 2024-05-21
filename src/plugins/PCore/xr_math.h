#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MATH_H__
#define __XR_MATH_H__

#define _USE_MATH_DEFINES
#include <cmath>

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif

namespace xray_re {

template<typename T> static inline T deg2rad(T deg) { return T(deg*M_PI/180.); }

template<typename T> static inline T clamp(T value, T min, T max)
{
	return (value < min) ? min : ((value > max) ? max : value);
}

} // end of namespace xray_re

#endif
