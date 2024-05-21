#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_RECT_H__
#define __XR_RECT_H__

#include "xr_limits.h"
#include "xr_vector2.h"

namespace xray_re {

template<typename T> struct _rect {
	_rect<T>&	invalidate();
	_rect<T>&	extend(const _vector2<T>& v);
	union {
		struct {
			_vector2<T>	lt;
			_vector2<T>	rb;
		};
		struct {
			T		x1, y1;
			T		x2, y2;
		};
		struct {
			T		left, top;
			T		right, bottom;
		};
	};
};

typedef _rect<float> frect;
typedef _rect<int> irect;

template<typename T> _rect<T>& _rect<T>::invalidate()
{
	x1 = y1 = xr_numeric_limits<T>::max();
	x2 = y2 = xr_numeric_limits<T>::real_min();
	return *this;
}

template<typename T> _rect<T>& _rect<T>::extend(const _vector2<T>& v)
{
	lt.min(v);
	rb.max(v);
	return *this;
}

} // end of namespace xray_re

#endif
