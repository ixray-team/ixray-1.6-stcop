#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_COLOR_H__
#define __XR_COLOR_H__

#include "xr_types.h"

namespace xray_re {

typedef uint32_t rgba32;

template<typename T> struct _color {
	T		r, g, b, a;
	_color<T>&	set(T _r, T _g, T _b, T _a = 0);
	_color<T>&	set(const _color<T>& that);
	_color<T>&	set(const rgba32 rgba);
	rgba32		get() const;
};

typedef _color<float> fcolor;

template<typename T> inline _color<T>& _color<T>::set(T _r, T _g, T _b, T _a)
{
	r = _r;
	g = _g;
	b = _b;
	a = _a;
	return *this;
}

template<typename T> inline _color<T>& _color<T>::set(const _color<T>& that)
{
	*this = that;
	return *this;
}

template<typename T> inline _color<T>& _color<T>::set(const rgba32 rgba)
{
	const T k_max_i = T(1)/255;
	r = (rgba & 0xff)*k_max_i;
	g = ((rgba >> 8) & 0xff)*k_max_i;
	b = ((rgba >> 16) & 0xff)*k_max_i;
	a = (rgba >> 24)*k_max_i;
	return *this;
}

template<typename T> inline rgba32 _color<T>::get() const
{
	const T k_max = T(255);
	rgba32 rgba = uint32_t(a*k_max) << 24;
	rgba |= uint32_t(b*k_max) << 16;
	rgba |= uint32_t(g*k_max) << 8;
	rgba |= uint32_t(r*k_max);
	return rgba;
}

} // end of namespace xray_re

#endif
