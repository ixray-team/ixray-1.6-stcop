#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_VECTOR4_H__
#define __XR_VECTOR4_H__

namespace xray_re {

template<typename T> struct _vector4 {
	T		x, y, z, w;
	void		set(T _x, T _y, T _z, T _w);
};

typedef _vector4<float> fvector4;

template<typename T> inline void _vector4<T>::set(T _x, T _y, T _z, T _w)
{
	x = _x;
	y = _y;
	z = _z;
	w = _w;
}

} // end of namespace xray_re

#endif
