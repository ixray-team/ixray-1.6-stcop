#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SPHERE_H__
#define __XR_SPHERE_H__

#include "xr_vector3.h"

namespace xray_re {

template<typename T> struct _sphere {
	void		reset();
	_sphere<T>&	set(const _sphere<T>& s);

	_vector3<T>	p;
	T		r;
};

typedef _sphere<float> fsphere;
typedef _sphere<double> dsphere;

template<typename T> void _sphere<T>::reset()
{
	p.set();
	r = 0;
}

template<typename T> inline _sphere<T>& _sphere<T>::set(const _sphere<T>& s)
{
	p.set(s.p);
	r = s.r;
	return *this;
}

} // end of namespace xray_re

#endif
