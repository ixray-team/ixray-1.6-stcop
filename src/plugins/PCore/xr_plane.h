#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_PLANE_H__
#define __XR_PLANE_H__

#include "xr_vector.h"

namespace xray_re {

template<typename T> struct _plane {
	_plane<T>&	build(const _vector3<T>& p, const _vector3<T>& normal);
	_plane<T>&	build(const _vector3<T>& p, const _vector3<T>& axis0, const _vector3<T>& axis1);
	T		distance(const _vector3<T>& p) const;

	_vector3<T>	n;
	T		d;
};

typedef _plane<float> fplane;

template<typename T> inline _plane<T>& _plane<T>::build(const _vector3<T>& p, const _vector3<T>& normal)
{
	n.set(normal).normalize();
	d = -n.dot_product(p);
	return *this;
}

template<typename T> inline _plane<T>&
_plane<T>::build(const _vector3<T>& p, const _vector3<T>& axis0, const _vector3<T>& axis1)
{
	n.cross_product(axis0, axis1).normalize();
	d = -n.dot_product(p);
	return *this;
}

template<typename T> inline T
_plane<T>::distance(const _vector3<T>& p) const { return n.dot_product(p) + d; }

} // end of namespace xray_re

#endif
