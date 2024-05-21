#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MESH_UTILS_H__
#define __XR_MESH_UTILS_H__

#include "xr_vector2.h"
#include "xr_vector3.h"

namespace xray_re {

template<typename T> static inline T calc_perimeter(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	return p0.distance(p1) + p1.distance(p2) + p2.distance(p0);
}

template<typename T> static inline T calc_perimeter(const _vector2<T>& p0, const _vector2<T>& p1, const _vector2<T>& p2)
{
	return p0.distance(p1) + p1.distance(p2) + p2.distance(p0);
}

template<typename T> static inline T calc_signed_area(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	// i = 0, j = 1
	T x = p0.y*p1.z - p0.z*p1.y;
	T y = p0.z*p1.x - p0.x*p1.z;
	T z = p0.x*p1.y - p0.y*p1.x;

	// i = 1, j = 2
	x += p1.y*p2.z - p1.z*p2.y;
	y += p1.z*p2.x - p1.x*p2.z;
	z += p1.x*p2.y - p1.y*p2.x;

	// i = 2, j = 0
	x += p2.y*p0.z - p2.z*p0.y;
	y += p2.z*p0.x - p2.x*p0.z;
	z += p2.x*p0.y - p2.y*p0.x;

	return T(0.5)*_vector3<T>().calc_normal(p0, p1, p2).dot_product(_vector3<T>().set(x, y, z));
}

template<typename T> static inline T calc_area(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	return std::abs(calc_signed_area(p0, p1, p2));
}

template<typename T> static inline T calc_area_xz(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	return T(0.5)*(p0.z*p1.x - p0.x*p1.z + p1.z*p2.x - p1.x*p2.z + p2.z*p0.x - p2.x*p0.z);
}

} // end of namespace xray_re

#endif
