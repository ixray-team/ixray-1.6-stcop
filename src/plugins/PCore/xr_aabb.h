#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_AABB_H__
#define __XR_AABB_H__

#include "xr_vector3.h"
#include "xr_limits.h"

namespace xray_re {

template<typename T> struct _aabb {
	_aabb<T>&	null();
	_aabb<T>&	set(const _aabb<T>& box);
	_aabb<T>&	invalidate();
	_aabb<T>&	extend(const _vector3<T>& p);
	_aabb<T>&	grow(T s);
	_aabb<T>&	grow(const _vector3<T>& s);
	_aabb<T>&	shrink(T s);
	_aabb<T>&	shrink(const _vector3<T>& s);
	_aabb<T>&	fix();
	T		volume() const;
	T		size() const;
	T		width() const;
	T		height() const;
	T		depth() const;
	void		center(_vector3<T>& v) const;
	T		overlap_ratio(const _aabb<T>& box) const;
	bool		contain(const _vector3<T>& p) const;
	bool		contain(const _aabb<T>& box) const;
	bool		intersect(const _aabb<T>& box) const;
	bool		intersect_zy(const _aabb<T>& box) const;
	_aabb<T>&	merge(const _aabb<T>& box);

	union {
		struct {
			_vector3<T>	min;
			_vector3<T>	max;
		};
		struct {
			T		x1, y1, z1;
			T		x2, y2, z2;
		};
	};
};

typedef _aabb<float> fbox;
typedef _aabb<int16_t> i16box;

template<typename T> inline _aabb<T>& _aabb<T>::null()
{
	min.set();
	max.set();
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::set(const _aabb<T>& box)
{
	min.set(box.min);
	max.set(box.max);
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::invalidate()
{
	x1 = y1 = z1 = xr_numeric_limits<T>::max();
	x2 = y2 = z2 = xr_numeric_limits<T>::real_min();
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::extend(const _vector3<T>& p)
{
	min.min(p);
	max.max(p);
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::grow(const _vector3<T>& s)
{
	min.sub(s);
	max.add(s);
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::grow(T s)
{
	min.sub(s);
	max.add(s);
	return *this;
}

template<typename T> inline _aabb<T>& _aabb<T>::shrink(const _vector3<T>& s)
{
	min.sub(s);
	max.add(s);
	return fix();
}

template<typename T> inline _aabb<T>& _aabb<T>::shrink(T s)
{
	min.add(s);
	max.sub(s);
	return fix();
}

template<typename T> inline _aabb<T>& _aabb<T>::fix()
{
	if (x2 < x1)
		x1 = x2 = T(0.5)*(x1 + x2);
	if (y2 < y1)
		y1 = y2 = T(0.5)*(y1 + y2);
	if (z2 < z1)
		z1 = z2 = T(0.5)*(z1 + z2);
	return *this;
}

template<typename T> inline bool _aabb<T>::contain(const _vector3<T>& p) const
{
	return (p.x >= min.x && p.x <= max.x) &&
			(p.y >= min.y && p.y <= max.y) &&
			(p.z >= min.z && p.z <= max.z);
}

template<typename T> inline bool _aabb<T>::contain(const _aabb<T>& box) const
{
	return (x1 <= box.x1 && box.x2 <= x2) &&
			(y1 <= box.y1 && box.y2 <= y2) &&
			(z1 <= box.z1 && box.z2 <= z2);
}

template<typename T> inline bool _aabb<T>::intersect(const _aabb<T>& box) const
{
	if (x1 > box.x2 || x2 < box.x1)
		return false;
	if (y1 > box.y2 || y2 < box.y1)
		return false;
	if (z1 > box.z2 || z2 < box.z1)
		return false;
	return true;
}

template<typename T> inline bool _aabb<T>::intersect_zy(const _aabb<T>& box) const
{
	if (z1 > box.z2 || z2 < box.z1)
		return false;
	if (y1 > box.y2 || y2 < box.y1)
		return false;
	return true;
}

template<typename T> inline T _aabb<T>::volume() const
{
	return (x2 - x1)*(y2 - y1)*(z2 - z1);
}

template<typename T> inline T _aabb<T>::size() const
{
	return (x2 - x1) + (y2 - y1) + (z2 - z1);
}

template<typename T> inline T _aabb<T>::width() const { return x2 - x1; }
template<typename T> inline T _aabb<T>::height() const { return y2 - y1; }
template<typename T> inline T _aabb<T>::depth() const { return z2 - z1; }

template<typename T> inline void _aabb<T>::center(_vector3<T>& p) const
{
	p.add(max, min).mul(T(0.5));
}

template<typename T> inline T _aabb<T>::overlap_ratio(const _aabb<T>& box) const
{
	T w = (x2 < box.x2) ? x2 : box.x2;
	w -= (x1 < box.x1) ? box.x1 : x1;
	if (w <= 0)
		return 0;
	T h = (y2 < box.z2) ? y2 : box.y2;
	h -= (y1 < box.y1) ? box.y1 : y1;
	if (h <= 0)
		return 0;
	T d = (z2 < box.z2) ? z2 : box.z2;
	d -= (z1 < box.z1) ? box.z1 : z1;
	if (d <= 0)
		return 0;
	return (w*h*d)/box.volume();
}

template<typename T> inline _aabb<T>& _aabb<T>::merge(const _aabb<T>& box)
{
	if (x1 > box.x1)
		x1 = box.x1;
	if (y1 > box.y1)
		y1 = box.y1;
	if (z1 > box.z1)
		z1 = box.z1;
	if (x2 < box.x2)
		x2 = box.x2;
	if (y2 < box.y2)
		y2 = box.y2;
	if (z2 < box.z2)
		z2 = box.z2;
	return *this;
}

} // end of namespace xray_re

#endif
