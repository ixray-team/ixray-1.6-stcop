#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_VECTOR2_H__
#define __XR_VECTOR2_H__

#include "xr_types.h"
#include "xr_math.h"

namespace xray_re {

template<typename T> struct _vector2 {
	template<typename F> _vector2<T>&	set(const _vector2<F>& a);

	_vector2<T>&	set(T _x = T(), T _y = T());
	_vector2<T>&	set(const T _xy[2]);
	void		get(T _xy[2]) const;
	T		magnitude() const;
	T		distance(const _vector2<T>& a) const;
	bool		similar(const _vector2<T>& a, T e = T(1e-6)) const;
	_vector2<T>&	min(const _vector2<T>& a);
	_vector2<T>&	max(const _vector2<T>& a);
	_vector2<T>&	add(const _vector2<T>& a);
	_vector2<T>&	sub(const _vector2<T>& a, const _vector2<T>& b);
	_vector2<T>&	mul(T s);
	int		compare(const _vector2<T>& right) const;
	bool		operator<(const _vector2<T>& right) const;
	bool		operator==(const _vector2<T>& right) const;
	bool		operator!=(const _vector2<T>& right) const;
	T&		operator[](size_t i);
	const T&	operator[](size_t i) const;

	union {
		struct {
			T	x, y;
		};
		struct {
			T	u, v;
		};
		T		xy[2];
	};
};

typedef _vector2<float> fvector2;
typedef _vector2<double> dvector2;
typedef _vector2<int32_t> i32vector2;

template<typename T> template<typename F> inline _vector2<T>& _vector2<T>::set(const _vector2<F>& a)
{
	x = T(a.x);
	y = T(a.y);
	return *this;
}

template<typename T> inline _vector2<T>& _vector2<T>::set(T _x, T _y) { x = _x; y = _y; return *this; }

template<typename T> inline _vector2<T>& _vector2<T>::set(const T _xy[2])
{
	x = _xy[0];
	y = _xy[1];
	return *this;
}

template<typename T> inline void _vector2<T>::get(T _xy[2]) const
{
	_xy[0] = x;
	_xy[1] = y;
}

template<typename T> inline T _vector2<T>::magnitude() const { return std::sqrt(x*x + y*y); }

template<typename T> inline T _vector2<T>::distance(const _vector2<T>& a) const
{
	T dx = a.x - x, dy = a.y - y;
	return std::sqrt(dx*dx + dy*dy);
}

template<typename T> bool _vector2<T>::similar(const _vector2<T>& a, T e) const
{
	if (e <= std::abs(a.x - x))
		return false;
	if (e <= std::abs(a.y - y))
		return false;
	return true;
}

template<typename T> inline _vector2<T>& _vector2<T>::min(const _vector2<T>& a)
{
	if (x >= a.x)
		x = a.x;
	if (y >= a.y)
		y = a.y;
	return *this;
}

template<typename T> inline _vector2<T>& _vector2<T>::max(const _vector2<T>& a)
{
	if (x < a.x)
		x = a.x;
	if (y < a.y)
		y = a.y;
	return *this;
}

template<typename T> inline _vector2<T>& _vector2<T>::add(const _vector2<T>& a)
{
	x += a.x;
	y += a.y;
	return *this;
}

template<typename T> inline _vector2<T>& _vector2<T>::sub(const _vector2<T>& a, const _vector2<T>& b)
{
	x = a.x - b.x;
	y = a.y - b.y;
	return *this;
}

template<typename T> inline _vector2<T>& _vector2<T>::mul(T s)
{
	x *= s;
	y *= s;
	return *this;
}

template<typename T> inline bool _vector2<T>::operator<(const _vector2<T>& right) const
{
	return x < right.x || (x == right.x && y < right.y);
}

template<typename T> inline bool _vector2<T>::operator==(const _vector2<T>& right) const
{
	return (x == right.x && y == right.y);
}

template<typename T> inline bool _vector2<T>::operator!=(const _vector2<T>& right) const
{
	return (x != right.x || y != right.y);
}

template<typename T> inline T& _vector2<T>::operator[](size_t i) { return xy[i]; }

template<typename T> inline const T& _vector2<T>::operator[](size_t i) const { return xy[i]; }

template<typename T> inline int _vector2<T>::compare(const _vector2<T>& right) const
{
#if 1
	return (x < right.x) ? -1 : (x == right.x ? (y < right.y ? -1 : (y == right.y ? 0 : +1)) : +1);
#else
	if (x < right.x)
		return -1;
	else if (x > right.x)
		return +1;
	if (y < right.y)
		return -1;
	else if (y > right.y)
		return +1;
	return 0;
#endif
}

} // end of namespace xray_re

#endif
