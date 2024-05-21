#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_VECTOR3_H__
#define __XR_VECTOR3_H__

#include "xr_types.h"
#include "xr_math.h"

namespace xray_re {

template<typename T> struct _quaternion;
template<typename T> struct _matrix;

template<typename T> struct _vector3 {
	template<typename F> _vector3<T>&	set(const _vector3<F>& v);

	_vector3<T>&	set(T _x = T(), T _y = T(), T _z = T());
	_vector3<T>&	set(const T _xyz[3]);
	void		get(T _xyz[3]) const;
	T		square_magnitude() const;
	T		magnitude() const;
	T		distance(const _vector3<T>& v) const;
	_vector3<T>&	set_magnitude(T l);
	_vector3<T>&	normalize();
	_vector3<T>&	normalize_safe();
	T		normalize_magn();
	_vector3<T>&	abs();
	_vector3<T>&	abs(const _vector3<T>& v);
	_vector3<T>&	min(const _vector3<T>& v);
	_vector3<T>&	max(const _vector3<T>& v);
	_vector3<T>&	sub(const _vector3<T>& a, const _vector3<T>& b);
	_vector3<T>&	sub(const _vector3<T>& a);
	_vector3<T>&	sub(const _vector3<T>& a, T s);
	_vector3<T>&	sub(T s);
	_vector3<T>&	add(const _vector3<T>& a);
	_vector3<T>&	add(const _vector3<T>& a, const _vector3<T>& b);
	_vector3<T>&	add(const _vector3<T>& a, T s);
	_vector3<T>&	add(T s);
	_vector3<T>&	mul(T s);
	_vector3<T>&	mul(const _vector3<T>& a, T s);
	_vector3<T>&	mul(const _vector3<T>& a, const _vector3<T>& s);
	_vector3<T>&	mul(const _vector3<T>& s);
	_vector3<T>&	div(T s);
	_vector3<T>&	mad(const _vector3<T>& p, const _vector3<T>& d, T s);
	_vector3<T>&	lerp(const _vector3<T>& a, const _vector3<T>& b, T t);
	_vector3<T>&	invert();
	_vector3<T>&	invert(const _vector3<T>& v);
	_vector3<T>&	rotate(const _quaternion<T>& q);
	_vector3<T>&	rotate(const _vector3<T>& v, const _quaternion<T>& q);
	_vector3<T>&	rotate(const _matrix<T>& m);
	_vector3<T>&	rotate(const _vector3<T>& v, const _matrix<T>& m);
	_vector3<T>&	transform(const _matrix<T>& m);
	_vector3<T>&	transform(const _vector3<T>& v, const _matrix<T>& m);
	_vector3<T>&	cross_product(const _vector3<T>& v1, const _vector3<T>& v2);
	_vector3<T>&	cross_product(const _vector3<T>& v);
	_vector3<T>&	calc_normal(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2);
	_vector3<T>&	calc_normal_non_normalized(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2);
	_vector3<T>&	calc_center(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2);
	_vector3<T>&	inertion(const _vector3<T>& p, float v);
	_vector3<T>&	average(const _vector3<T>& a);
	_vector3<T>&	average(const _vector3<T>& a, const _vector3<T>& b);
	_vector3<T>&	reflect(const _vector3<T>& dir, const _vector3<T>& norm);
	_vector3<T>&	slide(const _vector3<T>& dir, const _vector3<T>& norm);
	T		dot_product(const _vector3<T>& v) const;
	T		cosine(const _vector3<T>& v) const;
	bool		similar(const _vector3<T>& v, T e = T(1e-6)) const;
	bool		inverted(const _vector3<T>& v, T e = T(1e-6)) const;
	int		compare(const _vector3<T>& right) const;
	bool		operator<(const _vector3<T>& right) const;
	bool		operator>(const _vector3<T>& right) const;
	bool		operator==(const _vector3<T>& right) const;
	bool		operator!=(const _vector3<T>& right) const;
	T&		operator[](size_t i);
	const T&	operator[](size_t i) const;

	_vector3<T>&	decompress(uint16_t);
	uint16_t	compress() const;

	union {
		struct {
			T	x, y, z;
		};
		T		xyz[3];
	};
};

typedef _vector3<double> dvector3;
typedef _vector3<float> fvector3;
typedef _vector3<int32_t> i32vector3;
typedef _vector3<int16_t> i16vector3;
typedef _vector3<uint16_t> u16vector3;

template<typename T> template<typename F> inline _vector3<T>& _vector3<T>::set(const _vector3<F>& v)
{
	x = T(v.x);
	y = T(v.y);
	z = T(v.z);
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::set(T _x, T _y, T _z)
{
	x = _x;
	y = _y;
	z = _z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::set(const T _xyz[3])
{
	x = _xyz[0];
	y = _xyz[1];
	z = _xyz[2];
	return *this;
}

template<typename T> inline void _vector3<T>::get(T _xyz[3]) const
{
	_xyz[0] = x;
	_xyz[1] = y;
	_xyz[2] = z;
}

template<typename T> inline T _vector3<T>::square_magnitude() const { return x*x + y*y + z*z; }

template<typename T> inline T _vector3<T>::magnitude() const { return std::sqrt(x*x + y*y + z*z); }

template<typename T> inline T _vector3<T>::distance(const _vector3<T>& v) const
{
	T dx = v.x - x, dy = v.y - y, dz = v.z - z;
	return std::sqrt(dx*dx + dy*dy + dz*dz);
}

template<typename T> inline _vector3<T>& _vector3<T>::set_magnitude(T m) { return mul(m/magnitude()); }

template<typename T> inline _vector3<T>& _vector3<T>::normalize() { return div(magnitude()); }

template<typename T> inline _vector3<T>& _vector3<T>::normalize_safe()
{
	T m = magnitude();
	return (m > T(1e-6)) ? div(m) : set(0, 0, 0);
}

template<typename T> inline T _vector3<T>::normalize_magn()
{
	T m = magnitude();
	div(m);
	return m;
}

template<typename T> inline _vector3<T>& _vector3<T>::abs()
{
	if (x < 0)
		x = -x;
	if (y < 0)
		y = -y;
	if (z < 0)
		z = -z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::abs(const _vector3<T>& v)
{
	x = std::abs(v.x);
	y = std::abs(v.y);
	z = std::abs(v.z);
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::min(const _vector3<T>& v)
{
	if (x >= v.x)
		x = v.x;
	if (y >= v.y)
		y = v.y;
	if (z >= v.z)
		z = v.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::max(const _vector3<T>& v)
{
	if (x < v.x)
		x = v.x;
	if (y < v.y)
		y = v.y;
	if (z < v.z)
		z = v.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::sub(const _vector3<T>& a, const _vector3<T>& b)
{
	x = a.x - b.x;
	y = a.y - b.y;
	z = a.z - b.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::sub(const _vector3<T>& a)
{
	x -= a.x;
	y -= a.y;
	z -= a.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::sub(const _vector3<T>& a, T s)
{
	x = a.x - s;
	y = a.y - s;
	z = a.z - s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::sub(T s)
{
	x -= s;
	y -= s;
	z -= s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::add(const _vector3<T>& a, const _vector3<T>& b)
{
	x = a.x + b.x;
	y = a.y + b.y;
	z = a.z + b.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::add(const _vector3<T>& a)
{
	x += a.x;
	y += a.y;
	z += a.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::add(const _vector3<T>& a, T s)
{
	x = a.x + s;
	y = a.y + s;
	z = a.z + s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::add(T s)
{
	x += s;
	y += s;
	z += s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::mul(T s)
{
	x *= s;
	y *= s;
	z *= s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::mul(const _vector3<T>& v, T s)
{
	x = v.x * s;
	y = v.y * s;
	z = v.z * s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::mul(const _vector3<T>& a, const _vector3<T>& s)
{
	x = a.x * s.x;
	y = a.y * s.y;
	z = a.z * s.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::mul(const _vector3<T>& s)
{
	x *= s.x;
	y *= s.y;
	z *= s.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::div(T s)
{
	// no reciprocal for better precision
	x /= s;
	y /= s;
	z /= s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::mad(const _vector3<T>& p, const _vector3<T>& d, T s)
{
	x = p.x + d.x*s;
	y = p.y + d.y*s;
	z = p.z + d.z*s;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::lerp(const _vector3<T>& a, const _vector3<T>& b, T t)
{
	T w = 1 - t;
	x = a.x*w + b.x*t;
	y = a.y*w + b.y*t;
	z = a.z*w + b.z*t;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::invert()
{
	x = -x;
	y = -y;
	z = -z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::invert(const _vector3<T>& v)
{
	x = -v.x;
	y = -v.y;
	z = -v.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::rotate(const _quaternion<T>& q)
{
	T rw =-q.x*x - q.y*y - q.z*z;
	T rx = q.w*x + q.y*z - q.z*y;
	T ry = q.w*y + q.z*x - q.x*z;
	T rz = q.w*z + q.x*y - q.y*x;
	x = -rw*q.x + rx*q.w - ry*q.z + rz*q.y;
	y = -rw*q.y + ry*q.w - rz*q.x + rx*q.z;
	z = -rw*q.z + rz*q.w - rx*q.y + ry*q.x;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::rotate(const _vector3<T>& v, const _quaternion<T>& q)
{
	return set(v).rotate(q);
}

template<typename T> inline _vector3<T>& _vector3<T>::rotate(const _vector3<T>& v, const _matrix<T>& m)
{
	x = m._11*v.x + m._21*v.y + m._31*v.z;
	y = m._12*v.x + m._22*v.y + m._32*v.z;
	z = m._13*v.x + m._23*v.y + m._33*v.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::rotate(const _matrix<T>& m)
{
	T _x = m._11*x + m._21*y + m._31*z;
	T _y = m._12*x + m._22*y + m._32*z;
	z = m._13*x + m._23*y + m._33*z;
	x = _x;
	y = _y;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::transform(const _vector3<T>& v, const _matrix<T>& m)
{
	x = m._41 + m._11*v.x + m._21*v.y + m._31*v.z;
	y = m._42 + m._12*v.x + m._22*v.y + m._32*v.z;
	z = m._43 + m._13*v.x + m._23*v.y + m._33*v.z;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::transform(const _matrix<T>& m)
{
	T _x = m._41 + m._11*x + m._21*y + m._31*z;
	T _y = m._42 + m._12*x + m._22*y + m._32*z;
	z = m._43 + m._13*x + m._23*y + m._33*z;
	x = _x;
	y = _y;
	return *this;
}

template<typename T> inline
_vector3<T>& _vector3<T>::calc_normal(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
#if 0
//	x = v1.y*v2.z - v1.z*v2.y;
//	y = v1.z*v2.x - v1.x*v2.z;
//	z = v1.x*v2.y - v1.y*v2.x;

	T x01 = p1.x - p0.x, y01 = p1.y - p0.y, z01 = p1.y - p0.y;
	T x02 = p2.x - p0.x, y02 = p2.y - p0.y, z02 = p2.y - p0.y;
	T xt = y01*z02 - 
#else
	return sub(p1, p0).cross_product(_vector3<T>().sub(p2, p0)).normalize();
#endif
}

template<typename T> inline
_vector3<T>& _vector3<T>::calc_normal_non_normalized(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	return sub(p1, p0).cross_product(_vector3<T>().sub(p2, p0));
}

template<typename T> inline
_vector3<T>& _vector3<T>::calc_center(const _vector3<T>& p0, const _vector3<T>& p1, const _vector3<T>& p2)
{
	const T k_third = T(1)/3;
	x = (p0.x + p1.x + p2.x)*k_third;
	y = (p0.y + p1.y + p2.y)*k_third;
	z = (p0.z + p1.z + p2.z)*k_third;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::cross_product(const _vector3<T>& v1, const _vector3<T>& v2)
{
	x = v1.y*v2.z - v1.z*v2.y;
	y = v1.z*v2.x - v1.x*v2.z;
	z = v1.x*v2.y - v1.y*v2.x;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::cross_product(const _vector3<T>& v)
{
	T _x = y*v.z - z*v.y;
	T _y = z*v.x - x*v.z;
	z = x*v.y - y*v.x;
	x = _x;
	y = _y;
	return *this;
}

template<typename T> inline T _vector3<T>::dot_product(const _vector3<T>& v) const
{
	return x*v.x + y*v.y + z*v.z;
}

// for non-unit length vectors
template<typename T> T _vector3<T>::cosine(const _vector3<T>& v) const
{
	return dot_product(v)/std::sqrt(square_magnitude()*v.square_magnitude());
}

template<typename T> inline _vector3<T>& _vector3<T>::inertion(const _vector3<T>& p, float v)
{
	T t = 1 - v;
	x = x*v + p.x*t;
	y = y*v + p.y*t;
	z = z*v + p.z*t;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::average(const _vector3<T>& a)
{
	const T k_half = T(1)/2;
	x = (x + a.x)*k_half;
	y = (y + a.y)*k_half;
	z = (z + a.z)*k_half;
	return *this;
}

template<typename T> inline _vector3<T>& _vector3<T>::average(const _vector3<T>& a, const _vector3<T>& b)
{
	const T k_half = T(1)/2;
	x = (a.x + b.x)*k_half;
	y = (a.y + b.y)*k_half;
	z = (a.z + b.z)*k_half;
	return *this;
}

// dir, norm - unit vectors
template<typename T> inline _vector3<T>& _vector3<T>::reflect(const _vector3<T>& dir, const _vector3<T>& norm)
{
	T t = -2*dir.dot_product(norm);
	x = norm.x*t + dir.x;
	y = norm.y*t + dir.y;
	z = norm.z*t + dir.z;
	return *this;
}

// dir, norm - unit vectors
template<typename T> inline _vector3<T>& _vector3<T>::slide(const _vector3<T>& dir, const _vector3<T>& norm)
{
	T dp = dir.dot_product(norm);
	x = dir.x - dp*norm.x;
	y = dir.y - dp*norm.y;
	z = dir.z - dp*norm.z;
	return *this;
}

template<typename T> bool _vector3<T>::similar(const _vector3<T>& v, T e) const
{
	if (e <= std::abs(v.x - x))
		return false;
	if (e <= std::abs(v.y - y))
		return false;
	if (e <= std::abs(v.z - z))
		return false;
	return true;
}

template<typename T> bool _vector3<T>::inverted(const _vector3<T>& v, T e) const
{
	if (e <= std::abs(v.x + x))
		return false;
	if (e <= std::abs(v.y + y))
		return false;
	if (e <= std::abs(v.z + z))
		return false;
	return true;
}

template<typename T> inline int _vector3<T>::compare(const _vector3<T>& right) const
{
//	return (x < right.x) ? -1 : (x == right.x && (y < right.y || (y == right.y && z < right.z)));
	if (x < right.x)
		return -1;
	else if (x > right.x)
		return +1;
	if (y < right.y)
		return -1;
	else if (y > right.y)
		return +1;
	if (z < right.z)
		return -1;
	else if (z > right.z)
		return +1;
	return 0;
}

template<typename T> inline bool _vector3<T>::operator<(const _vector3<T>& right) const
{
	return x < right.x || (x == right.x && (y < right.y || (y == right.y && z < right.z)));
}

template<typename T> inline bool _vector3<T>::operator>(const _vector3<T>& right) const
{
	return x > right.x || (x == right.x && (y > right.y || (y == right.y && z > right.z)));
}

template<typename T> inline bool _vector3<T>::operator==(const _vector3<T>& right) const
{
	return (x == right.x && y == right.y && z == right.z);
}

template<typename T> inline bool _vector3<T>::operator!=(const _vector3<T>& right) const
{
	return x != right.x || y != right.y || z != right.z;
}

template<typename T> inline T& _vector3<T>::operator[](size_t i) { return xyz[i]; }

template<typename T> inline const T& _vector3<T>::operator[](size_t i) const { return xyz[i]; }

} // end of namespace xray_re

#endif
