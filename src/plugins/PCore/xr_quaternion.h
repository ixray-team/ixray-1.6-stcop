#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_QUATERNION_H__
#define __XR_QUATERNION_H__

namespace xray_re {

template<typename T> struct _vector3;
template<typename T> struct _matrix;

template<typename T> struct _quaternion {
	_quaternion<T>&		identity();
	_quaternion<T>&		set(const _matrix<T>& m);
	_quaternion<T>&		set(const _quaternion<T>& a);
	_quaternion<T>&		set(T _x, T _y, T _z, T _w);
	_quaternion<T>&		mul(T s);
	_quaternion<T>&		div(T s);
	_quaternion<T>&		normalize();
	_quaternion<T>&		rotation(const _vector3<T>& axis, T theta);
	_quaternion<T>&		rotation_ypr(T _y, T _p, T _r);
	_quaternion<T>&		rotation(const _vector3<T>& v1, const _vector3<T>& v2);
	_quaternion<T>&		slerp(const _quaternion<T>& q1, const _quaternion<T>& q2, T t);
	T			magnitude() const;
	T			dot_product(const _quaternion<T>& q) const;
	bool			similar(const _quaternion<T>& a, T e = T(1e-6)) const;

	union {
		struct {
			T	x, y, z, w;
		};
		_vector3<T>	xyz;
	};
};

typedef _quaternion<float> fquaternion;
typedef _quaternion<double> dquaternion;

template<typename T> inline _quaternion<T>& _quaternion<T>::identity()
{
	x = 0;
	y = 0;
	z = 0;
	w = T(1);
	return *this;
}

template<typename T> inline _quaternion<T>& _quaternion<T>::set(T _x, T _y, T _z, T _w)
{
	x = _x;
	y = _y;
	z = _z;
	w = _w;
	return *this;
}

template<typename T> _quaternion<T>& _quaternion<T>::set(const _matrix<T>& m)
{
	T trace = m._11 + m._22 + m._33;
	if (trace > 0) {
		T s = std::sqrt(trace + T(1));
		w = s*T(0.5);
		xr_assert(s > T(1e-6));
		s = T(0.5)/s;
		x = s*(m._32 - m._23);
		y = s*(m._13 - m._31);
		z = s*(m._21 - m._12);
	} else {
		int i = 0;
		if (m._22 > m._11)
			i = 1;
		if (m._33 > m.m[i][i])
			i = 2;
		int j = (1 << i)&3, k = (1 << j)&3;
//		int j = (i + 1)%3, k = (j + 1)%3;
		T s = std::sqrt(m.m[i][i] - m.m[j][j] - m.m[k][k] + T(1));
		xyz[i] = s*T(0.5);
		xr_assert(s > T(1e-6));
		s = T(0.5)/s;
		w = s*(m.m[k][j] - m.m[j][k]);
		xyz[j] = s*(m.m[j][i] + m.m[i][j]);
		xyz[k] = s*(m.m[k][i] + m.m[i][k]);
	}
	return *this;
}

template<typename T> inline _quaternion<T>& _quaternion<T>::set(const _quaternion<T>& a)
{
	x = a.x;
	y = a.y;
	z = a.z;
	w = a.w;
	return *this;
}

template<typename T> inline _quaternion<T>& _quaternion<T>::mul(T s)
{
	x *= s;
	y *= s;
	z *= s;
	w *= s;
	return *this;
}

template<typename T> inline _quaternion<T>& _quaternion<T>::div(T s)
{
	x /= s;
	y /= s;
	z /= s;
	w /= s;
	return *this;
}

template<typename T> inline
_quaternion<T>& _quaternion<T>::normalize() { return div(magnitude()); }

template<typename T> inline
_quaternion<T>& _quaternion<T>::rotation(const _vector3<T>& axis, T theta)
{
	w = std::cos(theta *= T(0.5));
	xyz.mul(axis, std::sin(theta));
	return *this;
}

template<typename T> inline
_quaternion<T>& _quaternion<T>::rotation(const _vector3<T>& v1, const _vector3<T>& v2)
{
	_vector3<T> m;
	if ((w = v1.dot_product(m.add(v1, v2).normalize_safe())) == 0) {
		if (std::abs(v1.x) > std::abs(v1.y)) {
			T inv_l = 1/std::sqrt(v1.x*v1.x + v1.z*v1.z);
			x = -v1.z*inv_l;
			y = 0;
			z = v1.x*inv_l;
		} else {
			T inv_l = 1/std::sqrt(v1.y*v1.y + v1.z*v1.z);
			x = 0;
			y = v1.z*inv_l;
			z = -v1.y*inv_l;
		}
	} else {
		xyz.cross_product(v1, m);
	}
	return *this;
}

template<typename T>
_quaternion<T>& _quaternion<T>::slerp(const _quaternion<T>& q1, const _quaternion<T>& q2, T t)
{
	T omega = std::acos(q1.dot_product(q2));
	if (!equivalent<T>(omega, 0)) {
		T sin_omega = T(1)/std::sin(omega);
		T k1 = std::sin((T(1) - t)*omega)/sin_omega;
		T k2 = std::sin(t*omega)/sin_omega;
		x = k1*q1.x + k2*q2.x;
		y = k1*q1.y + k2*q2.y;
		z = k1*q1.z + k2*q2.z;
		w = k1*q1.w + k2*q2.w;
		return *this;
	} else {
		return set(q1);
	}
}

template<typename T> inline
T _quaternion<T>::magnitude() const { return std::sqrt(x*x + y*y + z*z + w*w); }

template<typename T> inline
T _quaternion<T>::dot_product(const _quaternion<T>& q) const { return x*q.x + y*q.y + z*q.z + w*q.w; }

template<typename T> bool _quaternion<T>::similar(const _quaternion<T>& a, T e) const
{
	if (e <= std::abs(a.x - x))
		return false;
	if (e <= std::abs(a.y - y))
		return false;
	if (e <= std::abs(a.z - z))
		return false;
	if (e <= std::abs(a.w - w))
		return false;
	return true;
}

} // end of namespace xray_re

#endif
