#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MATRIX_H__
#define __XR_MATRIX_H__

#include "xr_vector3.h"
#include "xr_limits.h"
#include "xr_math.h"

namespace xray_re {

template<typename T> struct _vector3;
template<typename T> struct _quaternion;

template<typename T> struct _matrix33 {
	_matrix33<T>&	set(const _matrix33<T>& a);
	_matrix33<T>&	set(const _vector3<T>& _i, const _vector3<T>& _j, const _vector3<T>& _k);
	_matrix33<T>&	identity();
	T		determinant() const;
	union {
		struct {
			T		_11, _12, _13;
			T		_21, _22, _23;
			T		_31, _32, _33;
		};
		struct {
			_vector3<T>	i;
			_vector3<T>	j;
			_vector3<T>	k;
		};
		T			m[3][3];
	};
};

typedef _matrix33<float> fmatrix33;
typedef _matrix33<double> dmatrix33;

template<typename T> inline
_matrix33<T>& _matrix33<T>::set(const _vector3<T>& _i, const _vector3<T>& _j, const _vector3<T>& _k)
{
	i.set(_i);
	j.set(_j);
	k.set(_k);
	return *this;
}

template<typename T> inline _matrix33<T>& _matrix33<T>::identity()
{
	i.set(1, 0, 0);
	j.set(0, 1, 0);
	k.set(0, 0, 1);
	return *this;
}

template<typename T> inline T _matrix33<T>::determinant() const
{
	T cf1 = _22*_33 - _23*_32;
	T cf2 = _21*_33 - _23*_31;
	T cf3 = _21*_32 - _22*_31;
	return _11*cf1 - _12*cf2 + _13*cf3;
}

////////////////////////////////////////////////////////////////////////////////

template<typename T> struct _matrix {
	template<typename F> _matrix<T>&	set(const _matrix<F>& a);

	_matrix<T>&	set(const _matrix33<T>& a);
	_matrix<T>&	identity();
	bool		is_identity() const;
	_matrix<T>&	mk_xform(const _quaternion<T>& q, const _vector3<T>& v);
	_matrix<T>&	rotation(const _quaternion<T>& q);
	_matrix<T>&	translation(T x, T y, T z);
	_matrix<T>&	translation(const _vector3<T>& v);
	_matrix<T>&	mul(const _matrix<T>& a, const _matrix<T>& b);
	_matrix<T>&	mul_43(const _matrix<T>& a, const _matrix<T>& b);
	_matrix<T>&	mul_a_43(const _matrix<T>& a);
	_matrix<T>&	mul_b_43(const _matrix<T>& b);
	_matrix<T>&	invert_43();
	_matrix<T>&	invert_43(const _matrix<T>& a);
	_matrix<T>&	set_hpb(T h, T p, T b);		// not HPB (YXZ), but ZXY really?
	_matrix<T>&	set_hpb(const _vector3<T>& v);
	_matrix<T>&	set_xyz(T x, T y, T z);		// not XYZ, but reordered HPB (
	_matrix<T>&	set_xyz(const _vector3<T>& v);
	_matrix<T>&	set_xyz_i(T x, T y, T z);
	_matrix<T>&	set_xyz_i(const _vector3<T>& v);
	void		get_hpb(T& h, T& p, T& b) const;
	void		get_hpb(_vector3<T>& v) const;
	void		get_xyz(T& x, T& y, T& z) const;
	void		get_xyz(_vector3<T>& v) const;
	void		get_xyz_i(T& x, T& y, T& z) const;
	void		get_xyz_i(_vector3<T>& v) const;

	// gr1ph starts
	bool		can_invert_43(void) const;
	//gr1ph ends

	// these will operate with "canonical" euler angles
	_matrix<T>&	set_euler_xyz(T x, T y, T z);
	_matrix<T>&	set_euler_xyz(const _vector3<T>& v);
	void		get_euler_xyz(T& x, T& y, T& z) const;
	void		get_euler_xyz(_vector3<T>& v) const;

	union {
		struct {
			T		_11, _12, _13, _14;
			T		_21, _22, _23, _24;
			T		_31, _32, _33, _34;
			T		_41, _42, _43, _44;
		};
		struct {
			_vector3<T>	i;
			T		_14_dummy;
			_vector3<T>	j;
			T		_24_dummy;
			_vector3<T>	k;
			T		_34_dummy;
			_vector3<T>	c;
			T		_44_dummy;
		};
		T			m[4][4];
		T			__array[16];
	};
};

typedef _matrix<float> fmatrix;
typedef _matrix<double> dmatrix;

template<typename T> template<typename F> inline _matrix<T>& _matrix<T>::set(const _matrix<F>& a)
{
	for (uint_fast32_t i = 16; i != 0;) {
		--i;
		__array[i] = T(a.__array[i]);
	}
	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::set(const _matrix33<T>& a)
{
	i.set(a.i); _14 = 0;
	j.set(a.j); _24 = 0;
	k.set(a.k); _34 = 0;
	c.set(0, 0, 0); _44 = 1;
	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::identity()
{
	i.set(1, 0, 0); _14 = 0;
	j.set(0, 1, 0); _24 = 0;
	k.set(0, 0, 1); _34 = 0;
	c.set(0, 0, 0); _44 = 1;
	return *this;
}

template<typename T> inline bool _matrix<T>::is_identity() const
{
	return _11 == 1 && _12 == 0 && _13 == 0 && _14 == 0 &&
			_21 == 0 && _22 == 1 && _23 == 0 && _24 == 0 &&
			_31 == 0 && _32 == 0 && _33 == 1 && _34 == 0 &&
			_41 == 0 && _42 == 0 && _43 == 0 && _44 == 1;
}

template<typename T> _matrix<T>& _matrix<T>::mk_xform(const _quaternion<T>& q, const _vector3<T>& v)
{
	T wx = q.w*q.x;
	T wy = q.w*q.y;
	T wz = q.w*q.z;

	T xx = q.x*q.x;
	T xy = q.x*q.y;
	T xz = q.x*q.z;

	T yy = q.y*q.y;
	T yz = q.y*q.z;
	T zz = q.z*q.z;

#if 1
	// can't assume the unit quaternion because of quantization errors.
	T s = T(2)/(xx + yy + zz + q.w*q.w);
#else
	xr_assert(equivalent<T>(q.magnitude(), 1, T(1e-3)));
	const T s = 2;
#endif

	i.set(T(1) - s*(yy + zz), s*(xy - wz), s*(xz + wy)); _14 = 0;
	j.set(s*(xy + wz), T(1) - s*(xx + zz), s*(yz - wx)); _24 = 0;
	k.set(s*(xz - wy), s*(yz + wx), T(1) - s*(xx + yy)); _34 = 0;
	c.set(v); _44 = T(1);

	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::rotation(const _quaternion<T>& q)
{
	return mk_xform(q, _vector3<T>().set());
}

template<typename T> inline _matrix<T>& _matrix<T>::translation(T x, T y, T z)
{
	identity();
	c.set(x, y, z);
	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::translation(const _vector3<T>& v)
{
	return translation(v.x, v.y, v.z);
}

template<typename T> _matrix<T>& _matrix<T>::mul(const _matrix<T>& a, const _matrix<T>& b)
{
	assert(this != &a && this != &b);
	_11 = a._11*b._11 + a._21*b._12 + a._31*b._13 + a._41*b._14;
	_12 = a._12*b._11 + a._22*b._12 + a._32*b._13 + a._42*b._14;
	_13 = a._13*b._11 + a._23*b._12 + a._33*b._13 + a._43*b._14;
	_14 = a._14*b._11 + a._24*b._12 + a._34*b._13 + a._44*b._14;

	_21 = a._11*b._21 + a._21*b._22 + a._31*b._23 + a._41*b._24;
	_22 = a._12*b._21 + a._22*b._22 + a._32*b._23 + a._42*b._24;
	_23 = a._13*b._21 + a._23*b._22 + a._33*b._23 + a._43*b._24;
	_24 = a._14*b._21 + a._24*b._22 + a._34*b._23 + a._44*b._24;

	_31 = a._11*b._31 + a._21*b._32 + a._31*b._33 + a._41*b._34;
	_32 = a._12*b._31 + a._22*b._32 + a._32*b._33 + a._42*b._34;
	_33 = a._13*b._31 + a._23*b._32 + a._33*b._33 + a._43*b._34;
	_34 = a._14*b._31 + a._24*b._32 + a._34*b._33 + a._44*b._34;

	_41 = a._11*b._41 + a._21*b._42 + a._31*b._43 + a._41*b._44;
	_42 = a._12*b._41 + a._22*b._42 + a._32*b._43 + a._42*b._44;
	_43 = a._13*b._41 + a._23*b._42 + a._33*b._43 + a._43*b._44;
	_44 = a._14*b._41 + a._24*b._42 + a._34*b._43 + a._44*b._44;

	return *this;
}

template<typename T> _matrix<T>& _matrix<T>::mul_43(const _matrix<T>& a, const _matrix<T>& b)
{
	assert(this != &a && this != &b);
	_11 = a._11*b._11 + a._21*b._12 + a._31*b._13;
	_12 = a._12*b._11 + a._22*b._12 + a._32*b._13;
	_13 = a._13*b._11 + a._23*b._12 + a._33*b._13;
	_14 = 0;

	_21 = a._11*b._21 + a._21*b._22 + a._31*b._23;
	_22 = a._12*b._21 + a._22*b._22 + a._32*b._23;
	_23 = a._13*b._21 + a._23*b._22 + a._33*b._23;
	_24 = 0;

	_31 = a._11*b._31 + a._21*b._32 + a._31*b._33;
	_32 = a._12*b._31 + a._22*b._32 + a._32*b._33;
	_33 = a._13*b._31 + a._23*b._32 + a._33*b._33;
	_34 = 0;

	_41 = a._11*b._41 + a._21*b._42 + a._31*b._43 + a._41;
	_42 = a._12*b._41 + a._22*b._42 + a._32*b._43 + a._42;
	_43 = a._13*b._41 + a._23*b._42 + a._33*b._43 + a._43;
	_44 = 1;

	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::mul_a_43(const _matrix<T>& a)
{
	return mul_43(a, _matrix<T>().set(*this));
}

template<typename T> inline _matrix<T>& _matrix<T>::mul_b_43(const _matrix<T>& b)
{
	return mul_43(_matrix<T>().set(*this), b);
}

template<typename T> bool _matrix<T>::can_invert_43(void) const
{
	T cf1 = _22*_33 - _23*_32;
	T cf2 = _21*_33 - _23*_31;
	T cf3 = _21*_32 - _22*_31;
	T det = _11*cf1 - _12*cf2 + _13*cf3;
	
	return (!equivalent<T>(det, 0, 0.0000001f));
}

template<typename T> _matrix<T>& _matrix<T>::invert_43(const _matrix<T>& a)
{
	T cf1 = a._22*a._33 - a._23*a._32;
	T cf2 = a._21*a._33 - a._23*a._31;
	T cf3 = a._21*a._32 - a._22*a._31;

	T det = a._11*cf1 - a._12*cf2 + a._13*cf3;
	
	//xr_assert(!equivalent<T>(det, 0));
	// 1850 xform asserted
	xr_assert(!equivalent<T>(det, 0, 0.0000001f));

	_11 = cf1/det;
	_21 =-cf2/det;
	_31 = cf3/det;

//	_11 = (a._22*a._33 - a._23*a._32)/det;
	_12 =-(a._12*a._33 - a._13*a._32)/det;
	_13 = (a._12*a._23 - a._13*a._22)/det;
	_14 = 0;

//	_21 =-(a._21*a._33 - a._23*a._31)/det;
	_22 = (a._11*a._33 - a._13*a._31)/det;
	_23 =-(a._11*a._23 - a._13*a._21)/det;
	_24 = 0;

//	_31 = (a._21*a._32 - a._22*a._31)/det;
	_32 =-(a._11*a._32 - a._12*a._31)/det;
	_33 = (a._11*a._22 - a._12*a._21)/det;
	_34 = 0;

	_41 =-(a._41*_11 + a._42*_21 + a._43*_31);
	_42 =-(a._41*_12 + a._42*_22 + a._43*_32);
	_43 =-(a._41*_13 + a._42*_23 + a._43*_33);
	_44 = 1;

	return *this;
}

template<typename T> inline _matrix<T>& _matrix<T>::invert_43()
{
	return invert_43(_matrix<T>().set(*this));
}

template<typename T> inline _matrix<T>& _matrix<T>::set_hpb(T h, T p, T b)
{
	T sh = std::sin(h);
	T ch = std::cos(h);
	T sp = std::sin(p);
	T cp = std::cos(p);
	T sb = std::sin(b);
	T cb = std::cos(b);

#if 1
	_11 = ch*cb - sh*sp*sb;
	_12 = -cp*sb;
	_13 = ch*sb*sp + sh*cb;
	_14 = 0;

	_21 = sp*sh*cb + ch*sb;
	_22 = cb*cp;
	_23 = sh*sb - sp*ch*cb;
	_24 = 0;

	_31 = -cp*sh;
	_32 = sp;
	_33 = ch*cp;
	_34 = 0;

	_41 = 0;
	_42 = 0;
	_43 = 0;
	_44 = T(1);
#else
	i.set(ch*cb - sh*sp*sb, -cp*sb, ch*sb*sp + sh*cb); _14 = 0;
	j.set(sp*sh*cb + ch*sb, cb*cp, sh*sb - sp*ch*cb); _24 = 0;
	k.set(-cp*sh, sp, ch*cp); _34 = 0;
	c.set(0, 0, 0); _44 = T(1);
#endif

	return *this;
}

template<typename T> inline
_matrix<T>& _matrix<T>::set_hpb(const _vector3<T>& v) { return set_hpb(v.x, v.y, v.z); }

template<typename T> inline
_matrix<T>& _matrix<T>::set_xyz(T x, T y, T z) { return set_hpb(y, x, z); }

template<typename T> inline
_matrix<T>& _matrix<T>::set_xyz(const _vector3<T>& v) { return set_hpb(v.y, v.x, v.z);}

template<typename T> inline
_matrix<T>& _matrix<T>::set_xyz_i(T x, T y, T z) { return set_hpb(-y, -x, -z); }

template<typename T> inline
_matrix<T>& _matrix<T>::set_xyz_i(const _vector3<T>& v) { return set_hpb(-v.y, -v.x, -v.z); }

template<typename T> void _matrix<T>::get_hpb(T& h, T& p, T& b) const
{
	T cy = std::sqrt(_12*_12 + _22*_22);
	if (cy > 16*xr_numeric_limits<T>::epsilon()) {
		h = -std::atan2(_31, _33);
		p = -std::atan2(-_32, cy);
		b = -std::atan2(_12, _22);
	} else {
		h = -std::atan2(-_13, _11);
		p = -std::atan2(-_32, cy);
		b = 0;
	}
}

template<typename T> inline
void _matrix<T>::get_hpb(_vector3<T>& v) const { get_hpb(v.x, v.y, v.z); }

template<typename T> inline
void _matrix<T>::get_xyz(T& x, T& y, T& z) const { get_hpb(y, x, z); }

template<typename T> inline
void _matrix<T>::get_xyz(_vector3<T>& v) const { get_hpb(v.y, v.x, v.z); }

template<typename T> inline void _matrix<T>::get_xyz_i(T& x, T& y, T& z) const
{
	get_hpb(y, x, z);
	x = -x;
	y = -y;
	z = -z;
}

template<typename T> inline void _matrix<T>::get_xyz_i(_vector3<T>& v) const
{
	get_hpb(v.y, v.x, v.z);
	v.invert();
}

// ZXY: i=2, j=1, k=3
// XYZ: i=3, j=2, k=1
// _11 -> _22
// _12 -> _23
// _13 -> _21
// _21 -> _32
// _22 -> _33
// _23 -> _31
// _31 -> _12
// _32 -> _13
// _33 -> _11
template<typename T> inline _matrix<T>& _matrix<T>::set_euler_xyz(T x, T y, T z)
{
	x = -x;
	y = -y;
	z = -z;

	T sx = std::sin(z);
	T cx = std::cos(z);
	T sy = std::sin(y);
	T cy = std::cos(y);
	T sz = std::sin(x);
	T cz = std::cos(x);

	_11 = cx*cy;
	_12 = -cy*sx;
	_13 = sy;
	_14 = 0;

	_21 = cx*sz*sy + sx*cz;
	_22 = cx*cz - sx*sy*sz;
	_23 = -cy*sz;
	_24 = 0;

	_31 = sx*sz - sy*cx*cz;
	_32 = sy*sx*cz + cx*sz;
	_33 = cz*cy;
	_34 = 0;

	_41 = 0;
	_42 = 0;
	_43 = 0;
	_44 = T(1);

	return *this;
}

template<typename T> inline
_matrix<T>& _matrix<T>::set_euler_xyz(const _vector3<T>& v) { return set_euler_xyz(v.x, v.y, v.z);}

// _11 -> _22
// _12 -> _23
// _13 -> _21
// _22 -> _33
// _31 -> _12
// _32 -> _13
// _33 -> _11
template<typename T> inline void _matrix<T>::get_euler_xyz(T& x, T& y, T& z) const
{
	T cy = std::sqrt(_23*_23 + _33*_33);
	if (cy > 16*xr_numeric_limits<T>::epsilon()) {
		z = std::atan2(_12, _11);
		y = std::atan2(-_13, cy);
		x = std::atan2(_23, _33);
	} else {
		z = std::atan2(-_21, _22);
		y = std::atan2(-_13, cy);
		x = 0;
	}
}

template<typename T> inline
void _matrix<T>::get_euler_xyz(_vector3<T>& v) const { get_euler_xyz(v.x, v.y, v.z); }

} // end of namespace xray_re

#endif
