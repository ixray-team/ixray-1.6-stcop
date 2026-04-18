#pragma once

template <class T>
struct _vector4 
{
	typedef T			TYPE;
	typedef _vector4<T>	Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;
public:
	T x = 0;
	T y = 0;
	T z = 0;
	T w = 0;

	ICF	T&			operator[] (int i)					{ return *((T*)this + i); }
	ICF	T&			operator[] (int i)	const			{ return *((T*)this + i); }

	ICF	SelfRef 	set(T _x, T _y, T _z, T _w=1)		{ x=_x;		y=_y;		z=_z;		w=_w;		return *this; }
	ICF	SelfRef		set(const Self& v)					{ x=v.x;	y=v.y;		z=v.z;		w=v.w;		return *this; }

	ICF	SelfRef		add(const Self &v)					{ x+=v.x;	y+=v.y;		z+=v.z;		w+=v.w;		return *this; }
	ICF  SelfRef		add(T s)							{ x+=s;		y+=s;		z+=s;		w+=s;		return *this; }
	ICF	SelfRef		add(const Self &a, const Self &v)	{ x=a.x+v.x;y=a.y+v.y;	z=a.z+v.z;	w=a.w+v.w;	return *this; }
	ICF  SelfRef		add(const Self &a, T s)				{ x=a.x+s;  y=a.y+s;	z=a.z+s;	w=a.w+s;	return *this; }

	ICF	SelfRef		sub(T _x, T _y, T _z, T _w=1)		{ x-=_x;	y-=_y;		z-=_z;		w-=_w;		return *this; }
	ICF	SelfRef		sub(const Self &v)					{ x-=v.x;	y-=v.y;		z-=v.z;		w-=v.w;		return *this; }
	ICF  SelfRef		sub(T s)							{ x-=s;		y-=s;		z-=s;		w-=s;		return *this; }
	ICF	SelfRef		sub(const Self &a, const Self &v)	{ x=a.x-v.x;y=a.y-v.y;	z=a.z-v.z;	w=a.w-v.w;	return *this; }
	ICF  SelfRef		sub(const Self &a, T s)				{ x=a.x-s;  y=a.y-s;	z=a.z-s;	w=a.w-s;	return *this; }

	ICF	SelfRef 	mul(T _x, T _y, T _z, T _w=1)		{ x*=_x;	y*=_y;		z*=_z;		w*=_w;		return *this; }
	ICF	SelfRef		mul(const Self &v)					{ x*=v.x;	y*=v.y;		z*=v.z;		w*=v.w;		return *this; }
	ICF  SelfRef		mul(T s)							{ x*=s;		y*=s;		z*=s;		w*=s;		return *this; }
	ICF	SelfRef		mul(const Self &a, const Self &v)	{ x=a.x*v.x;y=a.y*v.y;	z=a.z*v.z;	w=a.w*v.w;	return *this; }
	ICF  SelfRef		mul(const Self &a, T s)				{ x=a.x*s;  y=a.y*s;	z=a.z*s;	w=a.w*s;	return *this; }

	ICF	SelfRef		div(const Self &v)					{ x/=v.x;	y/=v.y;		z/=v.z;		w/=v.w;		return *this; }
	ICF  SelfRef		div(T s)							{ x/=s;		y/=s;		z/=s;		w/=s;		return *this; }
	ICF	SelfRef		div(const Self &a, const Self &v)	{ x=a.x/v.x;y=a.y/v.y;	z=a.z/v.z;	w=a.w/v.w;	return *this; }
	ICF  SelfRef		div(const Self &a, T s)				{ x=a.x/s;  y=a.y/s;	z=a.z/s;	w=a.w/s;	return *this; }

	ICF	bool 		similar(const Self& v, T E=EPS_L)	{ return std::abs(x-v.x)<E && std::abs(y-v.y)<E && std::abs(z-v.z)<E && std::abs(w-v.w)<E;};

	ICF	T			magnitude_sqr ()					{ return x*x + y*y + z*z + w*w;		}
	ICF	T			magnitude()							{ return _sqrt(magnitude_sqr());	}
	ICF	SelfRef		normalize()							{ return mul(1/magnitude());		}

	ICF	SelfRef		normalize_as_plane()				{ return mul(1/_sqrt(x*x + y*y + z*z));	}

	ICF	SelfRef		lerp(const Self &p1, const Self &p2, T t )
	{
		T invt = 1.f-t;
		x = p1.x*invt + p2.x*t;
		y = p1.y*invt + p2.y*t;
		z = p1.z*invt + p2.z*t;
		w = p1.w*invt + p2.w*t;
		return *this;	
	}
};

typedef							_vector4<float>		Fvector4;
typedef							_vector4<double>	Dvector4;
typedef							_vector4<s32>		Ivector4;

typedef __declspec(align(16))	_vector4<float>		Fvector4a;
typedef __declspec(align(16))	_vector4<double>	Dvector4a;
typedef __declspec(align(16))	_vector4<s32>		Ivector4a;

template <class T>
ICF bool	_valid			(const _vector4<T>& v)	{ return _valid((T)v.x) && _valid((T)v.y) && _valid((T)v.z) && _valid((T)v.w);	}