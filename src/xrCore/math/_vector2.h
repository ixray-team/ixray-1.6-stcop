#ifndef __V2D__
#define __V2D__

template <class T>
struct _vector2 
{
	using TYPE = T;
	using Self = _vector2<T>;
	using SelfRef = Self&;
	using SelfCRef = const Self&;

	T x = 0;
	T y = 0;

	ICF  SelfRef set(float _u, float _v)			{ x=T(_u); y=T(_v);								return *this;	}
	ICF  SelfRef set(double _u, double _v)			{ x=T(_u); y=T(_v);								return *this;	}
	ICF  SelfRef set(int _u, int _v)				{ x=T(_u); y=T(_v);								return *this;	}
	ICF  SelfRef set(const Self &p)					{ x=p.x; y=p.y;									return *this;	}
	ICF  SelfRef abs(const Self &p)					{ x=std::abs(p.x);   y=std::abs(p.y);			return *this;	}
	ICF  SelfRef min(const Self &p)					{ x=std::min(x,p.x); y=std::min(y,p.y);			return *this;	}
	ICF  SelfRef min(T _x, T _y)					{ x=std::min(x,_x);  y=std::min(y,_y);			return *this;	}
	ICF  SelfRef max(const Self &p)					{ x=std::max(x,p.x); y=std::max(y,p.y);			return *this;	}
	ICF  SelfRef max(T _x, T _y)					{ x=std::max(x,_x);  y=std::max(y,_y);			return *this;	}
	ICF  SelfRef sub(const T p)						{ x-=p; y-=p;									return *this;	}
	ICF  SelfRef sub(const Self &p)					{ x-=p.x; y-=p.y;								return *this;	}
	ICF  SelfRef sub(const Self &p1, const Self &p2){ x=p1.x-p2.x; y=p1.y-p2.y;						return *this;	}
	ICF  SelfRef sub(const Self &p, float d)		{ x=p.x-d; y=p.y-d;								return *this;	}
	ICF  SelfRef add(const T p)						{ x+=p; y+=p;									return *this;	}
	ICF  SelfRef add(const Self &p)					{ x+=p.x; y+=p.y;								return *this;	}
	ICF  SelfRef add(const Self &p1, const Self &p2){ x=p1.x+p2.x; y=p1.y+p2.y;						return *this;	}
	ICF  SelfRef add(const Self &p, float d)		{ x=p.x+d; y=p.y+d;								return *this;	}
	ICF  SelfRef mul(const T s)						{ x*=s; y*=s;									return *this;	}
	ICF  SelfRef mul(const Self &p)					{ x*=p.x; y*=p.y;								return *this;	}
	ICF  SelfRef div(const T s)						{ x/=s; y/=s;									return *this;	}
	ICF  SelfRef div(const Self &p)					{ x/=p.x; y/=p.y;								return *this;	}
	ICF  SelfRef rot90(void)						{ float t=-x; x=y; y=t;							return *this;	}
	ICF  SelfRef cross(const Self &D)				{ x = D.y; y = -D.x;							return *this;	}
	ICF  T dot(Self &p)								{ return x*p.x + y*p.y;											}
	ICF  T dot(const Self &p) const					{ return x*p.x + y*p.y;											}
	ICF  SelfRef norm(void)							{ float m=_sqrt(x*x+y*y); x/=m; y/=m;			return *this;	}
	ICF  SelfRef norm_safe(void)					{ float m=_sqrt(x*x+y*y); if(m) {x/=m; y/=m;}	return *this;	}
	ICF  T distance_to(const Self &p) const 		{ return _sqrt((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y)); } 
	
	// Vector magnitude^2
	ICF T square_magnitude(void) const				{ return x*x + y*y;												}
	ICF T square_magnitude_x(void) const			{ return x*x; 													}
	ICF T square_magnitude_y(void) const			{ return y*y; 													}

	// Vector magnitude
	ICF T magnitude(void) const						{ return _sqrt(square_magnitude());								}
	ICF T magnitude_x(void) const					{ return _sqrt(square_magnitude_x());							}
	ICF T magnitude_y(void) const					{ return _sqrt(square_magnitude_y());							}

	// Interpolate vectors (inertion)
	ICF SelfRef inertion(const Self &p, T v)
	{
		T inv = T(1) - v;
		x = v*x + inv*p.x;
		y = v*y + inv*p.y;
		return *this;
	}

	// Lerp
	ICF SelfRef lerp(const Self &p1, const Self &p2, T t)
	{
		T invt = T(1) - t;
		x = p1.x*invt + p2.x*t;
		y = p1.y*invt + p2.y*t;
		return *this;
	}

	ICF SelfRef mad(const Self &p, const Self& d, T r) 
	{
		x = p.x + d.x*r;
		y = p.y + d.y*r;
		return *this;
	}
	
	ICF Self Cross()
	{
		// vector3 orthogonal to (x,y) is (y,-x)
		Self kCross;
		kCross.x = y;
		kCross.y = -x;
		return kCross;
	}

	ICF bool similar(Self &p, T eu, T ev) const
	{ 
		return std::abs(x-p.x)<eu && std::abs(y-p.y)<ev;
	}

	ICF bool similar(const Self &p, float E=EPS_L) const
	{ 
		return std::abs(x-p.x)<E && std::abs(y-p.y)<E;
	};

	// average arithmetic
	ICF SelfRef averageA(Self &p1, Self &p2) 
	{
		x = (p1.x + p2.x)*.5f;
		y = (p1.y + p2.y)*.5f;
		return *this;
	}

	// average geometric
	ICF SelfRef averageG(Self &p1, Self &p2) 
	{
		x = _sqrt(p1.x*p2.x);
		y = _sqrt(p1.y*p2.y);
		return *this;
	}

	T& operator[] (int i) const
	{
		// assert:  0 <= i < 2; x and y are packed into 2*sizeof(float) bytes
		return (T&) *(&x + i);
	}

	ICF SelfRef 	normalize		(void)					{ return norm();	 	}
	ICF SelfRef 	normalize_safe	(void)					{ return norm_safe();	}
	ICF SelfRef 	normalize		(const Self &v)			{ float m=_sqrt(v.x*v.x+v.y*v.y); x=v.x/m; y=v.y/m;			return *this; }
	ICF SelfRef 	normalize_safe	(const Self &v)			{ float m=_sqrt(v.x*v.x+v.y*v.y); if(m) {x=v.x/m; y=v.y/m;}	return *this; }
	ICF float 		dotproduct		(const Self &p) const	{ return dot(p);		}
	ICF float 		crossproduct	(const Self &p) const	{ return y*p.x - x*p.y; }
	
	ICF float getH() const
	{
		if (fis_zero(y))
		{
			if (fis_zero(x))
			{
				return 0.f;
			}

			return x > 0.0f ? -PI_DIV_2 : PI_DIV_2;
		}
		
		if (y < 0.f)
		{
			return -(atanf(x / y) - PI);
		}

		return -atanf(x / y);
	}
	IC	SelfRef	invert()								{ x=-x; y=-y;												return *this; }
};

typedef _vector2<float>		Fvector2;
typedef _vector2<double>	Dvector2;
typedef _vector2<int>		Ivector2;

template <class T>
bool	_valid			(const _vector2<T>& v)	{ return _valid((T)v.x) && _valid((T)v.y);	}

#endif