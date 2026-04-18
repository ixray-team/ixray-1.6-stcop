#pragma once

template <class T>
class _box3
{
public:
	typedef T			TYPE;
	typedef _box3<T>	Self;
	typedef Self&		SelfRef;
	typedef const Self&	SelfCRef;
	typedef _vector3<T>	Tvector;
	typedef _matrix<T>	Tmatrix;
public:
	union
	{
		struct
		{
			Tvector	min;
			Tvector	max;
		};
		struct
		{
			T x1, y1, z1;
			T x2, y2, z2;
		};
	};

	ICF _box3() : x1(T(0)), y1(T(0)), z1(T(0)), x2(T(0)), y2(T(0)), z2(T(0)) {}
	ICF _box3(const Tvector& _min, const Tvector& _max) : min(_min), max(_max) { }
	ICF _box3(T _x1, T _y1, T _z1, T _x2, T _y2, T _z2) : x1(_x1), y1(_y1), z1(_z1), x2(_x2), y2(_y2), z2(_z2) {}
	template<typename U>
	ICF _box3(U) = delete;

	template<typename U, typename V>
	ICF _box3(U, V) = delete;

	template<typename U, typename V, typename W>
	ICF _box3(U, V, W) = delete;

	template<typename U, typename V, typename W, typename X>
	ICF _box3(U, V, W, X) = delete;

	template<typename U, typename V, typename W, typename X, typename Y>
	ICF _box3(U, V, W, X, Y) = delete;

	ICF	bool	is_valid	()											{return (x2>=x1)&&(y2>=y1)&&(z2>=z1);}

	ICF	const T* data		()	const									{ return &min.x;	}	

	ICF SelfRef	set			(const Tvector &_min, const Tvector &_max)	{ min.set(_min);	max.set(_max);		return *this;	};
	ICF	SelfRef	set			(T x1_, T y1_, T z1_, T x2_, T y2_, T z2_)		{ min.set(x1_,y1_,z1_);max.set(x2_,y2_,z2_);	return *this;	};
	ICF	SelfRef	set			(SelfCRef b)								{ min.set(b.min);	max.set(b.max);		return *this;	};
    ICF SelfRef	setb		(const Tvector& center, const Tvector& dim)	{ min.sub(center,dim);max.add(center,dim);return *this;	}

	ICF	SelfRef	null		()								{ min.set(0,0,0);	max.set(0,0,0);					return *this;	};
	ICF	SelfRef	identity	()								{ min.set(-0.5,-0.5,-0.5);	max.set(0.5,0.5,0.5);						return *this;	};
	ICF	SelfRef	invalidate	()								{ min.set(type_max(T),type_max(T),type_max(T)); max.set(type_min(T),type_min(T),type_min(T));	return *this;	}
	
	ICF	SelfRef	shrink		(T s)							{ min.add(s); max.sub(s);	return *this;	};
	ICF	SelfRef	shrink		(const Tvector& s)				{ min.add(s); max.sub(s);	return *this;	};
	ICF	SelfRef	grow		(T s)							{ min.sub(s); max.add(s);	return *this;	};
	ICF	SelfRef	grow		(const Tvector& s)				{ min.sub(s); max.add(s);	return *this;	};
	
	ICF	SelfRef	add			(const Tvector &p)				{ min.add(p); max.add(p);	return *this;	};
	ICF	SelfRef	sub			(const Tvector &p)				{ min.sub(p); max.sub(p);	return *this;	};
	ICF	SelfRef	offset		(const Tvector &p)				{ min.add(p); max.add(p);	return *this;	};
	ICF	SelfRef	add			(SelfCRef b, const Tvector &p)	{ min.add(b.min, p); max.add(b.max, p);				return *this;	};
	
	ICF	bool	contains	(T x, T y, T z)		const		{ return (x>=x1) && (x<=x2) && (y>=y1) && (y<=y2) && (z>=z1) && (z<=z2); };
	ICF	bool	contains	(const Tvector &p)	const		{ return contains(p.x,p.y,p.z);	};
	ICF	bool	contains	(SelfCRef b)		const		{ return contains(b.min) && contains(b.max); };
	
	ICF	bool	similar		(SelfCRef b)		const		{ return min.similar(b.min) && max.similar(b.max); };
	
	ICF	SelfRef	modify		(const Tvector &p)				{ min.min(p); max.max(p);				return *this;	}
	ICF	SelfRef	modify		(T x, T y, T z)					{ _vector3<T> tmp = {x,y,z}; return		modify(tmp);	}
	ICF	SelfRef	merge		(SelfCRef b)					{ modify(b.min); modify(b.max);			return *this;	};
	ICF	SelfRef	merge		(SelfCRef b1, SelfCRef b2)		{ invalidate(); merge(b1); merge(b2);	return *this;	}
	ICF	SelfRef	xform		(SelfCRef B, const Tmatrix &m)
	{
		// The three edges transformed: you can efficiently transform an X-only vector3
		// by just getting the "X" column of the matrix
		Tvector vx,vy,vz;
		vx.mul				(m.i, B.max.x-B.min.x);	
		vy.mul				(m.j, B.max.y-B.min.y);	
		vz.mul				(m.k, B.max.z-B.min.z);	
		
		// Transform the min point
		m.transform_tiny	(min,B.min);
		max.set				(min);
		
		// Take the transformed min & axes and find _new_ extents
		// Using CPU code in the right place is faster...
		if(negative(vx.x))	min.x += vx.x; else max.x += vx.x;
		if(negative(vx.y))	min.y += vx.y; else max.y += vx.y;
		if(negative(vx.z))	min.z += vx.z; else max.z += vx.z;
		if(negative(vy.x))	min.x += vy.x; else max.x += vy.x;
		if(negative(vy.y))	min.y += vy.y; else max.y += vy.y;
		if(negative(vy.z))	min.z += vy.z; else max.z += vy.z;
		if(negative(vz.x))	min.x += vz.x; else max.x += vz.x;
		if(negative(vz.y))	min.y += vz.y; else max.y += vz.y;
		if(negative(vz.z))	min.z += vz.z; else max.z += vz.z;
		return *this;
	}
	ICF	SelfRef	xform		(const Tmatrix &m)
    {
		Self b;
        b.set(*this);
        return xform(b,m);
    }

	ICF	void		getsize		(Tvector& R )	const 	{ R.sub( max, min ); };
	ICF	void		getradius	(Tvector& R )	const 	{ getsize(R); R.mul(0.5f); };
	ICF	T			getradius	()				const 	{ Tvector R; getradius(R); return R.magnitude();	};
	ICF	T			getvolume	()				const	{ Tvector sz; getsize(sz); return sz.x*sz.y*sz.z;	};
	ICF	SelfCRef	getcenter	(Tvector& C )	const 	{
		C.x = (min.x + max.x) * 0.5f;
		C.y = (min.y + max.y) * 0.5f;
		C.z = (min.z + max.z) * 0.5f;
		return				*this;
	};
	ICF	SelfCRef	get_CD		(Tvector& bc, Tvector& bd)	const // center + dimensions
	{
		bd.sub				(max,min).mul(.5f);
		bc.add				(min,bd);
		return				*this;
	}
	ICF	SelfRef		scale		(float s)					// 0.1 means make 110%, -0.1 means make 90%
	{
		Fvector	bd;	bd.sub	(max,min).mul(s);
		grow				(bd);
		return				*this;
	}
	ICF	SelfCRef	getsphere	(Tvector &C, T &R) const {
		getcenter			(C);
		R = C.distance_to	(max);
		return				*this;
	};
	
	// Detects if this box intersect other
	ICF	bool	intersect	(SelfCRef box )
	{
		if( max.x < box.min.x )	return FALSE;
		if( max.y < box.min.y )	return FALSE;
		if( max.z < box.min.z )	return FALSE;
		if( min.x > box.max.x )	return FALSE;
		if( min.y > box.max.y )	return FALSE;
		if( min.z > box.max.z )	return FALSE;
		return TRUE;
	};

	ICF bool intersectTri(const Tvector(&p)[3], const Tvector& center, const Tvector& extents, bool bClass3 = true)
	{
		Tvector tv0, tv1, tv2;
		tv0.sub(p[0], center);
		tv1.sub(p[1], center);
		tv2.sub(p[2], center);

		Tvector e0, e1, e2;
		e0.sub(tv1, tv0);
		e1.sub(tv2, tv1);
		e2.sub(tv0, tv2);

		T p0, p1, p2, min, max, rad;

		min = std::min(tv0.x, std::min(tv1.x, tv2.x));
		max = std::max(tv0.x, std::max(tv1.x, tv2.x));
		if (min > extents.x || max < -extents.x)
			return false;

		min = std::min(tv0.y, std::min(tv1.y, tv2.y));
		max = std::max(tv0.y, std::max(tv1.y, tv2.y));
		if (min > extents.y || max < -extents.y)
			return false;

		min = std::min(tv0.z, std::min(tv1.z, tv2.z));
		max = std::max(tv0.z, std::max(tv1.z, tv2.z));
		if (min > extents.z || max < -extents.z)
			return false;

		Tvector normal = e0 ^ e1;
		if (std::abs(normal.x) < EPS_S && std::abs(normal.y) < EPS_S && std::abs(normal.z) < EPS_S)
			return true;

		T v0_dist = normal.dotproduct(tv0);
		T v1_dist = normal.dotproduct(tv1);
		T v2_dist = normal.dotproduct(tv2);
		min = std::min(v0_dist, std::min(v1_dist, v2_dist));
		max = std::max(v0_dist, std::max(v1_dist, v2_dist));

		rad = extents.x * std::abs(normal.x) + extents.y * std::abs(normal.y) + extents.z * std::abs(normal.z);

		if (min > rad || max < -rad)
			return false;

		if (bClass3)
		{
			T abs_e0x = std::abs(e0.x);
			T abs_e0y = std::abs(e0.y);
			T abs_e0z = std::abs(e0.z);
			T abs_e1x = std::abs(e1.x);
			T abs_e1y = std::abs(e1.y);
			T abs_e1z = std::abs(e1.z);
			T abs_e2x = std::abs(e2.x);
			T abs_e2y = std::abs(e2.y);
			T abs_e2z = std::abs(e2.z);

			p0 = tv0.y * e0.z - tv0.z * e0.y;
			p1 = tv1.y * e0.z - tv1.z * e0.y;
			p2 = tv2.y * e0.z - tv2.z * e0.y;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.y * abs_e0z + extents.z * abs_e0y;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.z * e0.x - tv0.x * e0.z;
			p1 = tv1.z * e0.x - tv1.x * e0.z;
			p2 = tv2.z * e0.x - tv2.x * e0.z;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e0z + extents.z * abs_e0x;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.x * e0.y - tv0.y * e0.x;
			p1 = tv1.x * e0.y - tv1.y * e0.x;
			p2 = tv2.x * e0.y - tv2.y * e0.x;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e0y + extents.y * abs_e0x;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.y * e1.z - tv0.z * e1.y;
			p1 = tv1.y * e1.z - tv1.z * e1.y;
			p2 = tv2.y * e1.z - tv2.z * e1.y;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.y * abs_e1z + extents.z * abs_e1y;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.z * e1.x - tv0.x * e1.z;
			p1 = tv1.z * e1.x - tv1.x * e1.z;
			p2 = tv2.z * e1.x - tv2.x * e1.z;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e1z + extents.z * abs_e1x;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.x * e1.y - tv0.y * e1.x;
			p1 = tv1.x * e1.y - tv1.y * e1.x;
			p2 = tv2.x * e1.y - tv2.y * e1.x;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e1y + extents.y * abs_e1x;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.y * e2.z - tv0.z * e2.y;
			p1 = tv1.y * e2.z - tv1.z * e2.y;
			p2 = tv2.y * e2.z - tv2.z * e2.y;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.y * abs_e2z + extents.z * abs_e2y;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.z * e2.x - tv0.x * e2.z;
			p1 = tv1.z * e2.x - tv1.x * e2.z;
			p2 = tv2.z * e2.x - tv2.x * e2.z;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e2z + extents.z * abs_e2x;
			if (min > rad || max < -rad)
				return false;

			p0 = tv0.x * e2.y - tv0.y * e2.x;
			p1 = tv1.x * e2.y - tv1.y * e2.x;
			p2 = tv2.x * e2.y - tv2.y * e2.x;
			min = std::min(p0, std::min(p1, p2));
			max = std::max(p0, std::max(p1, p2));
			rad = extents.x * abs_e2y + extents.y * abs_e2x;
			if (min > rad || max < -rad)
				return false;
		}

		return true;
	}

	ICF bool intersectTri(const Tvector (&p)[3], bool bClass3 = true)
	{
		Tvector center, extents;
		get_CD(center, extents);

		return intersectTri(p, center, extents, bClass3);
	}

	// Does the vector3 intersects box
	ICF bool Pick			(const Tvector& start, const Tvector& dir)
	{
		T	alpha,xt,yt,zt;
		Tvector rvmin,rvmax;

		rvmin.sub( min, start );
		rvmax.sub( max, start );

		if( !fis_zero(dir.x) ){
			alpha = rvmin.x / dir.x;
			yt = alpha * dir.y;
			if( yt >= rvmin.y && yt <= rvmax.y ){
				zt = alpha * dir.z;
				if( zt >= rvmin.z && zt <= rvmax.z )
					return true;
			}
			alpha = rvmax.x / dir.x;
			yt = alpha * dir.y;
			if( yt >= rvmin.y && yt <= rvmax.y ){
				zt = alpha * dir.z;
				if( zt >= rvmin.z && zt <= rvmax.z )
					return true;
			}
		}

		if( !fis_zero(dir.y) ){
			alpha = rvmin.y / dir.y;
			xt = alpha * dir.x;
			if( xt >= rvmin.x && xt <= rvmax.x ){
				zt = alpha * dir.z;
				if( zt >= rvmin.z && zt <= rvmax.z )
					return true;
			}
			alpha = rvmax.y / dir.y;
			xt = alpha * dir.x;
			if( xt >= rvmin.x && xt <= rvmax.x ){
				zt = alpha * dir.z;
				if( zt >= rvmin.z && zt <= rvmax.z )
					return true;
			}
		}

		if( !fis_zero(dir.z) ){
			alpha = rvmin.z / dir.z;
			xt = alpha * dir.x;
			if( xt >= rvmin.x && xt <= rvmax.x ){
				yt = alpha * dir.y;
				if( yt >= rvmin.y && yt <= rvmax.y )
					return true;
			}
			alpha = rvmax.z / dir.z;
			xt = alpha * dir.x;
			if( xt >= rvmin.x && xt <= rvmax.x ){
				yt = alpha * dir.y;
				if( yt >= rvmin.y && yt <= rvmax.y )
					return true;
			}
		}
		return false;
	};

	ICF u32& IR(T &x) { return (u32&)x; }
	enum ERP_Result{
		rpNone			= 0,
		rpOriginInside	= 1,
		rpOriginOutside	= 2,
		fcv_forcedword = u32(-1)
	};
	ICF ERP_Result Pick2(const Tvector& origin, const Tvector& dir, Tvector& coord)
	{
		bool Inside = TRUE;
		Tvector		MaxT;
		MaxT.x=MaxT.y=MaxT.z=-1.0f;
		
		// Find candidate planes.
		{
			if(origin[0] < min[0]) {
				coord[0]	= min[0];
				Inside		= FALSE;
				if(IR(dir[0]))	MaxT[0] = (min[0] - origin[0]) / dir[0]; // Calculate T distances to candidate planes
			} else if(origin[0] > max[0]) {
				coord[0]	= max[0];
				Inside		= FALSE;
				if(IR(dir[0]))	MaxT[0] = (max[0] - origin[0]) / dir[0]; // Calculate T distances to candidate planes
			}
		}
		{
			if(origin[1] < min[1]) {
				coord[1]	= min[1];
				Inside		= FALSE;
				if(IR(dir[1]))	MaxT[1] = (min[1] - origin[1]) / dir[1]; // Calculate T distances to candidate planes
			} else if(origin[1] > max[1]) {
				coord[1]	= max[1];
				Inside		= FALSE;
				if(IR(dir[1]))	MaxT[1] = (max[1] - origin[1]) / dir[1]; // Calculate T distances to candidate planes
			}
		}
		{
			if(origin[2] < min[2]) {
				coord[2]	= min[2];
				Inside		= FALSE;
				if(IR(dir[2]))	MaxT[2] = (min[2] - origin[2]) / dir[2]; // Calculate T distances to candidate planes
			} else if(origin[2] > max[2]) {
				coord[2]	= max[2];
				Inside		= FALSE;
				if(IR(dir[2]))	MaxT[2] = (max[2] - origin[2]) / dir[2]; // Calculate T distances to candidate planes
			}
		}
		
		// Ray origin inside bounding box
		if(Inside)
		{
			coord	= origin;
			return	rpOriginInside;
		}
		
		// Get largest of the maxT's for final choice of intersection
		u32 WhichPlane = 0;
		if(MaxT[1] > MaxT[0])			WhichPlane = 1;
		if(MaxT[2] > MaxT[WhichPlane])	WhichPlane = 2;
		
		// Check final candidate actually inside box
		if(IR(MaxT[WhichPlane])&0x80000000) return rpNone;
		
		if (0==WhichPlane)
		{
			// 1 & 2
			coord[1] = origin[1] + MaxT[0] * dir[1];
			if((coord[1] < min[1]) || (coord[1] > max[1]))	return rpNone;
			coord[2] = origin[2] + MaxT[0] * dir[2];
			if((coord[2] < min[2]) || (coord[2] > max[2]))	return rpNone;
			return rpOriginOutside;
		}
		if (1==WhichPlane)
		{
			// 0 & 2
			coord[0] = origin[0] + MaxT[1] * dir[0];
			if((coord[0] < min[0]) || (coord[0] > max[0]))	return rpNone;
			coord[2] = origin[2] + MaxT[1] * dir[2];
			if((coord[2] < min[2]) || (coord[2] > max[2]))	return rpNone;
			return rpOriginOutside;
		}
		if (2==WhichPlane)
		{
			// 0 & 1
			coord[0] = origin[0] + MaxT[2] * dir[0];
			if((coord[0] < min[0]) || (coord[0] > max[0]))	return rpNone;
			coord[1] = origin[1] + MaxT[2] * dir[1];
			if((coord[1] < min[1]) || (coord[1] > max[1]))	return rpNone;
			return rpOriginOutside;
		}
		return rpNone;
	}
	
	ICF void getpoint( int index,  Tvector& result ) const 
	{
		switch( index ){
		case 0: result.set( min.x, min.y, min.z ); break;
		case 1: result.set( min.x, min.y, max.z ); break;
		case 2: result.set( max.x, min.y, max.z ); break;
		case 3: result.set( max.x, min.y, min.z ); break;
		case 4: result.set( min.x, max.y, min.z ); break;
		case 5: result.set( min.x, max.y, max.z ); break;
		case 6: result.set( max.x, max.y, max.z ); break;
		case 7: result.set( max.x, max.y, min.z ); break;
		default: result.set( 0, 0, 0 ); break; }
	};
	ICF void getpoints(Tvector* result)
	{
		result[0].set( min.x, min.y, min.z );
		result[1].set( min.x, min.y, max.z );
		result[2].set( max.x, min.y, max.z );
		result[3].set( max.x, min.y, min.z );
		result[4].set( min.x, max.y, min.z );
		result[5].set( min.x, max.y, max.z );
		result[6].set( max.x, max.y, max.z );
		result[7].set( max.x, max.y, min.z );
	};

	ICF SelfRef modify(SelfCRef src, const Tmatrix& M)
	{
		Tvector pt;
		for(int i=0; i<8; i++){
			src.getpoint(i,pt);
			M.transform_tiny(pt);
			modify(pt);
		}
		return *this;
	}
};

typedef _box3<float>	Fbox;
typedef _box3<float>	Fbox3;
typedef _box3<double>	Dbox;
typedef _box3<double>	Dbox3;

template <class T>
ICF bool	_valid			(const _box3<T>& c)	{ return _valid(c.min) && _valid(c.max); }