#ifndef _F_SPHERE_H_
#define _F_SPHERE_H_

template <class T>
struct _sphere {
	_vector3<T>	P;
	T			R = 0;
public:
	ICF void		set(const _vector3<T> &_P, T _R)	{ P.set(_P); R = _R; }
	ICF void		set(const _sphere<T> &S)			{ P.set(S.P); R=S.R; }
	ICF void		identity()							{ P.set(0,0,0); R=1; }

	enum ERP_Result{
		rpNone			= 0,
		rpOriginInside	= 1,
		rpOriginOutside	= 2,
		fcv_forcedword = u32(-1)
	};


	ICF bool operator==(const _sphere<T>& Left)
	{
		return P == Left.P && R == Left.R;
	}

	// Ray-sphere intersection
	ICF ERP_Result intersect (const _vector3<T>& S, const _vector3<T>& D, T range, int& quantity, T afT[2]) const
	{
		// set up quadratic Q(t) = a*t^2 + 2*b*t + c
		_vector3<T> kDiff;  kDiff.sub	(S,P);
		T fA				= range*range;
		T fB				= kDiff.dotproduct(D)		* range;
		T fC				= kDiff.square_magnitude()	- R*R;
		ERP_Result result	= rpNone;

		T fDiscr			= fB*fB - fA*fC;
		if ( fDiscr < (T)0.0 ){
			quantity		= 0;
		} else if ( fDiscr > (T)0.0 ){
			T fRoot 		= _sqrt(fDiscr);
			T fInvA 		= ((T)1.0)/fA;
			afT[0]			= range*(-fB - fRoot)*fInvA;
			afT[1]			= range*(-fB + fRoot)*fInvA;
			if ( afT[0] >= (T)0.0 )		{	quantity	= 2;					result = rpOriginOutside;	}
			else if ( afT[1] >= (T)0.0 ){	quantity	= 1; afT[0] = afT[1];	result = rpOriginInside;	}
			else							quantity	= 0;
		} else {
			afT[0]			= range*(-fB/fA);
			if ( afT[0] >= (T)0.0 )		{	quantity	= 1;					result = rpOriginOutside;	}
			else							quantity	= 0;
		}
		return result;
	}
/*
			int				quantity;
			float			afT[2];
			Fsphere::ERP_Result	result	= sS.intersect(ray.pos,ray.fwd_dir,range,quantity,afT);

			if (Fsphere::rpOriginInside || ((result==Fsphere::rpOriginOutside)&&(afT[0]<range))){
				if (b_nearest)				{ 
					switch(result){
					case Fsphere::rpOriginInside:	range	= afT[0]<range?afT[0]:range;	break;
					case Fsphere::rpOriginOutside:	range	= afT[0];						break;
					}
					range2			=range*range; 
				}
*/
	ICF ERP_Result intersect_full	(const _vector3<T>& start, const _vector3<T>& dir, T& dist) const
	{
		int				quantity;
		float			afT[2];
		auto result = intersect(start,dir,dist,quantity,afT);

		if (result == _sphere<T>::rpOriginInside || ((result== _sphere<T>::rpOriginOutside)&&(afT[0]<dist))){
			switch(result){
				case _sphere<T>::rpOriginInside:	dist	= afT[0]<dist?afT[0]:dist;		break;
				case _sphere<T>::rpOriginOutside:	dist	= afT[0];						break;
			}
		}
		return			result;
	}

	ICF ERP_Result intersect	(const _vector3<T>& start, const _vector3<T>& dir, T& dist) const
	{
		int				quantity;
		T				afT[2];
		ERP_Result		result	= intersect(start,dir,dist,quantity,afT);
		if (rpNone!=result){
			VERIFY		(quantity>0);
			if (afT[0]<dist){
				dist	= afT[0];
				return	result;
			}
		}
		return			rpNone;
	}

	ICF ERP_Result intersect2(const _vector3<T>& S, const _vector3<T>& D, T& range) const
    {
		_vector3<T> Q;	Q.sub(P,S);
	
		T R2	= R*R;
		T c2	= Q.square_magnitude	();
		T v		= Q.dotproduct			(D);
		T d		= R2 - (c2 - v*v);

		if		(d > 0.f)
		{
			T _range	= v - _sqrt(d);
			if (_range<range)	{
				range = _range;
				return (c2<R2)?rpOriginInside:rpOriginOutside;
			}
		}
		return rpNone;
	}
	ICF bool		intersect(const _vector3<T>& S, const _vector3<T>& D) const	
	{
		_vector3<T> Q;	Q.sub(P,S);
	
		T c = Q.magnitude	();
		T v = Q.dotproduct	(D);
		T d = R*R - (c*c - v*v);
		return (d > 0);
	}
	ICF bool		intersect(const _sphere<T>& S) const
	{	
		T SumR = R+S.R;
		return P.distance_to_sqr(S.P) < SumR*SumR;
	}
	ICF bool		contains(const _vector3<T>& PT) const
	{
		return P.distance_to_sqr(PT) <= (R*R+EPS_S);
	}
	
	// returns true if this wholly contains the argument sphere
	ICF bool		contains(const _sphere<T>& S) const
	{
		// can't contain a sphere that's bigger than me !
		const T RDiff		= R - S.R;
		if ( RDiff < 0 )	return false;

		return ( P.distance_to_sqr(S.P) <= RDiff*RDiff );
	}

	// return's volume of sphere
	ICF T		volume	() const
	{
		return T( PI_MUL_4 / 3 ) * (R*R*R);
	}

	ICF bool intersectAABB(const _vector3<T>& aabb_center, const _vector3<T>& aabb_extents)
	{
		_vector3<T> diff;
		diff.sub(P, aabb_center);

		for (int i = 0; i < 3; i++)
		{
			if (std::abs(diff[i]) > aabb_extents[i] + R)
				return false;
		}

		_vector3<T> closest;
		closest.x = std::max(-aabb_extents.x, std::min(diff.x, aabb_extents.x));
		closest.y = std::max(-aabb_extents.y, std::min(diff.y, aabb_extents.y));
		closest.z = std::max(-aabb_extents.z, std::min(diff.z, aabb_extents.z));

		T distance_sqr = diff.distance_to_sqr(closest);
		return distance_sqr <= (R * R);
	}

	ICF bool intersectTri(const _vector3<T> (&p)[3], bool bClass3 = true)
	{
		if(bClass3)
		{
			_vector3<T> min_tri, max_tri;
			min_tri = max_tri = p[0];

			for (int i = 1; i < 3; i++)
			{
				min_tri.min(p[i]);
				max_tri.max(p[i]);
			}

			_vector3<T> sphere_min, sphere_max;
			sphere_min.set(P.x - R, P.y - R, P.z - R);
			sphere_max.set(P.x + R, P.y + R, P.z + R);

			if (sphere_max.x < min_tri.x || sphere_min.x > max_tri.x ||
				sphere_max.y < min_tri.y || sphere_min.y > max_tri.y ||
				sphere_max.z < min_tri.z || sphere_min.z > max_tri.z)
				return false;
		}

		_plane<T> tri_plane;
		tri_plane.build(p[0], p[1], p[2]);

		T dist_to_plane = std::abs(tri_plane.classify(P));
		if (dist_to_plane > R)
			return false;

		_vector3<T> projected;
		tri_plane.project(projected, P);

		return pointInTriangle(projected, p) || (bClass3 && checkSphereTriangleEdges(p));
	}

	IC void FindContactsClipping(const _vector3<T>* tri, void (*callback)(const _vector3<T>& contact, void* user_data), void* user_data = nullptr)
	{
		for (u8 i = 0; i < 3; i++)
		{
			T dist_sqr = P.distance_to_sqr(tri[i]);
			if (dist_sqr <= R * R)
				callback(tri[i], user_data);
		}

		for (u8 i = 0; i < 3; i++)
		{
			u8 j = (i + 1) % 3;

			_vector3<T> edge, to_sphere;
			edge.sub(tri[j], tri[i]);
			to_sphere.sub(P, tri[i]);

			T edge_len_sqr = edge.square_magnitude();
			if (edge_len_sqr < EPS_S) continue;

			T t = to_sphere.dotproduct(edge) / edge_len_sqr;
			t = std::max(0.0f, std::min(1.0f, t));

			_vector3<T> closest_on_edge;
			closest_on_edge.mad(tri[i], edge, t);

			T dist_sqr = P.distance_to_sqr(closest_on_edge);
			if (dist_sqr <= R * R)
				callback(closest_on_edge, user_data);
		}

		_plane<T> tri_plane;
		tri_plane.build(tri[0], tri[1], tri[2]);

		_vector3<T> projected;
		tri_plane.project(projected, P);

		if (pointInTriangle(projected, tri))
		{
			T dist_to_plane = std::abs(tri_plane.classify(P));
			if (dist_to_plane <= R)
				callback(projected, user_data);
		}
	}

	ICF bool pointInTriangle(const _vector3<T>& point, const _vector3<T>* tri)
	{
		_vector3<T> v0, v1, v2;
		v0.sub(tri[2], tri[0]);
		v1.sub(tri[1], tri[0]);
		v2.sub(point, tri[0]);

		T dot00 = v0.dotproduct(v0);
		T dot01 = v0.dotproduct(v1);
		T dot02 = v0.dotproduct(v2);
		T dot11 = v1.dotproduct(v1);
		T dot12 = v1.dotproduct(v2);

		T invDenom = 1.0f / (dot00 * dot11 - dot01 * dot01);
		T u = (dot11 * dot02 - dot01 * dot12) * invDenom;
		T v = (dot00 * dot12 - dot01 * dot02) * invDenom;

		return (u >= -EPS_S) && (v >= -EPS_S) && (u + v <= 1.0f + EPS_S);
	}

	ICF bool checkSphereTriangleEdges(const _vector3<T>* tri)
	{
		for (u8 i = 0; i < 3; i++)
		{
			u8 j = (i + 1) % 3;

			_vector3<T> edge, to_sphere;
			edge.sub(tri[j], tri[i]);
			to_sphere.sub(P, tri[i]);

			T edge_len_sqr = edge.square_magnitude();
			if (edge_len_sqr < EPS_S) continue;

			T t = to_sphere.dotproduct(edge) / edge_len_sqr;
			t = std::max(0.0f, std::min(1.0f, t));

			_vector3<T> closest_point;
			closest_point.mad(tri[i], edge, t);

			T dist_sqr = P.distance_to_sqr(closest_point);
			if (dist_sqr <= R * R)
				return true;
		}

		for (u8 i = 0; i < 3; i++)
		{
			T dist_sqr = P.distance_to_sqr(tri[i]);
			if (dist_sqr <= R * R)
				return true;
		}

		return false;
	}
};

typedef _sphere<float>	Fsphere;
typedef _sphere<double> Dsphere;

template <class T>
ICF bool	_valid			(const _sphere<T>& s)		{ return _valid(s.P) && _valid(s.R);	}

void	XRCORE_API		Fsphere_compute		(Fsphere& dest, const Fvector *verts, int count);

#endif