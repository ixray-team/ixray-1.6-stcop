#include "stdafx.h"
#include "ISpatial.h"
#include "sse_intersect.h"

extern Fvector	c_spatial_offset[8];
class _MM_ALIGN16 spatial_ray_walker
{
public:
	ray_t			ray;
	u32				mask;
	float			range;
	float			range2;
	ISpatial_DB*	space;

	bool bSSE = false;
	bool bFirst = false;
	bool bNearest = false;
public:
	spatial_ray_walker(bool SSE, bool First, bool Nearest)
		:bSSE(SSE), bFirst(First), bNearest(Nearest) {}

	void _init(ISpatial_DB* _space, u32 _mask, const Fvector& _start, const Fvector& _dir, float _range)
	{
		mask			= _mask;
		ray.pos.set		(_start);
		ray.inv_dir.set	(1.f,1.f,1.f).div(_dir);
		ray.fwd_dir.set (_dir);
		if (!bSSE)	{
			// for FPU - zero out inf
			if (_abs(_dir.x)>flt_eps){}	else ray.inv_dir.x=0;
			if (_abs(_dir.y)>flt_eps){}	else ray.inv_dir.y=0;
			if (_abs(_dir.z)>flt_eps){}	else ray.inv_dir.z=0;
		}
		range	= _range;
		range2	= _range*_range;
		space	= _space;
	}

	void walk(xr_vector<ISpatialShared>& R, ISpatial_NODE* N, Fvector& n_C, float n_R)
	{
		// Actual ray/aabb test
		if (bSSE)		{
			// use SSE
			float		d;
			if (!_box_sse(ray,n_C,n_R,d))				return;
			if (d>range)							return;
		} else {
			// use FPU
			Fvector		P;
			if (!_box_fpu(ray,n_C,n_R,P))				return;
			if (P.distance_to_sqr(ray.pos)>range2)	return;
		}

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (mask!=(S->spatial.type&mask))	continue;
			Fsphere&		sS	= S->spatial.sphere;
			int				quantity;
			float			afT[2];
			Fsphere::ERP_Result	result	= sS.intersect(ray.pos,ray.fwd_dir,range,quantity,afT);

			if (result==Fsphere::rpOriginInside || ((result==Fsphere::rpOriginOutside)&&(afT[0]<range)))
			{
				if (bNearest)
				{
					switch(result)
					{
					case Fsphere::rpOriginInside:	range	= afT[0]<range?afT[0]:range;	break;
					case Fsphere::rpOriginOutside:	range	= afT[0];						break;
					}
					range2			=range*range; 
				}
				R.push_back	(S);
				if (bFirst)				return;
			}
		}

		// recurse
		float	c_R		= n_R/2;
		for (u32 octant=0; octant<8; octant++)
		{
			if (0==N->children[octant])	continue;
			Fvector		c_C;			c_C.mad	(n_C,c_spatial_offset[octant],c_R);
			walk						(R,N->children[octant],c_C,c_R);
			if (bFirst && !R.empty())	return;
		}
	}
};

void ISpatial_DB::q_ray(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask_and, const Fvector&	_start,  const Fvector&	_dir, float _range)
{
	PROF_EVENT("ISpatial_DB::q_ray")
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.resize(0);

	spatial_ray_walker W(CPU::ID.hasFeature(CPUFeature::SSE), !!(_o&O_ONLYFIRST), !!(_o&O_ONLYNEAREST));
	W._init(this, _mask_and, _start, _dir, _range);
	W.walk(R, m_root, m_center, m_bounds);
}
