#include "StdAfx.h"
#include "ISpatial.h"

extern Fvector	c_spatial_offset[8];
class _MM_ALIGN16 spatial_box_walker
{
public:
	u32				mask;
	Fvector			center;
	Fvector			size;
	Fbox			box;
	ISpatial_DB*	space;

	bool bFirst = false;
public:
	spatial_box_walker(ISpatial_DB*	_space, u32 _mask, const Fvector& _center, const Fvector& _size)
	{
		mask	= _mask;
		center	= _center;
		size	= _size;
		box.setb(center,size);
		space	= _space;
	}
	void walk(xr_vector<ISpatialShared>& R, ISpatial_NODE* N, Fvector& n_C, float n_R)
	{
		// box
		float	n_vR = 2 * n_R;
		Fbox	BB;		BB.set(n_C.x - n_vR, n_C.y - n_vR, n_C.z - n_vR, n_C.x + n_vR, n_C.y + n_vR, n_C.z + n_vR);
		if (!BB.intersect(box))
			return;

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (0 == (S->spatial.type & mask))	
				continue;

			Fvector& sC = S->spatial.sphere.P;
			float			sR = S->spatial.sphere.R;
			Fbox			sB;		sB.set(sC.x - sR, sC.y - sR, sC.z - sR, sC.x + sR, sC.y + sR, sC.z + sR);
			if (!sB.intersect(box))	continue;

			R.push_back(S);
			if (bFirst)			return;
		}

		// recurse
		float	c_R = n_R / 2;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (0 == N->children[octant])	continue;
			Fvector		c_C;			c_C.mad(n_C, c_spatial_offset[octant], c_R);
			walk(R, N->children[octant], c_C, c_R);
			if (bFirst && !R.empty())	return;
		}
	}
};

void ISpatial_DB::q_box(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask, const Fvector& _center, const Fvector& _size, const Fvector& near_sort_origin)
{
	PROF_EVENT("ISpatial_DB::q_frustum");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.resize(0);

	spatial_box_walker W(this, _mask, _center, _size);
	W.bFirst = !!(_o&O_ONLYFIRST);
	W.walk(R, m_root, m_center, m_bounds);

	if (&near_sort_origin != &zero_fvector3)//nearest sorting
	{
		std::sort(R.begin(), R.end(),
		[&near_sort_origin](ISpatialShared& _1, ISpatialShared& _2)
		{
			float d1 = _1.get() ? _1->spatial.sphere.P.distance_to_sqr(near_sort_origin) : EPS_L;
			float d2 = _1.get() ? _2->spatial.sphere.P.distance_to_sqr(near_sort_origin) : EPS;
			return d1 < d2;
		});
	}
}

void ISpatial_DB::q_sphere(xr_vector<ISpatialShared>& R, u32 _o, u32 _mask, const Fvector& _center, const float _radius, const Fvector& near_sort_origin)
{
	Fvector _size = { _radius,_radius,_radius };
	q_box(R, _o, _mask, _center, _size, near_sort_origin);
}