#include "stdafx.h"
#include "ISpatial.h"

extern Fvector	c_spatial_offset[8];
struct spatial_ray_walker final
{
	Fvector pos, fwd_dir;
	ESPATIAL_TYPE mask;
	ISpatial_DB* space;
	xr_vector<ISpatialShared>& R;
	float range, range2;

	bool bFirst = false;
	bool bNearest = false;

	void xr_vectorcall walk(ISpatial_NODE* N, Fvector& n_C, float n_R)
	{
		float n_vR = n_R * 2.f;
		Fbox BB;
		BB.set(Fvector().sub(n_C, n_vR), Fvector().add(n_C, n_vR));
		Fvector P;
		if (!BB.Pick2(pos, fwd_dir, P))
			return;

		if (P.distance_to_sqr(pos) > range2)
			return;

		for (ISpatialShared& S : N->items)
		{
			if (!S.get())
				continue;

			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))
				continue;

			Fsphere& sS = S->spatial.sphere;
			int quantity;
			float afT[2];
			Fsphere::ERP_Result	result = sS.intersect(pos, fwd_dir,range,quantity,afT);

			if (result==Fsphere::rpOriginInside || ((result==Fsphere::rpOriginOutside)&&(afT[0]<range)))
			{
				if (bNearest)
				{
					switch(result)
					{
					case Fsphere::rpOriginInside:
						range = afT[0]<range?afT[0]:range;
						break;

					case Fsphere::rpOriginOutside:
						range = afT[0];
						break;
					}
					range2 = _sqr(range);
				}
				R.push_back(S);

				if (bFirst)
					return;
			}
		}

		float c_R = n_R * 0.5f;
		for (u32 octant=0; octant<8; octant++)
		{
			if (0==N->children[octant])
				continue;

			Fvector c_C;
			c_C.mad(n_C,c_spatial_offset[octant],c_R);

			walk(N->children[octant],c_C,c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_ray(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask_and, const Fvector&	_start,  const Fvector&	_dir, float _range)
{
	PROF_EVENT("ISpatial_DB::q_ray")
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_ray_walker W
	{
		_start,
		_dir,
		_mask_and,
		this,
		R,
		_range,
		_range*_range,
		!!(_o&O_ONLYFIRST),
		!!(_o&O_ONLYNEAREST),
	};
	W.walk(m_root, m_center, m_bounds);
}
