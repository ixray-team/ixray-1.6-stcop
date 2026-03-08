#include "stdafx.h"
#include "ISpatial.h"

extern Fvector	c_spatial_offset[8];
struct spatial_box_walker final
{
	Fbox			box;
	ESPATIAL_TYPE	mask;
	ISpatial_DB*	space;
	xr_vector<ISpatialShared>& R;
	bool bFirst = false;

	void xr_vectorcall walk(ISpatial_NODE* N, Fvector& n_C, float n_R)
	{
		// box
		float n_vR = n_R * 2.f;
		Fbox BB; BB.set(Fvector().sub(n_C, n_vR), Fvector().add(n_C, n_vR));
		if (!BB.intersect(box))
			return;

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))	
				continue;

			Fvector& sC = S->spatial.sphere.P;
			float sR = S->spatial.sphere.R;
			Fbox sB; sB.set(Fvector().sub(sC, sR), Fvector().add(sC, sR));
			if (!sB.intersect(box))	continue;

			R.push_back(S);
			if (bFirst)			return;
		}

		// recurse
		float c_R = n_R * 0.5f;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (0 == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_box(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const Fvector& _center, const Fvector& _size)
{
	PROF_EVENT("ISpatial_DB::q_box");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_box_walker W
	{
		Fbox().setb(_center,_size),
		_mask,
		this,
		R,
		!!(_o & O_ONLYFIRST),
	};
	W.walk(m_root, m_center, m_bounds);
}

struct spatial_sphere_walker final
{
	Fsphere sphere;        // сфера запроса
	ESPATIAL_TYPE mask;
	ISpatial_DB* space;
	xr_vector<ISpatialShared>& R;
	bool bFirst = false;

	void xr_vectorcall walk(ISpatial_NODE* N, Fvector& n_C, float n_R)
	{
		if (!Fsphere(n_C, n_R * 2.f).intersect(sphere))
			return;

		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;

			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))
				continue;

			if (sphere.intersect(S->spatial.sphere))
			{
				R.push_back(S);
				if (bFirst) return;
			}
		}

		float c_R = n_R * 0.5f;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (0 == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_sphere(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const Fvector& _center, const float _radius)
{
	PROF_EVENT("ISpatial_DB::q_sphere");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_sphere_walker W
	{
		Fsphere{_center, _radius},
		_mask,
		this,
		R,
		!!(_o & O_ONLYFIRST),
	};
	W.walk(m_root, m_center, m_bounds);
}