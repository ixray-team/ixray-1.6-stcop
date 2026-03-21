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

	void walk(ISpatial_NODE* N, const Fvector& n_C, float n_R)
	{
		// box
		float n_vR = n_R * 2.f;
		if (!box.intersect(Fbox{n_C-n_vR,n_C+n_vR}))
			return;

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))	
				continue;

			Fvector& sC = S->spatial.sphere.P;
			float sR = S->spatial.sphere.R;
			if (!box.intersect(Fbox{sC-sR,sC+sR})) continue;

			R.push_back(S);
			if (bFirst) return;
		}

		// recurse
		float c_R = n_R * 0.5f;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (nullptr == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_box(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const Fbox& box)
{
	PROF_EVENT("ISpatial_DB::q_box");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_box_walker W
	{
		box,
		_mask,
		this,
		R,
		!!(_o & O_ONLYFIRST),
	};
	W.walk(m_root, m_center, m_bounds);
}

struct spatial_obb_walker final
{
	Fobb			obb;
	ESPATIAL_TYPE	mask;
	ISpatial_DB* space;
	xr_vector<ISpatialShared>& R;
	bool bFirst = false;

	void walk(ISpatial_NODE* N, const Fvector& n_C, float n_R)
	{
		// box
		float n_vR = n_R * 2.f;
		if (!obb.intersectAABB(Fbox{n_C-n_vR,n_C+n_vR}))
			return;

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))
				continue;

			Fvector& sC = S->spatial.sphere.P;
			float sR = S->spatial.sphere.R;
			if (!obb.intersectAABB(Fbox{sC-sR,sC+sR})) continue;

			R.push_back(S);
			if (bFirst) return;
		}

		// recurse
		float c_R = n_R * 0.5f;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (nullptr == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_obb(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const Fobb& obb)
{
	PROF_EVENT("ISpatial_DB::q_box");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_obb_walker W
	{
		obb,
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

	void walk(ISpatial_NODE* N, const Fvector& n_C, float n_R)
	{
		if (!sphere.intersect(Fsphere{n_C, n_R*2.f}))
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
			if (nullptr == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R);

			if (bFirst && !R.empty())
				return;
		}
	}
};

void ISpatial_DB::q_sphere(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const Fsphere& sphere)
{
	PROF_EVENT("ISpatial_DB::q_sphere");
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_sphere_walker W
	{
		sphere,
		_mask,
		this,
		R,
		!!(_o & O_ONLYFIRST),
	};
	W.walk(m_root, m_center, m_bounds);
}