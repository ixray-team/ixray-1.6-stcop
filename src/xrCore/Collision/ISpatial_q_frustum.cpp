#include "stdafx.h"
#include "ISpatial.h"
#include "Frustum.h"

extern Fvector	c_spatial_offset[8];
struct spatial_frustum_walker final
{
	const CFrustum&	F;
	ESPATIAL_TYPE	mask;
	ISpatial_DB*	space;
	xr_vector<ISpatialShared>& R;
	void xr_vectorcall walk(ISpatial_NODE* N, const Fvector& n_C, float n_R, u32 fmask)
	{
		// box
		float n_vR = n_R * 2.f;
		if (fcvNone == F.testAABB(Fbox(n_C-n_vR,n_C+n_vR).data(), fmask))
			return;

		// test items
		for (ISpatialShared& S : N->items)
		{
			if (!S.get()) continue;
			if (ESPATIAL_TYPE::NONE == (S->spatial.type & mask))
				continue;

			Fvector& sC = S->spatial.sphere.P;
			float sR = S->spatial.sphere.R;
			u32 tmask = fmask;

			if (fcvNone == F.testSphere(sC, sR, tmask))
				continue;

			R.push_back(S);
		}

		// recurse
		float c_R = n_R * 0.5f;
		for (u32 octant = 0; octant < 8; octant++)
		{
			if (nullptr == N->children[octant])
				continue;

			walk(N->children[octant], Fvector().mad(n_C, c_spatial_offset[octant], c_R), c_R, fmask);
		}
	}
};

void ISpatial_DB::q_frustum(xr_vector<ISpatialShared>& R, u32 _o, ESPATIAL_TYPE _mask, const CFrustum& _frustum)
{
	PROF_EVENT("ISpatial_DB::q_frustum")
	xrSRWLockGuard guard(&db_lock, true);
	if (!m_root)
		return;

	R.clear();
	R.reserve(16);

	spatial_frustum_walker W
	{
		_frustum,
		_mask,
		this,
		R
	};
	W.walk(m_root,m_center,m_bounds,_frustum.getMask());
}
