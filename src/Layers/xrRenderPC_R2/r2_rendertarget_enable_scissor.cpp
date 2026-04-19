#include "stdafx.h"
#include "../../xrCore/Collision/cl_intersect.h"
#include "../xrRender/du_cone.h"

void CRenderTarget::enable_dbt_bounds		(light* L)
{
	if (!RImplementation.o.nvdbt)					return;
	if (!ps_r2_ls_flags.test(R2FLAG_USE_NVDBT))		return;

	u32	mask		= 0xffffffff;
	EFC_Visible vis	= RImplementation.ViewBase.testSphere(L->SpatialComponent->spatial.sphere.P,L->SpatialComponent->spatial.sphere.R*1.01f,mask);
	if (vis!=fcvFully)								return;

	// xform BB
	Fbox	BB;
	Fvector	rr; rr.set(L->SpatialComponent->spatial.sphere.R,L->SpatialComponent->spatial.sphere.R,L->SpatialComponent->spatial.sphere.R);
	BB.setb	(L->SpatialComponent->spatial.sphere.P, rr);

	Fbox	bbp; bbp.invalidate();
	for (u32 i=0; i<8; i++)		{
		Fvector		pt;
		BB.getpoint	(i,pt);
		Device.mFullTransform.transform	(pt);
		bbp.modify	(pt);
	}
	u_DBT_enable	(bbp.min.z,bbp.max.z);
}

// nv-DBT
bool	CRenderTarget::u_DBT_enable	(float zMin, float zMax)
{
	if (!RImplementation.o.nvdbt)					return	false;
	if (!ps_r2_ls_flags.test(R2FLAG_USE_NVDBT))		return	false;

	// enable cheat
	GRHI->DriverExt->SetDepthBounds(true, zMin, zMax);

	return true;
}

void	CRenderTarget::u_DBT_disable	()
{
	if (RImplementation.o.nvdbt && ps_r2_ls_flags.test(R2FLAG_USE_NVDBT))
		GRHI->DriverExt->SetDepthBounds(false, 0, 0);
}

bool CRenderTarget::enable_scissor(light* L)		// true if intersects near plane
{
	// Near plane intersection
	bool near_intersect = false;
	{
		Fmatrix& M = Device.mFullTransform;
		Fvector4 plane;
		plane.x = -(M._14 + M._13);
		plane.y = -(M._24 + M._23);
		plane.z = -(M._34 + M._33);
		plane.w = -(M._44 + M._43);
		float denom = -1.0f / _sqrt(_sqr(plane.x) + _sqr(plane.y) + _sqr(plane.z));
		plane.mul(denom);
		Fplane	P;	P.n.set(plane.x, plane.y, plane.z); P.d = plane.w;
		float	p_dist = P.classify(L->SpatialComponent->spatial.sphere.P) - L->SpatialComponent->spatial.sphere.R;
		near_intersect = (p_dist <= 0);
	}

#ifdef DEBUG
	Fsphere S;
	S.set(L->SpatialComponent->spatial.sphere.P, L->SpatialComponent->spatial.sphere.R);
	dbg_spheres.push_back(std::make_pair(S, L->color));
#endif

	// Scissor
	//. disable scissor because some bugs prevent it to work through multi-portals
	//. if (!Caps.bScissor)	return		near_intersect;
	return		near_intersect;
}