#include "stdafx.h"
#include "xrCDB.h"
#include "override/Model.h"

using namespace CDB;
using namespace Opcode;

struct cform_ray_collider final
{
	Fvector pos, fwd_dir;
  	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	float rRange, rRange2;

	bool bCull = false;
	bool bFirst = false;
	bool bNearest = false;

	ICF bool _tri(Fvector* tri, float& u, float& v, float& range)
	{
		Fvector edge1, edge2, tvec, pvec, qvec;
		float det,inv_det;
		
		// find vectors for two edges sharing vert0
		const Fvector& p0 = tri[0];
		const Fvector& p1 = tri[1];
		const Fvector& p2 = tri[2];
		edge1.sub(p1, p0);
		edge2.sub(p2, p0);
		// begin calculating determinant - also used to calculate U parameter
		// if determinant is near zero, ray lies in plane of triangle
		pvec.crossproduct(fwd_dir, edge2);
		det = edge1.dotproduct(pvec);

		if (bCull)
		{						
			if (det < EPS)  return false;
			tvec.sub(pos, p0);						// calculate distance from vert0 to ray origin
			u = tvec.dotproduct(pvec);					// calculate U parameter and test bounds
			if (u < 0.f || u > det) return false;
			qvec.crossproduct(tvec, edge1);				// prepare to test V parameter
			v = fwd_dir.dotproduct(qvec);			// calculate V parameter and test bounds
			if (v < 0.f || u + v > det) return false;
			range = edge2.dotproduct(qvec);				// calculate t, scale parameters, ray intersects triangle
			inv_det = 1.0f / det;
			range	*= inv_det;
			u		*= inv_det;
			v		*= inv_det;
		}
		else
		{			
			if (det > -EPS && det < EPS) return false;
			inv_det = 1.0f / det;
			tvec.sub(pos, p0);						// calculate distance from vert0 to ray origin
			u = tvec.dotproduct(pvec)*inv_det;			// calculate U parameter and test bounds
			if (u < 0.0f || u > 1.0f)    return false;
			qvec.crossproduct(tvec, edge1);				// prepare to test V parameter
			v = fwd_dir.dotproduct(qvec)*inv_det;	// calculate V parameter and test bounds
			if (v < 0.0f || u + v > 1.0f) return false;
			range = edge2.dotproduct(qvec)*inv_det;		// calculate t, ray intersects triangle
		}
		return true;
	}
	
	ICF void _prim(DWORD prim)
	{
		float u,v,r;
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (!_tri(tri_verts, u, v, r))
			return;

		if (r<=0 || r>rRange)
			return;

		u32 dummy = Tri.dummy;
		if (bNearest)	
		{
			if (dest->r_count())	
			{
				RESULT& R = *dest->r_begin();
				if (r<R.range)
				{
					R.id		= prim;
					R.range		= r;
					R.u			= u;
					R.v			= v;
					R.verts	[0]	= tri_verts[0];
					R.verts	[1]	= tri_verts[1];
					R.verts	[2]	= tri_verts[2];
					R.dummy		= dummy;
					rRange		= r;
					rRange2		= r*r;
				}
			}
			else
			{
				RESULT& R	= dest->r_add();
				R.id		= prim;
				R.range		= r;
				R.u			= u;
				R.v			= v;
				R.verts	[0]	= tri_verts[0];
				R.verts	[1]	= tri_verts[1];
				R.verts	[2]	= tri_verts[2];
				R.dummy		= dummy;
				rRange		= r;
				rRange2		= r*r;
			}
		}
		else
 		{
			RESULT& R	= dest->r_add();				// По порядку создает RESULT
			R.id		= prim;
			R.range		= r;
			R.u			= u;
			R.v			= v;
			R.verts	[0]	= tri_verts[0];
			R.verts	[1]	= tri_verts[1];
			R.verts	[2]	= tri_verts[2];
			R.dummy		= dummy;
		}
	}

	void _stab(const AABBNoLeafNode* node)
	{
		Fvector& center = (Fvector&)node->mAABB.mCenter;
		Fvector& extents = (Fvector&)node->mAABB.mExtents;
		Fbox BB; BB.set(center-extents, center+extents);
		Fvector P;
		if (!BB.Pick2(pos, fwd_dir, P))
			return;
		
		if (P.distance_to_sqr(pos) > rRange2)
			return;

		// 1st chield
		if (node->HasPosLeaf())	_prim(node->GetPosPrimitive());
		else					_stab(node->GetPos());

		// Early exit for "only first"
		if (bFirst)
		{
			if (dest->r_count())
				return;
		}

		// 2nd chield
		if (node->HasNegLeaf())	_prim(node->GetNegPrimitive());
		else					_stab(node->GetNeg());
	}
};

void COLLIDER::ray_query(const MODEL* m_def, const Fvector& r_start, const Fvector& r_dir, float r_range)
{
	PROF_EVENT("COLLIDER::ray_query");
	m_def->wait_loading();

	if (!m_def || m_def->tree == nullptr)
		return;

	// Get nodes
	const AABBNoLeafTree* T = (const AABBNoLeafTree*)m_def->tree->GetTree();
	const AABBNoLeafNode* N = T->GetNodes();

	r_clear();
	r_vec().reserve(16);

	cform_ray_collider RC
	{
		r_start,
		r_dir,
		this,
		m_def->tris,
		m_def->verts,
		r_range,
		r_range*r_range,

		!!(ray_mode & OPT_CULL),
		!!(ray_mode & OPT_ONLYFIRST),
		!!(ray_mode & OPT_ONLYNEAREST)
	};
	RC._stab(N);
}