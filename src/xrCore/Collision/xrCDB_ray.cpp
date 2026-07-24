#include "stdafx.h"
#include "xrCDB.h"
#include "override/Model.h"
#include "cl_intersect.h"
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

	ICF void _prim(size_t prim)
	{
		float u,v,r;
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (!TestRayTri(pos, fwd_dir, tri_verts, u, v, r, bCull))
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

		Fvector P;
		if (!Fbox(center-extents,center+extents).Pick2(pos, fwd_dir, P))
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
	if (!m_def)
		return;

	m_def->wait_loading();
	if (m_def->tree == nullptr)
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