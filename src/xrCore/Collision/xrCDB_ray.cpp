#include "stdafx.h"


#include "xrCDB.h"
#include "override/Model.h"
#include "sse_intersect.h"

using namespace		CDB;
using namespace		Opcode;

class _MM_ALIGN16 cform_ray_collider
{
public:
	// Intersection Filter
	bool			UseIntersectionFilter = false;
	bool			continue_work = true;
	OpcodeContext*	ctxt = 0;

	// Старый код

	COLLIDER*		dest;
	TRI*			tris;
	Fvector*		verts;
	
	ray_t			ray;
	float			rRange;
	float			rRange2;

	bool bSSE = false;
	bool bCull = false;
	bool bFirst = false;
	bool bNearest = false;

	cform_ray_collider(bool SSE, bool Cull, bool First, bool Nearest)
		:bSSE(SSE), bCull(Cull), bFirst(First), bNearest(Nearest) {}

	IC void _init(COLLIDER* CL, Fvector* V, TRI* T, const Fvector& C, const Fvector& D, float R)
	{
		dest			= CL;
		tris			= T;
		verts			= V;
		ray.pos.set		(C);
		ray.inv_dir.set	(1.f,1.f,1.f).div(D);
		ray.fwd_dir.set	(D);
		rRange			= R;
		rRange2			= R*R;
	}

	IC void _init_intersection(COLLIDER* CL, CDB::MODEL* model, const Fvector& C, const Fvector& D, float R)
	{
		tris = model->get_tris();
		verts = model->get_verts();

		dest = CL;
 		ray.pos.set(C);
		ray.inv_dir.set(1.f, 1.f, 1.f).div(D);
		ray.fwd_dir.set(D);
		rRange = R;
		rRange2 = R * R;

		if (ctxt)
			UseIntersectionFilter = ctxt->filterIntersect != nullptr;
	}

	// sse

	IC bool _tri(u32* p, float& u, float& v, float& range)
	{
		Fvector edge1, edge2, tvec, pvec, qvec;
		float	det,inv_det;
		
		// find vectors for two edges sharing vert0
		Fvector&			p0	= verts[ p[0] ];
		Fvector&			p1	= verts[ p[1] ];
		Fvector&			p2	= verts[ p[2] ];
		edge1.sub			(p1, p0);
		edge2.sub			(p2, p0);
		// begin calculating determinant - also used to calculate U parameter
		// if determinant is near zero, ray lies in plane of triangle
		pvec.crossproduct	(ray.fwd_dir, edge2);
		det = edge1.dotproduct(pvec);

		if (bCull)
		{						
			if (det < EPS)  return false;
			tvec.sub(ray.pos, p0);						// calculate distance from vert0 to ray origin
			u = tvec.dotproduct(pvec);					// calculate U parameter and test bounds
			if (u < 0.f || u > det) return false;
			qvec.crossproduct(tvec, edge1);				// prepare to test V parameter
			v = ray.fwd_dir.dotproduct(qvec);			// calculate V parameter and test bounds
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
			tvec.sub(ray.pos, p0);						// calculate distance from vert0 to ray origin
			u = tvec.dotproduct(pvec)*inv_det;			// calculate U parameter and test bounds
			if (u < 0.0f || u > 1.0f)    return false;
			qvec.crossproduct(tvec, edge1);				// prepare to test V parameter
			v = ray.fwd_dir.dotproduct(qvec)*inv_det;	// calculate V parameter and test bounds
			if (v < 0.0f || u + v > 1.0f) return false;
			range = edge2.dotproduct(qvec)*inv_det;		// calculate t, ray intersects triangle
		}
		return true;
	}
	
	void _prim(DWORD prim)
	{
		float	u,v,r;
		if (!_tri(tris[prim].verts, u, v, r))	return;
		if (r<=0 || r>rRange)					return;
		
		if (bNearest)	
		{
			if (dest->r_count())	
			{
				RESULT& R = *dest->r_begin();
				if (r<R.range)	{
					R.id		= prim;
					R.range		= r;
					R.u			= u;
					R.v			= v;
					R.verts	[0]	= verts[tris[prim].verts[0]];
					R.verts	[1]	= verts[tris[prim].verts[1]];
					R.verts	[2]	= verts[tris[prim].verts[2]];
					R.dummy		= tris[prim].dummy;
					rRange		= r;
					rRange2		= r*r;
				}
			} else {
				RESULT& R	= dest->r_add();
				R.id		= prim;
				R.range		= r;
				R.u			= u;
				R.v			= v;
				R.verts	[0]	= verts[tris[prim].verts[0]];
				R.verts	[1]	= verts[tris[prim].verts[1]];
				R.verts	[2]	= verts[tris[prim].verts[2]];
				R.dummy		= tris[prim].dummy;
				rRange		= r;
				rRange2		= r*r;
			}
		}
		else
 		{
			if (UseIntersectionFilter)
			{
				// OpcodeArgs  data;
				ctxt->result->hit_struct.u = u;
				ctxt->result->hit_struct.v = v;
				ctxt->result->hit_struct.prim = prim;
				ctxt->result->hit_struct.dist = r;
				ctxt->filterIntersect(ctxt->result);
				continue_work = ctxt->result->valid;
				return;
			}

			RESULT& R	= dest->r_add();				// По порядку создает RESULT
			R.id		= prim;
			R.range		= r;
			R.u			= u;
			R.v			= v;
			R.verts	[0]	= verts[tris[prim].verts[0]];
			R.verts	[1]	= verts[tris[prim].verts[1]];
			R.verts	[2]	= verts[tris[prim].verts[2]];
			R.dummy		= tris[prim].dummy;
		}
	}

	void _stab(const AABBNoLeafNode* node)
	{
		// Intersection filter stoping 
		if (!continue_work)
			return;

		// Actual ray/aabb test
		// use SSE
		if (bSSE)
		{
			// Should help
			_mm_prefetch((char*)node->GetNeg(), _MM_HINT_NTA);

			float d;
			if (!_box_sse(ray, (Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents, d))
				return;
			if (d > rRange)
				return;
		}
		else
		{
			Fvector		P;
			if (!_box_fpu(ray, (Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents, P))
				return;
			if (P.distance_to_sqr(ray.pos) > rRange2)
				return;
		}

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
	PROF_EVENT("COLLIDER::ray_query")
	if (!m_def)
		return;

	const_cast<MODEL*>(m_def)->syncronize();

	// Get nodes
	const AABBNoLeafTree* T = (const AABBNoLeafTree*)m_def->tree->GetTree();
	const AABBNoLeafNode* N = T->GetNodes();
	r_clear();

	// SSE
	// Binary dispatcher
	cform_ray_collider RC(CPU::ID.hasFeature(CPUFeature::SSE), !!(ray_mode&OPT_CULL), !!(ray_mode&OPT_ONLYFIRST), !!(ray_mode&OPT_ONLYNEAREST));
	RC._init(this, m_def->verts, m_def->tris, r_start, r_dir, r_range);
	RC._stab(N);
}


ICF void CDB::COLLIDER::rayTrace1(OpcodeContext* context)
{
	MODEL* MDL = static_cast<MODEL*>(context->result->MDL);

	MDL->syncronize();

	// Get nodes
	const AABBNoLeafTree* T = (const AABBNoLeafTree*)MDL->tree->GetTree();
	const AABBNoLeafNode* N = T->GetNodes();
	r_clear();

	cform_ray_collider RC(CPU::ID.hasFeature(CPUFeature::SSE), true, false, false);
	RC.ctxt = context;
	RC._init_intersection(this, MDL, context->r_start, context->r_dir, context->r_range);
	RC._stab(N);
}
