#include "stdafx.h"


#include "xrCDB.h"
#include "override/Model.h"

using namespace CDB;
using namespace Opcode;

struct cform_box_collider final
{
	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	
	Fbox box;
	bool bClass3, bFirst;

	ICF void _prim(size_t prim)
	{
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };
		if (!box.intersectTri(tri_verts, bClass3))
			return;
		RESULT& R = dest->r_add();
		R.id = prim;
		R.verts[0] = tri_verts[0];
		R.verts[1] = tri_verts[1];
		R.verts[2] = tri_verts[2];
		R.dummy = Tri.dummy;
	}
	void _stab(const AABBNoLeafNode* node)
	{
		// Actual box-box test
		Fvector& center = (Fvector&)node->mAABB.mCenter;
		Fvector& extents = (Fvector&)node->mAABB.mExtents;
		if (!box.intersect(Fbox{center-extents,center+extents}))
			return;
		
		// 1st chield
		if (node->HasPosLeaf())	_prim	(node->GetPosPrimitive());
		else					_stab	(node->GetPos());
		
		// Early exit for "only first"
		if (bFirst && dest->r_count())
			return;
		
		// 2nd chield
		if (node->HasNegLeaf())	_prim	(node->GetNegPrimitive());
		else					_stab	(node->GetNeg());
	}
};

void COLLIDER::box_query(const MODEL *m_def, const Fbox& _box)
{
	PROF_EVENT("COLLIDER::box_query");
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

	cform_box_collider BC
	{
		this,
		m_def->tris,
		m_def->verts,
		_box,
		!!(box_mode & OPT_FULL_TEST),
		!!(box_mode & OPT_ONLYFIRST)
	};
	BC._stab(N);
}

struct cform_obb_collider final
{
	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	Fobb obb;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void _prim(size_t prim)
	{
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (!obb.intersectTri(tri_verts, bClass3))
			return;

		RESULT& R = dest->r_add();
		R.id = prim;
		R.verts[0] = tri_verts[0];
		R.verts[1] = tri_verts[1];
		R.verts[2] = tri_verts[2];
		R.dummy = Tri.dummy;
	}

	void _stab(const AABBNoLeafNode* node)
	{
		// Actual OBB-AABB test
		if (!obb.intersectAABB((Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents)) return;

		// 1st child
		if (node->HasPosLeaf())	_prim(node->GetPosPrimitive());
		else					_stab(node->GetPos());

		// Early exit for "only first"
		if (bFirst && dest->r_count()) return;

		// 2nd child
		if (node->HasNegLeaf())	_prim(node->GetNegPrimitive());
		else					_stab(node->GetNeg());
	}
};

void COLLIDER::obb_query(const MODEL* m_def, const Fobb& obb)
{
	PROF_EVENT("COLLIDER::obb_query");
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

	cform_obb_collider OC
	{
		this,
		m_def->tris,
		m_def->verts,
		obb,
		!!(obb_mode & OPT_FULL_TEST),
		!!(obb_mode & OPT_ONLYFIRST)
	};
	OC._stab(N);
}

struct cform_sphere_collider final
{
	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	Fsphere sphere;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void _prim(size_t prim)
	{
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (!sphere.intersectTri(tri_verts, bClass3))
			return;

		RESULT& R = dest->r_add();
		R.id = prim;
		R.verts[0] = tri_verts[0];
		R.verts[1] = tri_verts[1];
		R.verts[2] = tri_verts[2];
		R.dummy = Tri.dummy;
	}

	void _stab(const AABBNoLeafNode* node)
	{
		// Actual Sphere-AABB test
		if (!sphere.intersectAABB((Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents)) return;

		// 1st child
		if (node->HasPosLeaf())	_prim(node->GetPosPrimitive());
		else					_stab(node->GetPos());

		// Early exit for "only first"
		if (bFirst && dest->r_count()) return;

		// 2nd child
		if (node->HasNegLeaf())	_prim(node->GetNegPrimitive());
		else					_stab(node->GetNeg());
	}
};

void COLLIDER::sphere_query(const MODEL* m_def, const Fsphere& sphere)
{
	PROF_EVENT("COLLIDER::sphere_query");
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

	cform_sphere_collider SC
	{
		this,
		m_def->tris,
		m_def->verts,
		sphere,
		!!(sphere_mode & OPT_FULL_TEST),
		!!(sphere_mode & OPT_ONLYFIRST)
	};
	SC._stab(N);
}
