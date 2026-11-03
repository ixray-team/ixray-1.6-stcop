#include "stdafx.h"


#include "xrCDB.h"
#include "override/Model.h"

using namespace CDB;
using namespace Opcode;

template <class Shape>
struct cform_shape_collider final
{
	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	Shape shape;

	bool bClass3 = false;
	bool bFirst = false;

	ICF void _prim(size_t prim)
	{
		auto& Tri = tris[prim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (!shape.intersectTri(tri_verts, bClass3))
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
		// Actual shape-AABB test
		if (!shape.intersectAABB((Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents)) return;

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

template <class Shape>
void COLLIDER::shape_query(const MODEL* m_def, const Shape& shape, u32 mode)
{
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

	cform_shape_collider<Shape> SC
	{
		this,
		m_def->tris,
		m_def->verts,
		shape,
		!!(mode & OPT_FULL_TEST),
		!!(mode & OPT_ONLYFIRST)
	};
	SC._stab(N);
}

void COLLIDER::box_query(const MODEL *m_def, const Fbox& _box)
{
	PROF_EVENT("COLLIDER::box_query");
	shape_query(m_def, _box, box_mode);
}

void COLLIDER::obb_query(const MODEL* m_def, const Fobb& obb)
{
	PROF_EVENT("COLLIDER::obb_query");
	shape_query(m_def, obb, obb_mode);
}

void COLLIDER::sphere_query(const MODEL* m_def, const Fsphere& sphere)
{
	PROF_EVENT("COLLIDER::sphere_query");
	shape_query(m_def, sphere, sphere_mode);
}

void COLLIDER::cylinder_query(const MODEL* m_def, const Fcylinder& cylinder)
{
	PROF_EVENT("COLLIDER::cylinder_query");
	shape_query(m_def, cylinder, cylinder_mode);
}

void COLLIDER::cone_query(const MODEL* m_def, const Fcone& cone)
{
	PROF_EVENT("COLLIDER::cone_query");
	shape_query(m_def, cone, cone_mode);
}
