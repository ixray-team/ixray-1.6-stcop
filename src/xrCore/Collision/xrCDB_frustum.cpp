#include "stdafx.h"


#include "xrCDB.h"
#include "override/Model.h"

#include "Frustum.h"

using namespace CDB;
using namespace Opcode;

struct cform_frustum_collider final
{
	COLLIDER* dest;
	const xr_vector<TRI>& tris;
	const xr_vector<Fvector>& verts;
	const CFrustum& F;

	bool bClass3, bFirst;

	sPoly Src, Dst;

	ICF void Prim(size_t InPrim)
	{
		auto& Tri = tris[InPrim];
		auto& TriVerts = Tri.verts;
		Fvector tri_verts[3] = { verts[TriVerts[0]], verts[TriVerts[1]], verts[TriVerts[2]] };

		if (bClass3)
		{
			Src.resize(3);
			Fvector* src = Src.begin();
			src[0] = tri_verts[0];
			src[1] = tri_verts[1];
			src[2] = tri_verts[2];
			if (F.ClipPoly(Src, Dst))
			{
				RESULT& R = dest->r_add();
				R.id = InPrim;
				R.verts[0] = tri_verts[0];
				R.verts[1] = tri_verts[1];
				R.verts[2] = tri_verts[2];
				R.dummy = tris[InPrim].dummy;
			}
		}
		else
		{
			RESULT& R = dest->r_add();
			R.id = InPrim;
			R.verts[0] = tri_verts[0];
			R.verts[1] = tri_verts[1];
			R.verts[2] = tri_verts[2];
			R.dummy = tris[InPrim].dummy;
		}
	}

	void Stab(const AABBNoLeafNode* node, u32 mask)
	{
		// Actual frustum/aabb test
		Fvector& center = (Fvector&)node->mAABB.mCenter;
		Fvector& extents = (Fvector&)node->mAABB.mExtents;
		if (fcvNone == F.testAABB(Fbox(center-extents,center+extents).data(), mask))	return;

		// 1st chield
		if (node->HasPosLeaf())
			Prim(node->GetPosPrimitive());
		else 
			Stab(node->GetPos(), mask);

		// Early exit for "only first"
		if (bFirst) 
		{
			if (dest->r_count()) 
				return;
		}

		// 2nd chield
		if (node->HasNegLeaf())
			Prim(node->GetNegPrimitive());
		else 
			Stab(node->GetNeg(), mask);
	}
};

void COLLIDER::frustum_query(const MODEL* m_def, const CFrustum& F)
{
	PROF_EVENT("COLLIDER::frustum_query");
	if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();

	r_clear();
	r_vec().reserve(16);

	cform_frustum_collider BC
	{
		this,
		m_def->tris,
		m_def->verts,
		F,
		!!(frustum_mode & OPT_FULL_TEST),
		!!(frustum_mode & OPT_ONLYFIRST)
	};
	BC.Stab(m_def->tree->GetTree()->GetNodes(), F.getMask());
}

struct cform_custom_collider final
{
	bool(*AABBCheck)(const Fvector&, const Fvector&, bool, void*);
	void* paabbc = nullptr;
	void(*GetTris)(size_t, void*);
	void* ptric = nullptr;
	void Stab(const AABBNoLeafNode* node)
	{
		bool pos_leaf = node->HasPosLeaf();
		bool neg_leaf = node->HasNegLeaf();
		if (nullptr==AABBCheck || !AABBCheck((Fvector&)node->mAABB.mCenter, (Fvector&)node->mAABB.mExtents, pos_leaf||neg_leaf, paabbc)) return;

		// 1st chield
		if (pos_leaf)
		{
			if (GetTris)
				GetTris(node->GetPosPrimitive(), ptric);
		}
		else
			Stab(node->GetPos());

		// 2nd chield
		if (neg_leaf)
		{
			if (GetTris)
				GetTris(node->GetNegPrimitive(), ptric);
		}
		else
			Stab(node->GetNeg());
	}
};

void COLLIDER::custom_query(const MODEL* m_def, bool(AABBCheckF)(const Fvector&, const Fvector&, bool, void*), void* paabbc, void(GetTrisF)(size_t, void*), void* ptric)
{
	PROF_EVENT("COLLIDER::custom_query");
	if (!m_def || m_def->tree == nullptr)
		return;

	m_def->wait_loading();

	cform_custom_collider CC
	{
		AABBCheckF,
		paabbc,
		GetTrisF,
		ptric
	};
	CC.Stab(m_def->tree->GetTree()->GetNodes());
}