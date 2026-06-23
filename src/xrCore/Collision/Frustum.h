#pragma once

#include "xrCDB.h"

#pragma pack(push, 4)

enum EFC_Visible
{
	fcvNone = 0,			 // Полностью за пределами фрустума — не рисуем
	fcvPartial = 1,			 // Частично в фрустуме — можно рисовать
	fcvFully = 2,			 // Полностью в фрустуме — можно точно рисовать
	fcv_forcedword = u32(-1) // Зарезервировано, обычно не используется
};

enum : u32
{
	FRUSTUM_MAXPLANES = 12,
	FRUSTUM_SAFE = FRUSTUM_MAXPLANES * 4,
	FRUSTUM_P_LEFT = 1 << 0,
	FRUSTUM_P_RIGHT = 1 << 1,
	FRUSTUM_P_TOP = 1 << 2,
	FRUSTUM_P_BOTTOM = 1 << 3,
	FRUSTUM_P_NEAR = 1 << 4,
	FRUSTUM_P_FAR = 1 << 5,
	FRUSTUM_P_LRTB = FRUSTUM_P_LEFT | FRUSTUM_P_RIGHT | FRUSTUM_P_TOP | FRUSTUM_P_BOTTOM,
	FRUSTUM_P_ALL = FRUSTUM_P_LRTB | FRUSTUM_P_NEAR | FRUSTUM_P_FAR,
	FRUSTUM_P_DUMMY = u32(-1)
};

using sPoly = svector<Fvector, FRUSTUM_SAFE>;

enum : u8
{
	mx = 0,
	my = 1,
	mz = 2,
	Mx = 3,
	My = 4,
	Mz = 5
};

IC u32 frustum_aabb_remap[8][6] =
	{
		{Mx, My, Mz, mx, my, mz},
		{Mx, My, mz, mx, my, Mz},
		{Mx, my, Mz, mx, My, mz},
		{Mx, my, mz, mx, My, Mz},
		{mx, My, Mz, Mx, my, mz},
		{mx, My, mz, Mx, my, Mz},
		{mx, my, Mz, Mx, My, mz},
		{mx, my, mz, Mx, My, Mz}
};

class XRCORE_API CFrustum
{
public:
	struct fplane : Fplane
	{
		u32 aabb_overlap_id = 0; // [0..7]
		ICF void cache()
		{
			if (positive(n.x))
			{
				if (positive(n.y))
				{
					if (positive(n.z))
					{
						aabb_overlap_id = 0;
					}
					else
					{
						aabb_overlap_id = 1;
					}
				}
				else
				{
					if (positive(n.z))
					{
						aabb_overlap_id = 2;
					}
					else
					{
						aabb_overlap_id = 3;
					}
				}
			}
			else
			{
				if (positive(n.y))
				{
					if (positive(n.z))
					{
						aabb_overlap_id = 4;
					}
					else
					{
						aabb_overlap_id = 5;
					}
				}
				else
				{
					if (positive(n.z))
					{
						aabb_overlap_id = 6;
					}
					else
					{
						aabb_overlap_id = 7;
					}
				}
			}
		}
	};
	fplane planes[FRUSTUM_MAXPLANES] = {};
	int p_count = 0;

	ICF EFC_Visible AABB_OverlapPlane(const fplane& P, const float* mM) const;
	ICF void _clear();
	ICF void _add(Fplane& P);
	ICF void _add(Fvector& P1, Fvector& P2, Fvector& P3);
	ICF CFrustum& CreateFromPoints(Fvector* p, int count, Fvector& COP);
	ICF CFrustum& CreateFromPlanes(Fplane* p, int count);
	ICF CFrustum& CreateFromPortal(sPoly* poly, Fvector& vPN, Fvector& vBase, Fmatrix& mFullXFORM);
	ICF void SimplifyPoly_AABB(sPoly* poly, Fplane& plane);
	ICF CFrustum& CreateOccluder(Fvector* p, int count, Fvector& vBase, CFrustum& clip);
	ICF bool CreateFromClipPoly(Fvector* p, int count, Fvector& vBase, CFrustum& clip);
	ICF CFrustum& CreateFromMatrix(Fmatrix& M, u32 mask);

	ICF sPoly* ClipPoly(sPoly& S, sPoly& D) const;

	ICF u32 getMask() const;

	ICF EFC_Visible testSphere(Fvector& c, float r, u32& test_mask) const;

	ICF bool testSphere_dirty(const Fvector& c, float r) const;
	ICF EFC_Visible testAABB(const float* mM, u32& test_mask) const;

	ICF EFC_Visible testSAABB(Fvector& c, float r, const float* mM, u32& test_mask) const;

	ICF bool testPoint(const Fvector& pt) const;

	ICF bool testPolyInside_dirty(Fvector* p, int count) const;

	ICF bool testPolyInside(sPoly& src) const;
	ICF bool testPolyInside(Fvector* p, int count) const;
};
#pragma pack(pop)