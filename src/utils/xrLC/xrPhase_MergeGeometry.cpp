#include "StdAfx.h"

#include "Build.h"
#include "../xrLC_Light/xrFace.h"

#include <thread>
#include <ppl.h>

// Stuff For need
void MakeCube(Fbox& BB_dest, const Fbox& BB_src)
{
	Fvector C, D;
	BB_src.get_CD(C, D);
	float max = D.x;
	if (D.y > max)
	{
		max = D.y;
	}
	if (D.z > max)
	{
		max = D.z;
	}

	BB_dest.set(C, C);
	BB_dest.grow(max);
}

bool ValidateMergeLinearSize(const Fvector& merged, const Fvector& orig1, const Fvector& orig2, int iAxis)
{
	if ((merged[iAxis] > (4 * c_SS_maxsize / 3)) &&
		(merged[iAxis] > (orig1[iAxis] + 1)) &&
		(merged[iAxis] > (orig2[iAxis] + 1)))
	{
		return false;
	}
	else
	{
		return true;
	}
}

ICF void CreateBox(vecFace& subdiv, Fbox& bb_base)
{
	for (u32 it = 0; it < subdiv.size(); it++)
	{
		Face* F = subdiv[it];
		bb_base.modify(F->v[0]->P);
		bb_base.modify(F->v[1]->P);
		bb_base.modify(F->v[2]->P);
	}
} 

bool NeedMerge(vecFace& subdiv, Fbox& bb_base)
{
	// 1. Amount of polygons
	if (subdiv.size() >= u32(3 * c_SS_HighVertLimit / 4))
	{
		return false;
	}

	Fvector sz_base;

	// 2. Bounding box
	bb_base.invalidate();
	CreateBox(subdiv, bb_base);

	bb_base.grow(EPS_S); // Enshure non-zero volume
	bb_base.getsize(sz_base);
	if (sz_base.x < c_SS_maxsize)
	{
		return true;
	}
	if (sz_base.y < c_SS_maxsize)
	{
		return true;
	}
	if (sz_base.z < c_SS_maxsize)
	{
		return true;
	}

	return true;
}
 

extern void xrPhase_MergeGeometry_Tbb();
void CBuild::xrPhase_MergeGeometry()
{
	string128 tmp;
	sprintf(tmp, "Merge Started... [%zu]", g_XSplit.size());
	clMsg(tmp);

	u32 Recalculated = 0;
	while (g_XSplit.size() != Recalculated)
	{
 		Recalculated = g_XSplit.size();
		xrPhase_MergeGeometry_Tbb();
	}

	// Проверяем на INFINITY
	validate_splits();

	AditionalData("Splits Merged [%u]", g_XSplit.size());
}