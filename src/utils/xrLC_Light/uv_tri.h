#pragma once
#include "tcf.h"
#include "xrFace.h"

struct XRLC_LIGHT_API UVtri : public _TCF
{
	Face* owner;

	bool similar(const UVtri& uv, float eps = EPS) const;

	// нормализованный AABB [0..1]
	Fvector2 uv_min_n;
	Fvector2 uv_max_n;

	// 🔥 НОВОЕ
	void computeAABB(const Fbox2& bounds);
	bool overlapsCell(u32 cx, u32 cy) const;
};

