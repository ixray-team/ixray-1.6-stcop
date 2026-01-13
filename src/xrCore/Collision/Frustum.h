// Frustum.h: interface for the CFrustum class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "xrCDB.h"

#pragma pack(push,4)

enum EFC_Visible 
{
	fcvNone = 0,       // Полностью за пределами фрустума — не рисуем
	fcvPartial = 1,       // Частично в фрустуме — можно рисовать
	fcvFully = 2,       // Полностью в фрустуме — можно точно рисовать
	fcv_forcedword = u32(-1) // Зарезервировано, обычно не используется
};


#define FRUSTUM_MAXPLANES	12
#define FRUSTUM_P_LEFT		(1<<0)
#define FRUSTUM_P_RIGHT		(1<<1)
#define FRUSTUM_P_TOP		(1<<2)
#define FRUSTUM_P_BOTTOM	(1<<3)
#define FRUSTUM_P_NEAR		(1<<4)
#define FRUSTUM_P_FAR		(1<<5)

#define FRUSTUM_P_LRTB		(FRUSTUM_P_LEFT|FRUSTUM_P_RIGHT|FRUSTUM_P_TOP|FRUSTUM_P_BOTTOM)
#define FRUSTUM_P_ALL		(FRUSTUM_P_LRTB|FRUSTUM_P_NEAR|FRUSTUM_P_FAR)

#define FRUSTUM_SAFE		(FRUSTUM_MAXPLANES*4)
typedef svector<Fvector,FRUSTUM_SAFE>		sPoly;
extern XRCORE_API u32 frustum_aabb_remap[8][6];

class XRCORE_API	CFrustum
{
public:
	struct fplane	: public Fplane
	{
		u32			aabb_overlap_id = 0;	// [0..7]
		void		cache	();	
	};
	fplane			planes[FRUSTUM_MAXPLANES] = {};
	int				p_count = 0;

public:
	ICF EFC_Visible		AABB_OverlapPlane	(const fplane& P, const float* mM) const
	{
		// calc extreme pts (neg,pos) along normal axis (pos in dir of norm, etc.)
		u32*	id		= frustum_aabb_remap[P.aabb_overlap_id];

		Fvector			Neg;
		Neg.set			(mM[id[3]],mM[id[4]],mM[id[5]]);
		if				(P.classify(Neg) > 0)	return	fcvNone;

		Fvector			Pos;
		Pos.set			(mM[id[0]],mM[id[1]],mM[id[2]]);
		if				(P.classify(Pos) <= 0)	return	fcvFully;

		return			fcvPartial;
	}
public:
	IC void			_clear				()				{ p_count=0; }
	void			_add				(Fplane &P);
	void			_add				(Fvector& P1, Fvector& P2, Fvector& P3);

	void			SimplifyPoly_AABB	(sPoly* P, Fplane& plane);

	void			CreateOccluder		(Fvector* p,	int count,		Fvector& vBase, CFrustum& clip);
	BOOL			CreateFromClipPoly	(Fvector* p,	int count,		Fvector& vBase, CFrustum& clip);	// returns 'false' if creation failed
	void			CreateFromPoints	(Fvector* p,	int count,		Fvector& vBase );
	void			CreateFromMatrix	(Fmatrix &M,	u32 mask);
	void			CreateFromPortal	(sPoly* P,		Fvector& vPN,	Fvector& vBase, Fmatrix& mFullXFORM);
	void			CreateFromPlanes	(Fplane* p,		int count);

	ICF sPoly* ClipPoly(sPoly& S, sPoly& D) const
	{
		sPoly* src = &D;
		sPoly* dest = &S;
		for (int i = 0; i < p_count; i++)
		{
			// cache plane and swap lists
			const fplane& P = planes[i];
			std::swap(src, dest);
			dest->clear();

			// classify all points relative to plane #i
			float	cls[FRUSTUM_SAFE];
			for (u32 j = 0; j < src->size(); j++) cls[j] = P.classify((*src)[j]);

			// clip everything to this plane
			cls[src->size()] = cls[0];
			src->push_back((*src)[0]);
			Fvector D_; float denum, t;
			for (u32 j = 0; j < src->size() - 1; j++)
			{
				if ((*src)[j].similar((*src)[j + 1], EPS_S)) continue;

				if (negative(cls[j]))
				{
					dest->push_back((*src)[j]);
					if (positive(cls[j + 1]))
					{
						// segment intersects plane
						D_.sub((*src)[j + 1], (*src)[j]);
						denum = P.n.dotproduct(D_);
						if (denum != 0) {
							t = -cls[j] / denum; //VERIFY(t<=1.f && t>=0);
							dest->last().mad((*src)[j], D_, t);
							dest->inc();
						}
					}
				}
				else {
					// J - outside
					if (negative(cls[j + 1]))
					{
						// J+1  - inside
						// segment intersects plane
						D_.sub((*src)[j + 1], (*src)[j]);
						denum = P.n.dotproduct(D_);
						if (denum != 0) {
							t = -cls[j] / denum; //VERIFY(t<=1.f && t>=0);
							dest->last().mad((*src)[j], D_, t);
							dest->inc();
						}
					}
				}
			}

			// here we end up with complete polygon in 'dest' which is inside plane #i
			if (dest->size() < 3) return 0;
		}
		return dest;
	}

	u32				getMask				() const { return (1<<p_count)-1; }

	ICF EFC_Visible	testSphere(Fvector& c, float r, u32& test_mask) const
	{
		u32	bit = 1;
		for (int i = 0; i < p_count; i++, bit <<= 1)
		{
			if (test_mask & bit) {
				float cls = planes[i].classify(c);
				if (cls > r) { test_mask = 0; return fcvNone; }	// none  - return
				if (std::abs(cls) >= r) test_mask &= ~bit;			// fully - no need to test this plane
			}
		}
		return test_mask ? fcvPartial : fcvFully;
	}

	ICF BOOL testSphere_dirty(const Fvector& c, float r) const
	{
		switch (p_count) {
		case 12:if (planes[11].classify(c) > r)	return FALSE;
		case 11:if (planes[10].classify(c) > r)	return FALSE;
		case 10:if (planes[9].classify(c) > r)	return FALSE;
		case 9:	if (planes[8].classify(c) > r)	return FALSE;
		case 8:	if (planes[7].classify(c) > r)	return FALSE;
		case 7:	if (planes[6].classify(c) > r)	return FALSE;
		case 6:	if (planes[5].classify(c) > r)	return FALSE;
		case 5:	if (planes[4].classify(c) > r)	return FALSE;
		case 4:	if (planes[3].classify(c) > r)	return FALSE;
		case 3:	if (planes[2].classify(c) > r)	return FALSE;
		case 2:	if (planes[1].classify(c) > r)	return FALSE;
		case 1:	if (planes[0].classify(c) > r)	return FALSE;
		case 0:	break;
		default:	NODEFAULT;
		}
		return TRUE;
	}
	ICF EFC_Visible	testAABB(const float* mM, u32& test_mask) const
	{
		// go for trivial rejection or acceptance using "faster overlap test"
		u32		bit = 1;

		for (int i = 0; i < p_count; i++, bit <<= 1)
		{
			if (test_mask & bit) {
				EFC_Visible	r = AABB_OverlapPlane(planes[i], mM);
				if (fcvFully == r)	test_mask &= ~bit;					// fully - no need to test this plane
				else if (fcvNone == r) { test_mask = 0; return fcvNone; }	// none - return
			}
		}
		return test_mask ? fcvPartial : fcvFully;
	}

	ICF EFC_Visible	testSAABB(Fvector& c, float r, const float* mM, u32& test_mask) const
	{
		u32	bit = 1;
		for (int i = 0; i < p_count; i++, bit <<= 1)
		{
			if (test_mask & bit)
			{
				float cls = planes[i].classify(c);
				if (cls > r) { test_mask = 0; return fcvNone; }	// none  - return
				if (std::abs(cls) >= r) test_mask &= ~bit;			// fully - no need to test this plane
				else {
					EFC_Visible	r_ = AABB_OverlapPlane(planes[i], mM);
					if (fcvFully == r_)	test_mask &= ~bit;					// fully - no need to test this plane
					else if (fcvNone == r_) { test_mask = 0; return fcvNone; }	// none - return
				}
			}
		}
		return test_mask ? fcvPartial : fcvFully;
	}

	ICF BOOL testPoint(const Fvector& pt) const
	{
		for (int i = 0; i < p_count; i++)
		{
			if (planes[i].classify(pt) > 0.0f)
				return FALSE;
		}
		return TRUE;
	}

	ICF BOOL testPolyInside_dirty(Fvector* p, int count) const
	{
		Fvector* e = p + count;
		for (int i = 0; i < p_count; i++)
		{
			const fplane& P = planes[i];
			for (Fvector* I = p; I != e; I++)
				if (P.classify(*I) > 0) return false;
		}
		return true;
	}

	ICF BOOL testPolyInside(sPoly& src)											const
    {
    	sPoly d;
        return !!ClipPoly(src,d);
    }
	ICF BOOL testPolyInside(Fvector* p, int count)									const
    {
    	sPoly src(p,count);
        return testPolyInside(src);
    }
};
#pragma pack(pop)