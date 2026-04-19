#include "stdafx.h"
#include "Frustum.h"

//////////////////////////////////////////////////////////////////////
#define			mx			0
#define			my			1
#define			mz			2
#define			Mx			3
#define			My			4
#define			Mz			5

u32 XRCORE_API frustum_aabb_remap[8][6]
{
	{ Mx,My,Mz,mx,my,mz}, 
	{ Mx,My,mz,mx,my,Mz}, 
	{ Mx,my,Mz,mx,My,mz}, 
	{ Mx,my,mz,mx,My,Mz}, 
	{ mx,My,Mz,Mx,my,mz}, 
	{ mx,My,mz,Mx,my,Mz}, 
	{ mx,my,Mz,Mx,My,mz}, 
	{ mx,my,mz,Mx,My,Mz}
};

//////////////////////////////////////////////////////////////////////
CFrustum& CFrustum::CreateFromPoints(Fvector* p, int count, Fvector& COP)
{
	VERIFY(count<FRUSTUM_MAXPLANES);
	VERIFY(count>=3);

	_clear();
	for (int i=1; i<count; i++)
		_add(COP,p[i-1],p[i]);
	_add(COP,p[count-1],p[0]);

	return *this;
}

CFrustum& CFrustum::CreateFromPlanes(Fplane* p, int count)
{
	for (int k=0; k<count; k++)
		planes[k].set(p[k]);

	for (int i=0;i<count;i++)	{
		float denom		=	1.0f / planes[i].n.magnitude();// Get magnitude of Vector
		planes[i].n.x	*=	denom;
		planes[i].n.y	*=	denom;
		planes[i].n.z	*=	denom;
		planes[i].d		*=	denom;
		planes[i].cache	();
	}

	p_count = count;

	return *this;
}

CFrustum& CFrustum::CreateFromPortal(sPoly* poly, Fvector& vPN, Fvector& vBase, Fmatrix& mFullXFORM)
{
	Fplane	P;
	P.build_precise	((*poly)[0],(*poly)[1],(*poly)[2]);

	if (poly->size()>6) {
		SimplifyPoly_AABB(poly,P);
		P.build_precise	((*poly)[0],(*poly)[1],(*poly)[2]);
	}

	// Check plane orientation relative to viewer
	// and reverse if needed
	if (P.classify(vBase)<0)
	{
		std::reverse(poly->begin(),poly->end());
		P.build_precise	((*poly)[0],(*poly)[1],(*poly)[2]);
	}

	// Base creation
	CreateFromPoints(poly->begin(),poly->size(),vBase);

	// Near clipping plane
	_add		(P);

	// Far clipping plane
	Fmatrix &M	= mFullXFORM;
	P.n.x		= -(M._14 - M._13);
	P.n.y		= -(M._24 - M._23);
	P.n.z		= -(M._34 - M._33);
	P.d			= -(M._44 - M._43);
	float denom = 1.0f / P.n.magnitude();
	P.n.x		*= denom;
	P.n.y		*= denom;
	P.n.z		*= denom;
	P.d			*= denom;
	_add		(P);

	return *this;
}

void CFrustum::SimplifyPoly_AABB(sPoly* poly, Fplane& plane)
{
	Fmatrix		mView,mInv;
	Fvector		from,up,right,y;
	from.set	((*poly)[0]);
	y.set		(0,1,0);
	if (std::abs(plane.n.y)>0.99f) y.set(1,0,0);
	right.crossproduct		(y,plane.n);
	up.crossproduct			(plane.n,right);
	mView.build_camera_dir	(from,plane.n,up);

	// Project and find extents
	Fvector2	min,max;
	min.set		(flt_max,flt_max);
	max.set		(flt_min,flt_min);
	for (u32 i=0; i<poly->size(); i++)
	{
		Fvector2 tmp;
		mView.transform_tiny32(tmp,(*poly)[i]);
		min.min(tmp.x,tmp.y);
		max.max(tmp.x,tmp.y);
	}

	// Build other 2 points and inverse project
	Fvector2	p1,p2;
	p1.set		(min.x,max.y);
	p2.set		(max.x,min.y);
	mInv.invert	(mView);
	poly->clear	();

	mInv.transform_tiny23(poly->last(),min);	poly->inc();
	mInv.transform_tiny23(poly->last(),p1);		poly->inc();
	mInv.transform_tiny23(poly->last(),max);	poly->inc();
	mInv.transform_tiny23(poly->last(),p2);		poly->inc();
}

CFrustum& CFrustum::CreateOccluder(Fvector* p, int count, Fvector& vBase, CFrustum& clip)
{
	VERIFY(count<FRUSTUM_SAFE);
	VERIFY(count>=3);

	bool	edge[FRUSTUM_SAFE];
	float	cls	[FRUSTUM_SAFE];
	ZeroMemory	(edge,sizeof(edge));
	for (int i=0; i<clip.p_count; i++)
	{
		// classify all points relative to plane #i
		fplane &P = clip.planes[i];
		for (int j=0; j<count; j++) cls[j]= std::abs(P.classify(p[j]));

		// test edges to see which lies directly on plane
		for (int j=0; j<count; j++) {
			if (cls[j]<EPS_L)
			{
				int next = j+1; if (next>=count) next=0;
				if (cls[next]<EPS_L) {
					// both points lies on plane - mark as 'open'
					edge[j] = true;
				}
			}
		}
	}

	// here we have all edges marked accordenly to 'open' / 'closed' classification
	_clear	();
	_add	(p[0],p[1],p[2]);		// main plane
	for (int i=0; i<count; i++)
	{
		if (!edge[i]) {
			int next = i+1; if (next>=count) next=0;
			_add(vBase,p[i],p[next]);
		}
	}

	return *this;
}

bool CFrustum::CreateFromClipPoly(Fvector* p, int count, Fvector& vBase, CFrustum& clip)
{
	VERIFY(count<FRUSTUM_MAXPLANES);
	VERIFY(count>=3);

	sPoly	poly1	(p,count);
	sPoly	poly2;
	sPoly*	dest	= clip.ClipPoly(poly1,poly2);

	// here we end up with complete frustum-polygon in 'dest'
	if (nullptr==dest)	return false;

	CreateFromPoints(dest->begin(),dest->size(),vBase);
	return	true;
}

CFrustum& CFrustum::CreateFromMatrix(Fmatrix &M, u32 mask)
{
	VERIFY			(_valid(M));
	p_count			= 0;

	// Left clipping plane
	if (mask&FRUSTUM_P_LEFT)
	{
		planes[p_count].n.x		= -(M._14 + M._11);
		planes[p_count].n.y		= -(M._24 + M._21);
		planes[p_count].n.z		= -(M._34 + M._31);
		planes[p_count].d		= -(M._44 + M._41);
		p_count++;
	}

	// Right clipping plane
	if (mask&FRUSTUM_P_RIGHT)
	{
		planes[p_count].n.x		= -(M._14 - M._11);
		planes[p_count].n.y		= -(M._24 - M._21);
		planes[p_count].n.z		= -(M._34 - M._31);
		planes[p_count].d		= -(M._44 - M._41);
		p_count++;
	}

	// Top clipping plane
	if (mask&FRUSTUM_P_TOP)
	{
		planes[p_count].n.x		= -(M._14 - M._12);
		planes[p_count].n.y		= -(M._24 - M._22);
		planes[p_count].n.z		= -(M._34 - M._32);
		planes[p_count].d		= -(M._44 - M._42);
		p_count++;
	}

	// Bottom clipping plane
	if (mask&FRUSTUM_P_BOTTOM)
	{
		planes[p_count].n.x		= -(M._14 + M._12);
		planes[p_count].n.y		= -(M._24 + M._22);
		planes[p_count].n.z		= -(M._34 + M._32);
		planes[p_count].d		= -(M._44 + M._42);
		p_count++;
	}

	// Far clipping plane
	if (mask&FRUSTUM_P_FAR)
	{
		planes[p_count].n.x		= -(M._14 - M._13);
		planes[p_count].n.y		= -(M._24 - M._23);
		planes[p_count].n.z		= -(M._34 - M._33);
		planes[p_count].d		= -(M._44 - M._43);
		p_count++;
	}

	// Near clipping plane
	if (mask&FRUSTUM_P_NEAR)
	{
		planes[p_count].n.x		= -(M._14 + M._13);
		planes[p_count].n.y		= -(M._24 + M._23);
		planes[p_count].n.z		= -(M._34 + M._33);
		planes[p_count].d		= -(M._44 + M._43);
		p_count++;
	}

	for (int i=0;i<p_count;i++)
	{
		float denom		= 1.0f / planes[i].n.magnitude();// Get magnitude of Vector
		planes[i].n.x	*= denom;
		planes[i].n.y	*= denom;
		planes[i].n.z	*= denom;
		planes[i].d		*= denom;
		planes[i].cache	();
	}

	return *this;
}
