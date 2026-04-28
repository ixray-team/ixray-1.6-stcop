#include "stdafx.h"
#include "xrLight_ImplicitDeflector.h"
#include "b_build_texture.h"
#include "xrFace.h"
#include "xrLC_GlobalData.h"

u32	ImplicitDeflector::Width	()						
{
	return texture->dwWidth; 
}
u32	ImplicitDeflector::Height	()						
{
	return texture->dwHeight; 
}
	
u32& ImplicitDeflector::Texel(u32 x, u32 y)
{
	u32* raw = static_cast<u32*>(*texture->pSurface);
	return raw[y * Width() + x];
}

void	ImplicitDeflector::Bounds	(u32 ID, Fbox2& dest)
{
	Face* F		= faces[ID];
	_TCF& TC	= F->tc[0];
	dest.min.set	(TC.uv[0]);
	dest.max.set	(TC.uv[0]);
	dest.modify		(TC.uv[1]);
	dest.modify		(TC.uv[2]);
}

void	ImplicitDeflector::Bounds_Summary (Fbox2& bounds)
{
	bounds.invalidate();
	for (u32 I=0; I<faces.size(); I++)
	{
		Fbox2	B;
		Bounds	(I,B);
		bounds.merge(B);
	}
} 

// Client Global

#include "hash2D.h"

hash2D <Face*, 768, 768> hash2dImpl;

vecFace& ImplicitCalcGlobs::query(float px, float py)
{
	return hash2dImpl.query(px, py);
}

void	ImplicitCalcGlobs::Initialize(ImplicitDeflector& d)
{
	defl = &d;
	R_ASSERT(defl);
	Fbox2 bounds;
	defl->Bounds_Summary(bounds);

	hash2dImpl.initialize(bounds, (u32)defl->faces.size());
	for (u32 fid = 0; fid < defl->faces.size(); fid++)
	{
		Face* F = defl->faces[fid];
		F->AddChannel(F->tc[0].uv[0], F->tc[0].uv[1], F->tc[0].uv[2]); // make compatible format with LMAPs
		defl->Bounds(fid, bounds);
		hash2dImpl.add(bounds, F);
	}
}