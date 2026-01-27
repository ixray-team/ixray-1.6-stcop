#include "stdafx.h"

#include "xrLight_ImplicitCalcGlobs.h"
#include "xrLight_ImplicitDeflector.h"
#include "xrFace.h"
#include "xrLC_GlobalData.h"

#include "hash2D.h"

hash2D <Face*, 768, 768> hash2dImpl;

vecFace& ImplicitCalcGlobs::query(float px, float py)
{
	return hash2dImpl.query(px, py);
}

void	ImplicitCalcGlobs::Initialize( ImplicitDeflector &d )
{
	defl = &d;
	R_ASSERT( defl );
	Fbox2 bounds;
	defl->Bounds_Summary			(bounds);
	
	hash2dImpl.initialize	(bounds,(u32)defl->faces.size());
	for (u32 fid=0; fid<defl->faces.size(); fid++)
	{
		Face* F				= defl->faces[fid];
		F->AddChannel		(F->tc[0].uv[0],F->tc[0].uv[1],F->tc[0].uv[2]); // make compatible format with LMAPs
		defl->Bounds			(fid,bounds);
		hash2dImpl.add	(bounds,F);
	}
}