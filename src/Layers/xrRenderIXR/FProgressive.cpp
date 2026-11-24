// FProgressive.cpp: implementation of the FProgressive class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "../../xrEngine/Fmesh.h"
#include "FProgressive.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

FProgressive::FProgressive	() : Fvisual()
{
	last_lod		= 0;
}

FProgressive::~FProgressive	()
{

}

void FProgressive::Release	()
{
	Fvisual::Release();
	xr_free			(nSWI.sw);
}

void FProgressive::Load		(const char* N, IReader *data, u32 dwFlags)
{
	PROF_EVENT(__FUNCTION__);
	Fvisual::Load	(N,data,dwFlags);

	// normal SWI
	destructor<IReader> lods (data->open_chunk	(OGF_SWIDATA));
    nSWI.reserved[0]	= lods().r_u32();	// reserved 16 bytes
    nSWI.reserved[1]	= lods().r_u32();
    nSWI.reserved[2]	= lods().r_u32();
    nSWI.reserved[3]	= lods().r_u32();
    nSWI.count			= lods().r_u32();
	VERIFY				(nullptr==nSWI.sw);
    nSWI.sw				= xr_alloc<FSlideWindow>(nSWI.count);
	lods().r			(nSWI.sw,nSWI.count*sizeof(FSlideWindow));
}

void FProgressive::Render	(GfxCmdList& Cmd, float LOD)
{
	//PROF_EVENT("FProgressive::Render");
	int lod_id		= last_lod;
	if (LOD>=0.f){
		clamp			(LOD,0.f,1.f);
		lod_id			= iFloor((1.f-LOD)*float(nSWI.count-1)+0.5f);
		last_lod		= lod_id;
	}
	VERIFY				(lod_id>=0 && lod_id<int(nSWI.count));
	FSlideWindow& SW	= nSWI.sw[lod_id];
	Cmd.SetGeometry	(rm_geom);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, SW.num_tris * 3, iBase+SW.offset, vBase);
}

#define PCOPY(a)	a = pFrom->a
void	FProgressive::Copy	(dxRender_Visual *pSrc)
{
	Fvisual::Copy	(pSrc);
	FProgressive	*pFrom = (FProgressive *)pSrc;
	PCOPY			(nSWI);
}
