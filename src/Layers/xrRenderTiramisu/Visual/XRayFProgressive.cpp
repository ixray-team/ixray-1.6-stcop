#include "stdafx.h"
#include "XRayFProgressive.h"


CDS0_FProgressive::CDS0_FProgressive()
{
	xSWI = 0;
}

CDS0_FProgressive::~CDS0_FProgressive()
{

}

void CDS0_FProgressive::Release()
{
	xr_free(nSWI.sw);
	if (xSWI) {
		xr_free(xSWI->sw);
		xr_delete(xSWI);
		xSWI = 0;

	}
}

void CDS0_FProgressive::Load(const char* N, IReader* data, u32 dwFlags)
{
	CDS0_FVisual::Load(N, data, dwFlags| VLOAD_SWI);
}

#define PCOPY(a)	a = pFrom->a
void CDS0_FProgressive::Copy(CDS0_RenderVisual* from)
{
	CDS0_FVisual::Copy(from);
	CDS0_FProgressive* pFrom = (CDS0_FProgressive*)from;
	PCOPY(nSWI);
	PCOPY(xSWI);
}
