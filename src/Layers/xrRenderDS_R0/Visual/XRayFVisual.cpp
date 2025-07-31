#include "stdafx.h"

#define VLOAD_NOVERTICES 1<<0
#include "XRayFVisual.h"

CDS0_FVisual::CDS0_FVisual()
{
	CountIndex = 0;
	OffsetIndex = 0;
	CountVertex = 0;
	OffsetVertex = 0;
	FVF = 0;
}

CDS0_FVisual::~CDS0_FVisual()
{
}

void CDS0_FVisual::Load(const char* N, IReader* data, u32 dwFlags)
{
	CDS0_RenderVisual::Load(N, data, dwFlags);
	bool Loaded = false;

	if (dwFlags & VLOAD_SWI)
	{
		destructor<IReader> lods(data->open_chunk(OGF_SWIDATA));
		nSWI.reserved[0] = lods().r_u32();	// reserved 16 bytes
		nSWI.reserved[1] = lods().r_u32();
		nSWI.reserved[2] = lods().r_u32();
		nSWI.reserved[3] = lods().r_u32();
		nSWI.count = lods().r_u32();
		VERIFY(NULL == nSWI.sw);
		nSWI.sw = xr_alloc<FSlideWindow>(nSWI.count);
		lods().r(nSWI.sw, nSWI.count * sizeof(FSlideWindow));
	}

	if (data->find_chunk(OGF_GCONTAINER))
	{
		Loaded = true;
		u32 ID = data->r_u32();
		OffsetVertex = data->r_u32();
		CountVertex = data->r_u32();

		ID = data->r_u32();
		OffsetIndex = data->r_u32();
		CountIndex = data->r_u32();
	}

	if (!Loaded && (dwFlags & VLOAD_NOVERTICES) == 0)
	{
		if (data->find_chunk(OGF_VCONTAINER))
		{
			R_ASSERT2(0, "pls notify andy about this.");
		}
		else
		{
			R_ASSERT(data->find_chunk(OGF_VERTICES));
			FVF = data->r_u32();

			CountVertex = data->r_u32();
		}
	}

	// indices
	if (!Loaded)
	{
		if (data->find_chunk(OGF_ICONTAINER))
		{
			R_ASSERT2(0, "pls notify andy about this.");
		}
		else
		{
			R_ASSERT(data->find_chunk(OGF_INDICES));
			CountIndex = data->r_u32();
		}

	}
}
#define PCOPY(a)	a = pFrom->a
void CDS0_FVisual::Copy(CDS0_RenderVisual* from)
{
	CDS0_RenderVisual::Copy(from);

	CDS0_FVisual* pFrom = dynamic_cast<CDS0_FVisual*> (from);

	PCOPY(FVF);
}