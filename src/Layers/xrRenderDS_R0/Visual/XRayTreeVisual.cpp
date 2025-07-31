

#include "stdafx.h"
#include "XRayTreeVisual.h"
#include "XRayRenderVisual.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
constexpr int FTreeVisual_tile = 16;
constexpr int FTreeVisual_quant = 32768 / FTreeVisual_tile;

void CDS0_TreeVisual::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	CDS0_RenderVisual::Load(N, data, dwFlags);
	R_ASSERT(data->find_chunk(OGF_GCONTAINER));
	{
		// verts
		u32 ID = data->r_u32();
		OffsetVertex = data->r_u32();
		CountVertex = data->r_u32();

		ID = data->r_u32();
		OffsetIndex = data->r_u32();
		CountIndex = data->r_u32();
	}

	// load tree-def
	R_ASSERT(data->find_chunk(OGF_TREEDEF2));
	{
		data->r(&xform, sizeof(xform));
		data->r(&c_scale, sizeof(c_scale));	c_scale.rgb.mul(.5f);	c_scale.hemi *= .5f;	c_scale.sun *= .5f;
		data->r(&c_bias, sizeof(c_bias));	c_bias.rgb.mul(.5f);	c_bias.hemi *= .5f;	c_bias.sun *= .5f;
	}

}
#define PCOPY(a)	a = pFrom->a
void CDS0_TreeVisual::Copy(CDS0_RenderVisual* from)
{
	CDS0_RenderVisual::Copy(from);

	CDS0_TreeVisual* pFrom = dynamic_cast<CDS0_TreeVisual*> (from);
	PCOPY(xform);
	PCOPY(c_scale);
	PCOPY(c_bias);
	PCOPY(FVF);

	PCOPY(OffsetVertex);
	PCOPY(CountVertex);

	PCOPY(OffsetIndex);
	PCOPY(CountIndex);
}


CDS0_TreeVisual::CDS0_TreeVisual(void)
{
}

CDS0_TreeVisual::~CDS0_TreeVisual(void)
{
}

CDS0_TreeVisual_ST::CDS0_TreeVisual_ST(void)
{
}

CDS0_TreeVisual_ST::~CDS0_TreeVisual_ST(void)
{
}
void CDS0_TreeVisual_ST::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	inherited::Load(N, data, dwFlags);

}

void CDS0_TreeVisual_ST::Copy(CDS0_RenderVisual* pFrom)
{
	inherited::Copy(pFrom);
}

CDS0_TreeVisual_PM::CDS0_TreeVisual_PM(void)
{
}

CDS0_TreeVisual_PM::~CDS0_TreeVisual_PM(void)
{
}

void CDS0_TreeVisual_PM::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	inherited::Load(N, data, dwFlags);
	R_ASSERT(data->find_chunk(OGF_SWICONTAINER));
	{
		u32 ID = data->r_u32();
	}
}

void CDS0_TreeVisual_PM::Copy(CDS0_RenderVisual* from)
{
	inherited::Copy(from);
	CDS0_TreeVisual_PM* pFrom = dynamic_cast<CDS0_TreeVisual_PM*> (from);
	PCOPY(pSWI);
}
