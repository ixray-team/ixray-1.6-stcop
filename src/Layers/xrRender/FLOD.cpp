#include "stdafx.h"
#include "../../xrEngine/FmeshRender.h"
#include "FLOD.h"

struct _hw 
{
	Fvector		p0		;
	Fvector		p1		;
	Fvector		n0		;
	Fvector		n1		;
	u32			sun_af	;
	Fvector2	t0		;
	Fvector2	t1		;
	u32			rgbh0	;
	u32			rgbh1	;
};

static RHIInputElementDesc dwDecl[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "POSITION", 1, ERHI_FORMAT::R32G32B32_FLOAT, 0, 12, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },	
	{ "NORMAL", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 1, ERHI_FORMAT::R32G32B32_FLOAT, 0, 36, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "COLOR", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 48, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 52, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 1, ERHI_FORMAT::R32G32_FLOAT, 0, 60, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 2, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 68, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 3, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 72, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

void FLOD::Load			(const char* N, IReader *data, u32 dwFlags)
{
	inherited::Load		(N,data,dwFlags);

#ifndef MU_LODS_OFF_BILLBOARD
	// LOD-def
	bool FoundedChunk = !!data->find_chunk(OGF_LODDEF2);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_LODDEF2");

	for (int f=0; f<8; f++)
	{
		data->r					(facets[f].v,sizeof(facets[f].v));
		_vertex* v				= facets[f].v;

		Fvector					N_,T;
		N_.set					(0,0,0);
		T.mknormal				(v[0].v,v[1].v,v[2].v);	N_.add	(T);
		T.mknormal				(v[1].v,v[2].v,v[3].v);	N_.add	(T);
		T.mknormal				(v[2].v,v[3].v,v[0].v);	N_.add	(T);
		T.mknormal				(v[3].v,v[0].v,v[1].v);	N_.add	(T);
		N_.div					(4.f);
		facets[f].N.normalize	(N_);
		facets[f].N.invert		();
	}
#endif

	// VS
	geom.create(dwDecl, std::size(dwDecl), RCache.Vertex.Buffer(), RCache.QuadIB);

	// lod correction
	Fvector3			S;
	vis.box.getradius	(S);
	float r 			= vis.sphere.R;
	std::sort			(&S.x,&S.x+3);
	float a				= S.y;
	float Sf			= 4.f*(0.5f*(r*r*asin(a/r)+a*_sqrt(r*r-a*a)));
	float Ss			= M_PI*r*r;
	lod_factor			= Sf/Ss;
}
void FLOD::Copy			(dxRender_Visual *pFrom	)
{
	inherited::Copy		(pFrom);

	FLOD* F				= (FLOD*)pFrom;
	geom				= F->geom		;
	lod_factor			= F->lod_factor	;
#ifndef MU_LODS_OFF_BILLBOARD
	CopyMemory		(facets,F->facets,sizeof(facets));
#endif
}

void FMUMeshLODs::Load(const char* N, IReader* data, u32 dwFlags)
{
	FHierrarhyVisual::Load(N,data,dwFlags);
	std::ranges::sort(children, [&](dxRender_Visual* A, dxRender_Visual* B)
	{
		auto AC = (FMUMeshLOD*)A;
		auto BC = (FMUMeshLOD*)B;
		return AC->LODLevel < BC->LODLevel;
	});
}

void FLOD::Render		(float LOD)
{
}
