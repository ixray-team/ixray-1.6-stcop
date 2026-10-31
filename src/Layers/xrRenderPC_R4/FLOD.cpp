#include "stdafx.h"
#include "../../xrEngine/Fmesh.h"
#include "FLOD.h"
#include "PackedVertex.h"

FLOD::~FLOD()
{
	if (Facets.VertexCount)
	{
		RImplementation.Geometry.Free(Facets);
	}
}

void FLOD::UploadFacets()
{
	PackedVertex Vertices[32];
	for (u32 Face = 0; Face < 8; ++Face)
	{
		const u32 Normal = VertexPacking::EncodeOct(facets[Face].N);
		for (u32 Corner = 0; Corner < 4; ++Corner)
		{
			const _vertex& Source = facets[Face].v[Corner];
			PackedVertex& Target = Vertices[Face * 4 + Corner];
			Target = { Source.v, Normal, 0, 0, Source.c_rgb_hemi, Source.c_sun };
			VertexPacking::EncodeUV(Target, Source.t.x, Source.t.y);
		}
	}
	Facets = RImplementation.Geometry.Alloc(32, 0);
	RImplementation.Geometry.Upload(Facets, 0, Vertices, 32);
	geom.create(EGeomSource::Static, Facets.Page);
}

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

	UploadFacets();

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
	lod_factor			= F->lod_factor	;
#ifndef MU_LODS_OFF_BILLBOARD
	CopyMemory		(facets,F->facets,sizeof(facets));
#endif
	UploadFacets();
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

void FLOD::Render		(GfxCmdList& Cmd, float LOD)
{
}
