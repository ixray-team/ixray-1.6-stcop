
#include "XRayFLOD.h"

#include "XRayRenderVisual.h"

XRayFLOD::XRayFLOD()
{
}

XRayFLOD::~XRayFLOD()
{
}

void XRayFLOD::Render(float LOD)
{
}

void XRayFLOD::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	XRayFHierrarhyVisual::Load(N, data, dwFlags);

	// LOD-def
	R_ASSERT(data->find_chunk(OGF_LODDEF2));
	for (u32 f = 0; f < 8; f++)
	{
		data->r(facets[f].v, sizeof(facets[f].v));
		_vertex* v = facets[f].v;

		Fvector					FN, T;
		FN.set(0, 0, 0);
		T.mknormal(v[0].v, v[1].v, v[2].v);	FN.add(T);
		T.mknormal(v[1].v, v[2].v, v[3].v);	FN.add(T);
		T.mknormal(v[2].v, v[3].v, v[0].v);	FN.add(T);
		T.mknormal(v[3].v, v[0].v, v[1].v);	FN.add(T);
		FN.div(4.f);
		facets[f].N.normalize(FN);
		facets[f].N.invert();
	}
	Fvector3			S;
	Vis.box.getradius(S);
	float r = Vis.sphere.R;
	std::sort(&S.x, &S.x + 3);
	float a = S.y;
	float Sf = 4.f * (0.5f * (r * r * asin(a / r) + a * _sqrt(r * r - a * a)));
	float Ss = M_PI * r * r;
	lod_factor = Sf / Ss;
}

void XRayFLOD::Copy(XRayRenderVisual* pFrom)
{
	XRayFHierrarhyVisual::Copy(pFrom);

	XRayFLOD* F = (XRayFLOD*)pFrom;
	lod_factor = F->lod_factor;
	std::memcpy(facets, F->facets, sizeof(facets));
}
