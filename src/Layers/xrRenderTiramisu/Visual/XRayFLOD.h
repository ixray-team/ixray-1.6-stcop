#pragma once
#include "XRayFHierrarhyVisual.h"

class CDS0_FLOD :
	public CDS0_FHierrarhyVisual
{
public:
	struct _vertex
	{
		Fvector		v;
		Fvector2	t;
		u32			c_rgb_hemi;	// rgb,hemi
		u8			c_sun;
	};
	struct _face
	{
		_vertex		v[4];
		Fvector		N;
	};
	struct _hw
	{
		Fvector		p0;
		Fvector		p1;
		Fvector		n0;
		Fvector		n1;
		u32			sun_af;
		Fvector2	t0;
		Fvector2	t1;
		u32			rgbh0;
		u32			rgbh1;
	};

	_face			facets[8];
	float			lod_factor;

public:
	CDS0_FLOD();
	virtual ~CDS0_FLOD();
	virtual void Render(float LOD);									// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored
	virtual void Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* pFrom);
};