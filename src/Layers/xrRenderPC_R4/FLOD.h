#pragma once
#include "FHierrarhyVisual.h"

class FLOD: public FHierrarhyVisual
{
	typedef FHierrarhyVisual inherited;
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
		_vertex		v[4]	;
		Fvector		N		;
	};

	ref_geom		geom		;
	GeometryAlloc	Facets		= {};
	_face			facets		[8];
	float			lod_factor	;
	bool			next_lod_checked = false;
    bool			has_next_lod = false;
public:
	~FLOD() override;
	virtual void Render			(GfxCmdList& Cmd, float LOD		);									// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored
	virtual void Load			(const char* N, IReader *data, u32 dwFlags);
	virtual void Copy			(dxRender_Visual *pFrom	);
private:
	void UploadFacets();
};

class FMUMeshLODs : public FHierrarhyVisual
{
	virtual void Load(const char* N, IReader *data, u32 dwFlags) override;
};

class FMUMeshLOD : public FHierrarhyVisual
{
public:
	u8 LODLevel = -1;
};
