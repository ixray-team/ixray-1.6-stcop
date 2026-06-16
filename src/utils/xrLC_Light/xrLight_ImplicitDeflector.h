#pragma once

#include "lm_layer.h"
#include "xrFace.h"

struct b_BuildTexture;

class ImplicitDeflector
{
public:
	b_BuildTexture*			texture;
	lm_layer				lmap;
	xr_vector<Face*>		faces;
	
	ImplicitDeflector() : texture(0)
	{
	}
	~ImplicitDeflector()
	{
		Deallocate	();
	}
	
	void			Allocate	()
	{
		lmap.create	(Width(), Height());
	}
	
	void			Deallocate	()
	{
		lmap.clear_memory();
		
		faces.clear(); 
		faces.shrink_to_fit();
	}
	
	u32			Width	()						;
	u32			Height	()						;	
	
	u32&		Texel	(u32 x, u32 y)			;
	base_color& Lumel	(u32 x, u32 y)			{ return lmap.surface[y*Width()+x];	}
	u8&			Marker	(u32 x, u32 y)			{ return lmap.marker [y*Width()+x];	}
	u8&			Samples	(u32 x, u32 y)			{ return lmap.samples[y*Width()+x]; }

	void		Bounds			(u32 ID, Fbox2& dest);
	void		Bounds_Summary	(Fbox2& bounds);
 
	void		SaveTextures();
};

#include "hash2D.h"
class ImplicitCalcGlobs
{
	hash2D<Face*, 256, 256> hash2dImpl;

	ImplicitDeflector* defl;

public:
	ImplicitCalcGlobs() : defl(0)
	{
	}
 	vecFace& query(float px, float py);

	IC ImplicitDeflector& DATA()
	{
		R_ASSERT(defl);
		return *defl;
	}
 	void Initialize(ImplicitDeflector& d);
};
