#pragma once
#include "XRayRenderVisual.h"

class	CDS0_TreeVisual : public CDS0_RenderVisual
{
private:
	struct	_5color
	{
		Fvector					rgb;		// - all static lighting
		float					hemi;		// - hemisphere
		float					sun;		// - sun
	};
protected:
	_5color						c_scale;
	_5color						c_bias;
	Fmatrix						xform;
public:
	size_t CountIndex;
	size_t OffsetIndex;
	size_t CountVertex;
	size_t OffsetVertex;
	u32 FVF;

	struct SUniformBuffer
	{
		Fmatrix	xform;
		Fmatrix	xform_v;
		Fvector4 c_scale, c_bias, wind, wave;
		Fvector4 consts;
		Fvector4 c_sun;
	};

public:
	virtual void Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* pFrom);

	CDS0_TreeVisual(void);
	virtual ~CDS0_TreeVisual(void);
};

class CDS0_TreeVisual_ST : public CDS0_TreeVisual
{
	typedef CDS0_TreeVisual inherited;
public:
	CDS0_TreeVisual_ST(void);
	virtual			~CDS0_TreeVisual_ST(void);
	virtual void	Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void	Copy(CDS0_RenderVisual* pFrom);
};

class CDS0_TreeVisual_PM : public CDS0_TreeVisual
{
	typedef CDS0_TreeVisual inherited;
private:
	FSlideWindowItem* pSWI;
	u32					last_lod;
public:
	CDS0_TreeVisual_PM(void);
	virtual			~CDS0_TreeVisual_PM(void);
	virtual void	Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void	Copy(CDS0_RenderVisual* pFrom);
};
