#pragma once
#include "XRayRenderVisual.h"

class	XRayTreeVisual : public XRayRenderVisual
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
//	BearFactoryPointer<BearRHI::BearRHIIndexBuffer> IndexBuffer;
//	BearFactoryPointer<BearRHI::BearRHIVertexBuffer> VertexBuffer;
	struct SUniformBuffer
	{
		Fmatrix	xform;
		Fmatrix	xform_v;
		Fvector4 c_scale, c_bias, wind, wave;
		Fvector4 consts;
		Fvector4 c_sun;
	};
//	BearFactoryPointer<BearRHI::BearRHIUniformBuffer> UniformBuffer;
public:

	//virtual void UpdateUniform(XRayUniformAllocator::EUniformType Type, void* ptr);
	virtual void Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void Copy(XRayRenderVisual* pFrom);

	XRayTreeVisual(void);
	virtual ~XRayTreeVisual(void);
};

class XRayTreeVisual_ST : public XRayTreeVisual
{
	typedef XRayTreeVisual inherited;
public:
	XRayTreeVisual_ST(void);
	virtual			~XRayTreeVisual_ST(void);
//	virtual bool Render(float LOD, EShaderElement SEType, XRayObjectRender& Item);
	virtual void	Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void	Copy(XRayRenderVisual* pFrom);
};

class XRayTreeVisual_PM : public XRayTreeVisual
{
	typedef XRayTreeVisual inherited;
private:
	FSlideWindowItem* pSWI;
	u32					last_lod;
public:
	XRayTreeVisual_PM(void);
	virtual			~XRayTreeVisual_PM(void);
	//virtual bool Render(float LOD, EShaderElement SEType, XRayObjectRender& Item);
	virtual void	Load(LPCSTR N, IReader* data, u32 dwFlags);
	virtual void	Copy(XRayRenderVisual* pFrom);
};
