#pragma once

#include "../../Include/xrRender/UIRender.h"

class dxUIRender :
	public IUIRender
{
public:
	dxUIRender() : PrimitiveType(ptNone), m_PointType(pttNone) {;}

	virtual void CreateUIGeom();
	virtual void DestroyUIGeom();

	virtual void SetShader(IUIShader &shader);
	virtual void SetAlphaRef(int aref);
	virtual void SetScissor(Irect* rect=NULL);

	virtual Irect GetScissor() const { return copy_scissor; };

	virtual void GetActiveTextureResolution(Fvector2 &res);

	virtual void PushPoint(float x, float y, float z, u32 C, float u, float v);

	virtual void** StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType);

	virtual u32 VBuffMaxSize();

	virtual void FlushPrimitive();

	virtual const char*	UpdateShaderName(const char* tex_name, const char* sh_name);

	virtual void CacheSetXformWorld(const Fmatrix& M);
	virtual void CacheSetProject(const Fmatrix& M);
	virtual void CacheSetView(const Fmatrix& M);

	virtual void CacheSetCullMode(ERHI_CULLMODE);

	virtual void zb_enable(u32 val);

private:
	ref_geom		hGeom_L;
	ref_geom		hGeom_TL;
	ref_geom		hGeom_LIT;

	ePrimitiveType	PrimitiveType;
	ePointType		m_PointType;

	//	Vertex buffer attributes
	u32				m_iMaxVerts;
	u32				vOffset;

	FVF::L*			L_start_pv;
	FVF::L*			L_pv;

	FVF::TL*		TL_start_pv;
	FVF::TL*		TL_pv;

	FVF::LIT*		LIT_start_pv;
	FVF::LIT*		LIT_pv;

	Irect copy_scissor = {};
};

extern dxUIRender	UIRenderImpl;