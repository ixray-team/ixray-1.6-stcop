#pragma once

#include "../../Include/xrRender/UIRender.h"

class dxUIRender :
	public IUIRender
{
public:
	dxUIRender() : PrimitiveType(ptNone), m_PointType(pttNone) {;}

	virtual void CreateUIGeom() override;
	virtual void DestroyUIGeom() override;

	virtual void SetShader(IUIShader &shader) override;
	virtual void SetAlphaRef(int aref) override;
	virtual void SetScissor(Irect* rect= nullptr) override;

	virtual Irect GetScissor() const override { return copy_scissor; };

	virtual void GetActiveTextureResolution(Fvector2 &res) override;

	virtual void PushPoint(float x, float y, float z, u32 C, float u, float v) override;

	virtual void** StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType) override;

	virtual u32 VBuffMaxSize() override;

	virtual void FlushPrimitive() override;

	virtual const char*	UpdateShaderName(const char* tex_name, const char* sh_name) override;

	virtual void CacheSetXformWorld(const Fmatrix& M) override;
	virtual void CacheSetProject(const Fmatrix& M) override;
	virtual void CacheSetView(const Fmatrix& M) override;

	virtual void CacheSetCullMode(ERHI_CULLMODE) override;

	virtual void zb_enable(u32 val) override;

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