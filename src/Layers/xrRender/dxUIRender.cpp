#include "stdafx.h"
#include "dxUIRender.h"

#include "dxUIShader.h"

dxUIRender UIRenderImpl;

void dxUIRender::CreateUIGeom()
{
	hGeom_TL.create(FVF::F_TL, RCache.Vertex.Buffer(), 0);
	hGeom_LIT.create(FVF::F_LIT, RCache.Vertex.Buffer(), 0);
}

void dxUIRender::DestroyUIGeom()
{
	hGeom_TL = nullptr;
	hGeom_LIT = nullptr;
}

void dxUIRender::SetShader(IUIShader &shader)
{
	dxUIShader *pShader = (dxUIShader*) &shader;
	VERIFY(&pShader);
	VERIFY(pShader->hShader);
	RCache.set_Shader(pShader->hShader);
}

void dxUIRender::SetAlphaRef(int aref)
{
	RCache.set_AlphaRef(aref);
}

void dxUIRender::SetScissor(Irect* rect)
{
	RCache.set_Scissor(rect);
	GRHI->StateManager->OverrideScissoring(rect != nullptr, true);
}

void dxUIRender::GetActiveTextureResolution(Fvector2 &res)
{
	CTexture* T = RCache.get_ActiveTexture(0);
	res.set(float(T->get_Width()),float(T->get_Height()));
}

LPCSTR dxUIRender::UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name)
{
	string_path buff;
	if (FS.exist(buff, _game_textures_, tex_name, ".ogm"))
	{
		return "hud\\movie";
	}

	return sh_name;
}

void dxUIRender::PushPoint(float x, float y, float z, u32 C, float u, float v)
{
	switch(m_PointType)
	{
	case pttLIT:
		LIT_pv->set(x, y, z, C, u, v);
		++LIT_pv;
		break;
	case pttTL:
		TL_pv->set(x, y, C, u, v);
		++TL_pv;
		break;
	}
}

void dxUIRender::StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType)
{
	VERIFY(PrimitiveType == ptNone);
	VERIFY(m_PointType == pttNone);

	m_iMaxVerts = iMaxVerts;
	PrimitiveType = primType;
	m_PointType = pointType;

	switch(m_PointType)
	{
	case pttLIT:
		LIT_start_pv	= (FVF::LIT*)RCache.Vertex.Lock	(m_iMaxVerts,hGeom_LIT.stride(),vOffset);
		LIT_pv			= LIT_start_pv;
		break;
	case pttTL:
		TL_start_pv		= (FVF::TL*)RCache.Vertex.Lock	(m_iMaxVerts,hGeom_TL.stride(),vOffset);
		TL_pv			= TL_start_pv;
		break;
	}
}

void dxUIRender::FlushPrimitive()
{
	u32 primCount					= 0;
	ERHI_PRIMITIVE_TOPOLOGY d3dPrimType	= ERHI_PRIMITIVE_TOPOLOGY::UNDEFINED;
	std::ptrdiff_t p_cnt			= 0;

	switch(m_PointType)
	{
	case pttLIT:
		p_cnt						= LIT_pv-LIT_start_pv;
		VERIFY(u32(p_cnt)<=m_iMaxVerts);

		RCache.Vertex.Unlock		(u32(p_cnt),hGeom_LIT.stride());
		RCache.set_Geometry	 		(hGeom_LIT);
		break;
	case pttTL:
		p_cnt						= TL_pv-TL_start_pv;
		VERIFY(u32(p_cnt)<=m_iMaxVerts);

		RCache.Vertex.Unlock		(u32(p_cnt),hGeom_TL.stride());
		RCache.set_Geometry	 		(hGeom_TL);
		break;
	default:
		NODEFAULT;
	}

	//	Update data for primitive type
	switch(PrimitiveType)
	{
	case ptTriStrip:
		primCount = (u32)(p_cnt-2);
		d3dPrimType = ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP;
		break;
	case ptTriList:
		primCount = (u32)(p_cnt/3);
		d3dPrimType = ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST;
		break;
	case ptLineStrip:
		primCount = (u32)(p_cnt-1);
		d3dPrimType = ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP;
		break;
	case ptLineList:
		primCount = (u32)(p_cnt/2);
		d3dPrimType = ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST;
		break;
	default:
		NODEFAULT;
	}

	if (primCount>0) 
		RCache.Render(d3dPrimType,vOffset,primCount);

	PrimitiveType	= ptNone;
	m_PointType		= pttNone;
}

void dxUIRender::CacheSetXformWorld(const Fmatrix& M)
{
	RCache.set_xform_world(M);
}

void dxUIRender::CacheSetCullMode(ERHI_CULLMODE m)
{
	GRHI->StateManager->SetCullMode(m);
}

void dxUIRender::zb_enable(u32 val)
{
	RCache.set_Z(val);
}

