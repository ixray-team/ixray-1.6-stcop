#include "stdafx.h"
#include "dxUIRender.h"

#include "dxUIShader.h"

dxUIRender UIRenderImpl;

void dxUIRender::CreateUIGeom()
{
	GfxCmdList& Cmd = RImplementation.Imm();
	hGeom_L.create(FVF::F_L, Cmd.Vertex.Buffer(), nullptr);
	hGeom_TL.create(FVF::F_TL, Cmd.Vertex.Buffer(), nullptr);
	hGeom_LIT.create(FVF::F_LIT, Cmd.Vertex.Buffer(), nullptr);
}

void dxUIRender::DestroyUIGeom()
{
	hGeom_L = nullptr;
	hGeom_TL = nullptr;
	hGeom_LIT = nullptr;
}

void dxUIRender::SetShader(IUIShader &shader)
{
	dxUIShader *pShader = (dxUIShader*) &shader;
	VERIFY(&pShader);
	VERIFY(pShader->hShader);
	RImplementation.Imm().SetPass(pShader->hShader);
}

void dxUIRender::SetAlphaRef(int aref)
{
	RImplementation.Imm().SetAlphaRef(aref);
}

void dxUIRender::SetScissor(Irect* rect)
{
	GfxCmdList& Cmd = RImplementation.Imm();
	if (rect) 
	{
		copy_scissor.set(*rect);
	}
	else 
	{
		copy_scissor.set(0, 0, Device.TargetWidth, Device.TargetHeight);
	}

	Cmd.SetScissor(rect);
	Cmd.SetScissorOverride(rect != nullptr);
}

void dxUIRender::GetActiveTextureResolution(Fvector2 &res)
{
	CTexture* T = RImplementation.Imm().GetTexture(0);
	res.set(float(T->get_Width()),float(T->get_Height()));
}

const char* dxUIRender::UpdateShaderName(const char* tex_name, const char* sh_name)
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
	case pttL:
		L_pv->set(x, y, z, C);
		++L_pv;
		break;
	}
}

void** dxUIRender::StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType)
{
	VERIFY(PrimitiveType == ptNone);
	VERIFY(m_PointType == pttNone);
	GfxCmdList& Cmd = RImplementation.Imm();

	m_iMaxVerts = iMaxVerts;
	PrimitiveType = primType;
	m_PointType = pointType;

	switch(m_PointType)
	{
	case pttLIT:
		LIT_start_pv	= (FVF::LIT*)Cmd.Vertex.Lock(m_iMaxVerts,hGeom_LIT.stride(),vOffset);
		LIT_pv			= LIT_start_pv;
		return (void**)&LIT_pv;
	case pttTL:
		TL_start_pv		= (FVF::TL*)Cmd.Vertex.Lock(m_iMaxVerts,hGeom_TL.stride(),vOffset);
		TL_pv			= TL_start_pv;
		return (void**)&TL_pv;
	case pttL:
		L_start_pv		= (FVF::L*)Cmd.Vertex.Lock(m_iMaxVerts,hGeom_L.stride(),vOffset);
		L_pv			= L_start_pv;
		return (void**)&L_pv;
	}

	return nullptr;
}

void dxUIRender::FlushPrimitive()
{
	GfxCmdList& Cmd = RImplementation.Imm();
	u32 primCount					= 0;
	ERHI_PRIMITIVE_TOPOLOGY d3dPrimType	= ERHI_PRIMITIVE_TOPOLOGY::UNDEFINED;
	std::ptrdiff_t p_cnt			= 0;

	switch(m_PointType)
	{
	case pttLIT:
		p_cnt						= LIT_pv-LIT_start_pv;
		VERIFY(u32(p_cnt)<=m_iMaxVerts);

		Cmd.Vertex.Unlock		(u32(p_cnt),hGeom_LIT.stride());
		Cmd.SetGeometry	 		(hGeom_LIT);
		break;
	case pttTL:
		p_cnt						= TL_pv-TL_start_pv;
		VERIFY(u32(p_cnt)<=m_iMaxVerts);

		Cmd.Vertex.Unlock		(u32(p_cnt),hGeom_TL.stride());
		Cmd.SetGeometry	 		(hGeom_TL);
		Cmd.SetDepthEnable				(FALSE);
		break;
	case pttL:
		p_cnt = L_pv - L_start_pv;
		VERIFY(u32(p_cnt) <= m_iMaxVerts);

		Cmd.Vertex.Unlock(u32(p_cnt), hGeom_L.stride());
		Cmd.SetGeometry(hGeom_L);
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
		Cmd.Draw(d3dPrimType, RHITopologyUtils::GetIndexCount(primCount, d3dPrimType), vOffset);

	PrimitiveType	= ptNone;
	m_PointType		= pttNone;
}

void dxUIRender::CacheSetXformWorld(const Fmatrix& M)
{
	RImplementation.Imm().SetObjectTransform(M);
}

void dxUIRender::CacheSetProject(const Fmatrix& M)
{
	RImplementation.Imm().SetView(RImplementation.Imm().GetView().View, M);
}

void dxUIRender::CacheSetView(const Fmatrix& M)
{
	RImplementation.Imm().SetView(M, RImplementation.Imm().GetView().Project);
}

void dxUIRender::CacheSetCullMode(ERHI_CULLMODE m)
{
	RImplementation.Imm().SetCullMode(m);
}

void dxUIRender::zb_enable(u32 val)
{
	RImplementation.Imm().SetDepthEnable(val);
}

u32 dxUIRender::VBuffMaxSize()
{
	return RImplementation.Imm().Vertex.GetSize();
}
