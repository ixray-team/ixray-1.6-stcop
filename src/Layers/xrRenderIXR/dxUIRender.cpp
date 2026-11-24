#include "stdafx.h"
#include "dxUIRender.h"

#include "dxUIShader.h"

dxUIRender UIRenderImpl;

void dxUIRender::CreateUIGeom()
{
	hGeom.create(EGeomSource::Ring);
}

void dxUIRender::DestroyUIGeom()
{
	hGeom = nullptr;
}

void dxUIRender::SetShader(IUIShader &shader)
{
	dxUIShader *pShader = (dxUIShader*) &shader;
	VERIFY(&pShader);
	VERIFY(pShader->hShader);
	Recorder.SetPass(pShader->hShader->E[0]._get());
}

void dxUIRender::SetAlphaRef(int aref)
{
	Recorder.SetAlphaRef(aref);
}

void dxUIRender::SetScissor(Irect* rect)
{
	if (rect) 
	{
		copy_scissor.set(*rect);
	}
	else 
	{
		copy_scissor.set(0, 0, Device.TargetWidth, Device.TargetHeight);
	}

	Recorder.SetScissor(rect);
}

void dxUIRender::GetActiveTextureResolution(Fvector2 &res)
{
	// A draw kept for the render thread has bound nothing yet: the pass's own texture then
	CTexture* T = DrawRecorder::IsRecording() ? Recorder.PassTexture() : RImplementation.Imm().GetTexture(0);

	if (!T)
	{
		res.set(1.f, 1.f);
		return;
	}

	res.set(float(std::max(T->get_Width(), 1u)), float(std::max(T->get_Height(), 1u)));
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
	DrawRecorder& Stream = Recorder;

	m_iMaxVerts = iMaxVerts;
	PrimitiveType = primType;
	m_PointType = pointType;

	switch(m_PointType)
	{
	case pttLIT:
		LIT_start_pv	= (FVF::LIT*)Stream.Lock(m_iMaxVerts,EVertexFormat::LIT,vOffset);
		LIT_pv			= LIT_start_pv;
		return (void**)&LIT_pv;
	case pttTL:
		TL_start_pv		= (FVF::TL*)Stream.Lock(m_iMaxVerts,EVertexFormat::TL,vOffset);
		TL_pv			= TL_start_pv;
		return (void**)&TL_pv;
	case pttL:
		L_start_pv		= (FVF::L*)Stream.Lock(m_iMaxVerts,EVertexFormat::L,vOffset);
		L_pv			= L_start_pv;
		return (void**)&L_pv;
	}

	return nullptr;
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
		break;
	case pttTL:
		p_cnt						= TL_pv-TL_start_pv;
		Recorder.SetDepth				(false);
		break;
	case pttL:
		p_cnt = L_pv - L_start_pv;
		break;
	default:
		NODEFAULT;
	}
	VERIFY(u32(p_cnt)<=m_iMaxVerts);
	Recorder.Unlock(u32(p_cnt));

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
		Recorder.Draw(hGeom._get(), d3dPrimType, RHITopologyUtils::GetIndexCount(primCount, d3dPrimType), vOffset);

	PrimitiveType	= ptNone;
	m_PointType		= pttNone;
}

void dxUIRender::CacheSetXformWorld(const Fmatrix& M)
{
	Recorder.SetWorld(M);
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
	Recorder.SetCull(m);
}

void dxUIRender::zb_enable(u32 val)
{
	Recorder.SetDepth(val != 0);
}

u32 dxUIRender::VBuffMaxSize()
{
	return RImplementation.Dynamic.GetCapacity() * sizeof(FVF::L);
}
