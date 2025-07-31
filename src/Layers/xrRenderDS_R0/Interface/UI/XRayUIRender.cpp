#include "stdafx.h"
CDS0_UIShader::CDS0_UIShader()
{
}

void CDS0_UIShader::Copy(IUIShader& _in)
{

}

void CDS0_UIShader::create(LPCSTR sh, LPCSTR tex)
{
}

bool CDS0_UIShader::inited()
{
	return false;
}

void CDS0_UIShader::destroy()
{
}

CDS0_UIRender::CDS0_UIRender()
{
}

CDS0_UIRender::~CDS0_UIRender()
{
}

void CDS0_UIRender::CreateUIGeom()
{
}

void CDS0_UIRender::DestroyUIGeom()
{
	

}

void CDS0_UIRender::SetShader(IUIShader& shader)
{
}

void CDS0_UIRender::SetAlphaRef(int aref)
{
}

void CDS0_UIRender::SetScissor(Irect* rect)
{

	
}

void CDS0_UIRender::GetActiveTextureResolution(Fvector2& res)
{
	
}

void CDS0_UIRender::PushPoint(float x, float y, float z, u32 C, float u, float v)
{
	
}

void CDS0_UIRender::StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType)
{

	
	
}
void CDS0_UIRender::FlushPrimitive()
{
	

	
}

void CDS0_UIRender::Flush()
{
}

LPCSTR CDS0_UIRender::UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name)
{
	string_path buff;

	return  FS.exist(buff, "$game_textures$", tex_name, ".ogm") ? "hud\\movie" : sh_name;
}

void CDS0_UIRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void CDS0_UIRender::CacheSetCullMode(CullMode)
{
}
CDS0_UIRender GUIRender;