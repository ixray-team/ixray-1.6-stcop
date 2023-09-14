#include "pch.h"

Rendering::UIRender UIRenderInterface;

using namespace Rendering;

void UIRender::CreateUIGeom()
{	 
}	 
	 
void UIRender::DestroyUIGeom()
{	 
}	 
	 
void UIRender::SetShader(IUIShader& shader)
{	 
}	 
	 
void UIRender::SetAlphaRef(int aref)
{	 
}	 
	 
void UIRender::SetScissor(Irect* rect)
{	 
}	 
	 
void UIRender::GetActiveTextureResolution(Fvector2& res)
{	 
}	 
	 
void UIRender::PushPoint(float x, float y, float z, u32 C, float u, float v)
{	 
}	 
	 
void UIRender::StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType)
{	 
}	 
	 
void UIRender::FlushPrimitive()
{
}

LPCSTR UIRender::UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name)
{
	return LPCSTR();
}

void UIRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void UIRender::CacheSetCullMode(CullMode)
{
}
