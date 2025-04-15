#include "stdafx.h"
#ifdef DEBUG_DRAW
XRayDebugRender::XRayDebugRender()
{
}

void XRayDebugRender::Render()
{
}

void XRayDebugRender::NextSceneMode()
{
}

void XRayDebugRender::ZEnable(bool bEnable)
{
}

void XRayDebugRender::OnFrameEnd()
{
}

void XRayDebugRender::SetShader(const debug_shader& shader)
{
}

void XRayDebugRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void XRayDebugRender::CacheSetCullMode(CullMode mode)
{
}

void XRayDebugRender::SetAmbient(u32 colour)
{
}

void XRayDebugRender::SetDebugShader(dbgShaderHandle shdHandle)
{
}

void XRayDebugRender::DestroyDebugShader(dbgShaderHandle shdHandle)
{
}

void XRayDebugRender::dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C)
{
}

void XRayDebugRender::add_lines(const Fvector* vertices, const u32& vertex_count, const u32* pairs, const u32& pair_count, const u32& color)
{
}
#endif
