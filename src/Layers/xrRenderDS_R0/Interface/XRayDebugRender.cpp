#include "stdafx.h"
#ifdef DEBUG_DRAW
CDS0_DebugRender::CDS0_DebugRender()
{
}

void CDS0_DebugRender::Render()
{
}

void CDS0_DebugRender::NextSceneMode()
{
}

void CDS0_DebugRender::ZEnable(bool bEnable)
{
}

void CDS0_DebugRender::OnFrameEnd()
{
}

void CDS0_DebugRender::SetShader(const debug_shader& shader)
{
}

void CDS0_DebugRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void CDS0_DebugRender::CacheSetCullMode(CullMode mode)
{
}

void CDS0_DebugRender::SetAmbient(u32 colour)
{
}

void CDS0_DebugRender::SetDebugShader(dbgShaderHandle shdHandle)
{
}

void CDS0_DebugRender::DestroyDebugShader(dbgShaderHandle shdHandle)
{
}

void CDS0_DebugRender::dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C)
{
}

void CDS0_DebugRender::add_lines(const Fvector* vertices, const u32& vertex_count, const u32* pairs, const u32& pair_count, const u32& color)
{
}
#endif
