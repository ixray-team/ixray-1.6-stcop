#include "pch.h"

using namespace Rendering;

void
DebugRender::Render()
{
}

void 
DebugRender::add_lines(Fvector const* vertices, u32 const& vertex_count, u16 const* pairs, u32 const& pair_count, u32 const& color)
{
}

void 
DebugRender::NextSceneMode()
{
}

void 
DebugRender::ZEnable(bool bEnable)
{
}

void
DebugRender::OnFrameEnd()
{
}

void 
DebugRender::SetShader(const debug_shader& shader)
{
}

void 
DebugRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void
DebugRender::CacheSetCullMode(CullMode)
{
}

void 
DebugRender::SetAmbient(u32 colour)
{
}

void 
DebugRender::SetDebugShader(dbgShaderHandle shdHandle)
{
}

void
DebugRender::DestroyDebugShader(dbgShaderHandle shdHandle)
{
}

void
DebugRender::dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C)
{
}
