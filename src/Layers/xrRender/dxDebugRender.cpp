#include "stdafx.h"

#ifdef DEBUG_DRAW

#include "dxDebugRender.h"
#include "dxUIShader.h"

#include "../xrRenderDX10/dx10BufferUtils.h"

dxDebugRender DebugRenderImpl;

dxDebugRender::dxDebugRender()
{
	m_lines.reserve			(line_vertex_limit);
	m_dbgVB = nullptr;
}

void dxDebugRender::Init()
{
	R_ASSERT(RHIUtils::CreateVertexBuffer(
		&m_dbgVB,
		nullptr,
		line_vertex_limit * 2,
		false));

	m_dbgGeom.create(FVF::F_L, m_dbgVB, nullptr);
	m_dbgShaders[dbgShaderWorld].create("debug_draw");
}

void dxDebugRender::Shutdown()
{
	m_dbgShaders[dbgShaderWorld].destroy();
	m_dbgGeom.destroy();

	m_dbgVB->Release();
	m_dbgVB = nullptr;
}

void dxDebugRender::Render()
{
	if (m_lines.empty())
		return;

	GPU_EVENT(DebugRender);
	size_t offset = 0;
	while (offset < m_lines.size())
	{
		size_t drawCount = std::min((size_t)line_vertex_limit / sizeof(std::pair<FVF::L, FVF::L>), m_lines.size() - offset);

		m_dbgVB->UpdateSubresource
		(
			m_lines.data() + offset,
			drawCount * sizeof(std::pair<FVF::L, FVF::L>)
		);

		RCache.set_xform_world(Fidentity);
		RCache.set_xform_view(Device.mView);
		RCache.set_xform_project(Device.mProject);
#ifndef _EDITOR
		RCache.set_RT(RImplementation.Target->rt_BackbufferLUT->pRT);
#endif
		RCache.set_Element(m_dbgShaders[dbgShaderWorld]->E[r_debug_render_depth * 4]);
		RCache.set_Geometry(m_dbgGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST, 0, drawCount * 2);

		offset += drawCount;
	}

	m_lines.clear();//resize(0);
}

void dxDebugRender::add_lines(Fvector const* vertices, u32 const& vertex_count, u32 const* pairs, u32 const& pair_count, u32 const& color)
{
	for (size_t i = 0; i < pair_count * 2; i += 2)
	{
		u32 i0 = pairs[i];
		u32 i1 = pairs[i + 1];
		FVF::L v0 = {};
		FVF::L v1 = {};
		v0.p = vertices[i0];
		v1.p = vertices[i1];
		v0.color = color;
		v1.color = color;

		m_lines.emplace_back(v0, v1);
	}
}

void dxDebugRender::NextSceneMode()
{
//	TODO: DX10: Check if need this for DX10
	VERIFY(!"Not implemented for DX10");
}

void dxDebugRender::ZEnable(bool bEnable)
{
	RCache.set_Z(bEnable);
}

void dxDebugRender::OnFrameEnd()
{
	RCache.OnFrameEnd();
}

void dxDebugRender::SetShader(const debug_shader &shader)
{
	RCache.set_Shader(((dxUIShader*)&*shader)->hShader);
}

void dxDebugRender::CacheSetXformWorld(const Fmatrix& M)
{
	RCache.set_xform_world(M);
}

void dxDebugRender::CacheSetCullMode(ERHI_CULLMODE m)
{
	GRHI->StateManager->SetCullMode(m);
}

void dxDebugRender::SetAmbient(u32 colour)
{
	//	TODO: DX10: Check if need this for DX10
	VERIFY(!"Not implemented for DX10");
}

void dxDebugRender::SetDebugShader(dbgShaderHandle shdHandle)
{
	R_ASSERT(shdHandle<dbgShaderCount);

	static const char* dbgShaderParams[][2] = 
	{
		{ "hud\\default" , "ui\\ui_pop_up_active_back" } , // dbgShaderWindow
		{ "debug_draw", nullptr } // dbgShaderWorld
	};

	if(!m_dbgShaders[shdHandle])
		m_dbgShaders[shdHandle].create(
			dbgShaderParams[shdHandle][0], dbgShaderParams[shdHandle][1]);
	
	RCache.set_Shader(m_dbgShaders[shdHandle]);
}

void dxDebugRender::DestroyDebugShader(dbgShaderHandle shdHandle)
{
	R_ASSERT(shdHandle<dbgShaderCount);

	m_dbgShaders[shdHandle].destroy();
}

void dxDebugRender::dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C)
{
	RCache.dbg_DrawTRI(T, p1, p2, p3, C);
}

#endif	//	DEBUG