#include "stdafx.h"

#ifdef DEBUG_DRAW

#include "dxDebugRender.h"
#include "dxUIShader.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"
#endif // USE_DX11

dxDebugRender DebugRenderImpl;

dxDebugRender::dxDebugRender()
{
	m_lines.reserve			(line_vertex_limit);
	m_dbgVB = nullptr;
}

void dxDebugRender::Init()
{
#ifdef USE_DX11
	R_CHK(dx10BufferUtils::CreateVertexBuffer(
		&m_dbgVB,
		nullptr,
		line_vertex_limit,
		false));

	m_dbgGeom.create(FVF::F_L, m_dbgVB, nullptr);
	m_dbgShaders[dbgShaderWorld].create("debug_draw");
#endif
}

void dxDebugRender::Shutdown()
{
#ifdef USE_DX11
	m_dbgShaders[dbgShaderWorld].destroy();
	m_dbgGeom.destroy();

	m_dbgVB->Release();
	m_dbgVB = nullptr;
#endif // USE_DX11
}

void dxDebugRender::Render()
{
	if (m_lines.empty())
		return;

#ifdef USE_DX11
	size_t offset = 0;
	while (offset < m_lines.size())
	{
		size_t drawCount = std::min((size_t)line_vertex_limit / sizeof(std::pair<FVF::L, FVF::L>), m_lines.size() - offset);

		RContext->UpdateSubresource(
			m_dbgVB,
			0,
			nullptr,
			m_lines.data() + offset,
			drawCount * sizeof(std::pair<FVF::L, FVF::L>),
			0);

		RCache.set_xform_world(Fidentity);
		RCache.set_Element(m_dbgShaders[dbgShaderWorld]->E[r_debug_render_depth * 4]);
		RCache.set_Geometry(m_dbgGeom);
		RCache.Render(D3DPT_LINELIST, 0, drawCount / 2);

		offset += drawCount;
	}
#else
	RCache.set_xform_world(Fidentity);
	RCache.OnFrameEnd();
	RCache.set_Z(r_debug_render_depth);
	CHK_DX(RDevice->SetFVF(FVF::F_L));
	CHK_DX(RDevice->DrawPrimitiveUP(D3DPT_LINELIST, m_lines.size() / 2, m_lines.data(), sizeof(FVF::L)));
#endif // USE_DX11

	m_lines.clear();//resize(0);
}

void dxDebugRender::add_lines(Fvector const* vertices, u32 const& vertex_count, u32 const* pairs, u32 const& pair_count, u32 const& color)
{
	for (size_t i = 0; i < pair_count * 2; i += 2) {
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
#ifdef USE_DX11
//	TODO: DX10: Check if need this for DX10
	VERIFY(!"Not implemented for DX10");
#else //USE_DX11
	Caps.SceneMode = (Caps.SceneMode + 1) % 3;
#endif
}

void dxDebugRender::ZEnable(bool bEnable)
{
	//CHK_DX(RDevice->SetRenderState(D3DRS_ZENABLE,bEnable));
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

void dxDebugRender::CacheSetCullMode(CullMode m)
{
	RCache.set_CullMode	(CULL_NONE+m);
}

void dxDebugRender::SetAmbient(u32 colour)
{
#ifdef USE_DX11
	//	TODO: DX10: Check if need this for DX10
	VERIFY(!"Not implemented for DX10");
#else //USE_DX11
	CHK_DX(RDevice->SetRenderState (D3DRS_AMBIENT, colour));
#endif
}

void dxDebugRender::SetDebugShader(dbgShaderHandle shdHandle)
{
	R_ASSERT(shdHandle<dbgShaderCount);

	static const LPCSTR dbgShaderParams[][2] = 
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
