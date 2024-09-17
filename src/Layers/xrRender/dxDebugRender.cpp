#include "stdafx.h"

#ifdef DEBUG_DRAW

#include "dxDebugRender.h"
#include "dxUIShader.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"
#endif // USE_DX11

dxDebugRender DebugRenderImpl;
dxDebugRender DebugRenderImpl_1;
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

#if 0
void _add_lines		(  xr_vector<FVF::L> &vertices, xr_vector<u32>& indices, Fvector const *pvertices, u32 const &vertex_count, u32 const *pairs, u32 const &pair_count, u32 const &color)
{
	u32								vertices_size = (u32)vertices.size();

	u32								indices_size = (u32)indices.size();
	indices.resize					(indices_size + 2*pair_count);
	xr_vector<u32>::iterator				I = indices.begin() + indices_size;
	xr_vector<u32>::iterator				E = indices.end();
	const u32*J = pairs;
	for ( ; I != E; ++I, ++J)
		*I							= vertices_size + *J;

	vertices.resize					(vertices_size + vertex_count);
	xr_vector<FVF::L>::iterator		i = vertices.begin() + vertices_size;
	xr_vector<FVF::L>::iterator		e = vertices.end();
	Fvector const					*j = pvertices;
	for ( ; i != e; ++i, ++j) {
		(*i).color					= color;
		auto Pos = *j;
		Device.mFullTransform_saved.transform(Pos);
		Pos.x = (Pos.x * 0.5f + 0.5f) * Device.TargetWidth;
		Pos.y = (-Pos.y * 0.5f + 0.5f) * Device.TargetHeight;
		(*i).p						= Pos;
	}
}
#endif

#if 0
void dxDebugRender::add_lines		(Fvector const *vertices, u32 const &vertex_count, u32 const *pairs, u32 const &pair_count, u32 const &color)
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

		Device.mView_saved.transform(v0.p);
		Device.mView_saved.transform(v1.p);

		if (v0.p.z > VIEWPORT_NEAR && v1.p.z <= VIEWPORT_NEAR) {
			Fvector fp = v1.p;
			fp.lerp(v0.p, v1.p, (v0.p.z - VIEWPORT_NEAR) / (v0.p.z - v1.p.z));
			v1.p = fp;
		}

		if (v0.p.z <= VIEWPORT_NEAR && v1.p.z > VIEWPORT_NEAR) {
			Fvector fp = v0.p;
			fp.lerp(v1.p, v0.p, (v1.p.z - VIEWPORT_NEAR) / (v1.p.z - v0.p.z));
			v0.p = fp;
		}

		if (v0.p.z <= VIEWPORT_NEAR && v1.p.z <= VIEWPORT_NEAR) {
			continue;
		}

		Device.mProject_saved.transform(v0.p);
		Device.mProject_saved.transform(v1.p);
		v0.p.x = (v0.p.x * 0.5f + 0.5f) * Device.TargetWidth;
		v0.p.y = (-v0.p.y * 0.5f + 0.5f) * Device.TargetHeight;

		v1.p.x = (v1.p.x * 0.5f + 0.5f) * Device.TargetWidth;
		v1.p.y = (-v1.p.y * 0.5f + 0.5f) * Device.TargetHeight;

		m_lines.emplace_back(v0, v1);
	}

}
#endif

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
		{"hud\\default" , "ui\\ui_pop_up_active_back"} ,// dbgShaderWindow
		{ "debug_draw", nullptr }
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


struct RDebugRender: 
	public dxDebugRender,
	public pureRender
{
private:
 xr_vector<std::pair<FVF::L, FVF::L>>		_lines;

//	Vertices		_line_vertices;
//	Indices			_line_indices;
public:
	RDebugRender()
	{
#ifndef _EDITOR
		Device.seqRender.Add		(this,REG_PRIORITY_LOW-100);
#endif
	}

virtual	~RDebugRender()
	{
#ifndef _EDITOR
		Device.seqRender.Remove		(this);
#endif
	}

void OnRender()
	{
		m_lines = _lines;
		Render();

	}
virtual void	add_lines			(Fvector const *vertices, u32 const &vertex_count, u32 const *pairs, u32 const &pair_count, u32 const &color)
{
	_lines.resize(0);
	for (size_t i = 0; i < pair_count * 2; i += 2) {
	u32 i0 = pairs[i];
		u32 i1 = pairs[i + 1];
		FVF::L v0 = {};
		FVF::L v1 = {};
		v0.p = vertices[i0];
		v1.p = vertices[i1];
		v0.color = color;
		v1.color = color;

		Device.mView_saved.transform(v0.p);
		Device.mView_saved.transform(v1.p);

		if (v0.p.z > VIEWPORT_NEAR && v1.p.z <= VIEWPORT_NEAR) {
			Fvector fp = v1.p;
			fp.lerp(v0.p, v1.p, (v0.p.z - VIEWPORT_NEAR) / (v0.p.z - v1.p.z));
			v1.p = fp;
		}

		if (v0.p.z <= VIEWPORT_NEAR && v1.p.z > VIEWPORT_NEAR) {
			Fvector fp = v0.p;
			fp.lerp(v1.p, v0.p, (v1.p.z - VIEWPORT_NEAR) / (v1.p.z - v0.p.z));
			v0.p = fp;
		}

		if (v0.p.z <= VIEWPORT_NEAR && v1.p.z <= VIEWPORT_NEAR) {
			continue;
		}

		Device.mProject_saved.transform(v0.p);
		Device.mProject_saved.transform(v1.p);
		v0.p.x = (v0.p.x * 0.5f + 0.5f) * Device.TargetWidth;
		v0.p.y = (-v0.p.y * 0.5f + 0.5f) * Device.TargetHeight;

		v1.p.x = (v1.p.x * 0.5f + 0.5f) * Device.TargetWidth;
		v1.p.y = (-v1.p.y * 0.5f + 0.5f) * Device.TargetHeight;

		_lines.emplace_back(v0, v1);
	}
}
} rdebug_render_impl;
dxDebugRender *rdebug_render = &rdebug_render_impl; 

#endif	//	DEBUG