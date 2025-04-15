#pragma once

#ifdef DEBUG_DRAW
#include "FVF.h"
#include "../../Include/xrRender/DebugRender.h"

class dxDebugRender : public IDebugRender
{
public:
					dxDebugRender		();

			void	Init				();
			void	Shutdown			();

	virtual void	Render				();
	virtual void	add_lines			(Fvector const *vertices, u32 const &vertex_count, u32 const *pairs, u32 const &pair_count, u32 const &color);

	// routed to RCache
	virtual void	NextSceneMode		();
	virtual void	ZEnable				(bool bEnable);
	virtual void	OnFrameEnd			();
	virtual void	SetShader			(const debug_shader &shader);
	virtual void	CacheSetXformWorld	(const Fmatrix& M);
	virtual void	CacheSetCullMode	(CullMode);
	virtual void	SetAmbient			(u32 colour);

	// Shaders
	virtual void	SetDebugShader		(dbgShaderHandle shdHandle);
	virtual void	DestroyDebugShader	(dbgShaderHandle shdHandle);

#ifdef DEBUG_DRAW
	virtual void	dbg_DrawTRI			(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C);
#endif	//	DEBUG

private:
	enum {
		line_vertex_limit				= 32767 * 256,
		line_index_limit				= 32767 * 256
	};

public:
	xr_vector<std::pair<FVF::L, FVF::L>> m_lines;

private:
	ref_shader		m_dbgShaders[dbgShaderCount];
	ref_geom		m_dbgGeom;
	ID3DVertexBuffer* m_dbgVB;
};

extern dxDebugRender DebugRenderImpl;

#endif // DEBUG
