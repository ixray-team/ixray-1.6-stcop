#pragma once
#include "FVF.h"
#include "../../Include/xrRender/DebugRender.h"

#ifdef DEBUG_DRAW
class dxDebugRender :
	public IDebugRender
{
public:
					dxDebugRender		();

			void	Init				();
			void	Shutdown			();

	virtual void	Render				();
	virtual void	add_lines			(Fvector const *vertices, u32 const &vertex_count, u32 const *pairs, u32 const &pair_count, u32 const &color);

	// routed to the immediate backend
	virtual void	NextSceneMode		();
	virtual void	ZEnable				(bool bEnable);
	virtual void	OnFrameEnd			();
	virtual void	SetShader			(const debug_shader &shader);
	virtual void	CacheSetXformWorld	(const Fmatrix& M);
	virtual void	CacheSetCullMode	(ERHI_CULLMODE);
	virtual void	SetAmbient			(u32 colour);

	// Shaders
	virtual void	SetDebugShader		(dbgShaderHandle shdHandle);
	virtual void	DestroyDebugShader	(dbgShaderHandle shdHandle);

	virtual void	dbg_DrawTRI			(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C);

			void	DrawOBB				(const Fmatrix& T, const Fvector& HalfSize, u32 C);
			void	DrawLine			(const Fmatrix& T, const Fvector& P1, const Fvector& P2, u32 C);
			void	DrawEllipse			(const Fmatrix& T, u32 C);

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
};

extern dxDebugRender DebugRenderImpl;

#endif // DEBUG
