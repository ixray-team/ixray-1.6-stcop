#pragma once
#ifdef DEBUG_DRAW
class CDS0_DebugRender :public IDebugRender
{
public:

public:
	CDS0_DebugRender();
	virtual void	Render();

	// routed to RCache
	virtual void	NextSceneMode();
	virtual void	ZEnable(bool bEnable);
	virtual void	OnFrameEnd();
	virtual void	SetShader(const debug_shader &shader);
	virtual void	CacheSetXformWorld(const Fmatrix& M);
	virtual void	CacheSetCullMode(CullMode mode);
	virtual void	SetAmbient(u32 colour);

	// Shaders
	virtual void	SetDebugShader(dbgShaderHandle shdHandle);
	virtual void	DestroyDebugShader(dbgShaderHandle shdHandle);
	virtual void	dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C);
	void add_lines(const Fvector* vertices, const u32& vertex_count, const u32* pairs, const u32& pair_count, const u32& color) override;
};
#endif