#pragma once

#ifdef DEBUG_DRAW
class CDS0_DebugRender :
	public IDebugRender
{
public:

public:
	CDS0_DebugRender();
	virtual void Render() override;

	// routed to RCache
	virtual void NextSceneMode() override;
	virtual void ZEnable(bool bEnable) override;
	virtual void OnFrameEnd() override;
	virtual void SetShader(const debug_shader& shader) override;
	virtual void CacheSetXformWorld(const Fmatrix& M) override;
	virtual void CacheSetCullMode(ERHI_CULLMODE mode) override;
	virtual void SetAmbient(u32 colour) override;

	// Shaders
	virtual void SetDebugShader(dbgShaderHandle shdHandle) override;
	virtual void DestroyDebugShader(dbgShaderHandle shdHandle) override;
#ifdef DEBUG
	virtual void dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C) override;
#endif
	void add_lines(const Fvector* vertices, const u32& vertex_count, const u32* pairs, const u32& pair_count, const u32& color) override;
};
#endif