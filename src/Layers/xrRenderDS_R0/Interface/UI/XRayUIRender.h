#pragma once

class CDS0_UIShader :
	public IUIShader
{
public:
	CDS0_UIShader();
	virtual void Copy(IUIShader& _in) override;
	virtual void create(str_c sh, str_c tex = nullptr) override;
	virtual bool inited() override;
	virtual void destroy() override;
};

class CDS0_UIRender:
	public IUIRender
{
public:
	CDS0_UIRender();
	~CDS0_UIRender();
	virtual void CreateUIGeom() override;
	virtual void DestroyUIGeom() override;

	virtual void SetShader(IUIShader &shader) override;
	virtual void SetAlphaRef(int aref) override;
	virtual void SetScissor(Irect* rect = nullptr) override;
	virtual void GetActiveTextureResolution(Fvector2 &res) override;

	virtual void PushPoint(float x, float y, float z, u32 C, float u, float v) override;

	virtual void** StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType) override;
	virtual void FlushPrimitive() override;
	virtual void Flush();
	virtual str_c UpdateShaderName(str_c tex_name, str_c sh_name) override;

	virtual void CacheSetXformWorld(const Fmatrix& M) override;
	virtual void CacheSetCullMode(ERHI_CULLMODE) override;

	virtual void zb_enable(u32 val) override {};

	virtual Irect GetScissor() const override { return Irect(); };
private:
};

extern CDS0_UIRender GUIRender;