#pragma once

#include "TiramisuRenderTypes.h"
#include "Resources/RenderVertexTypes.h"

class TiramisuRenderTextureResourceProxy;
class TiramisuRenderTexture;

class CDS0_UIShader :
	public IUIShader
{
public:
	CDS0_UIShader();
	~CDS0_UIShader();
	virtual void Copy(IUIShader& _in);
	virtual void create(LPCSTR sh, LPCSTR tex = 0);
	virtual bool inited();
	virtual void destroy();

	TiramisuRenderTexture* Texture = nullptr;
	xr_string ShaderName;
	xr_string TextureName;
};

void DumpLiveTiramisuUiShaders();

// Legacy UI primitive, временно переводимый в UI pass Tiramisu.
struct FXRayUIPrimitive
{
	u32 VertexOffset = 0;
	u32 VertexCount = 0;
	IUIRender::ePrimitiveType PrimitiveType;
	IUIRender::ePointType PointType;
	xr_vector<FUIVertex> VertexesCache;
	TiramisuRenderTexture* Texture = nullptr;
	TiramisuRenderTextureResourceProxy* TextureResourceProxy = nullptr;
	Irect ScissorRect;
};

class CDS0_UIRender :
	public IUIRender
{
public:
	CDS0_UIRender();
	~CDS0_UIRender();
	virtual void CreateUIGeom();
	virtual void DestroyUIGeom();

	virtual void SetShader(IUIShader& shader);
	virtual void SetAlphaRef(int aref);
	virtual void SetScissor(Irect* rect = NULL);
	virtual void GetActiveTextureResolution(Fvector2& res);

	virtual void PushPoint(float x, float y, float z, u32 C, float u, float v);

	virtual void** StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType);
	virtual void FlushPrimitive();
	virtual void Flush();
	virtual LPCSTR UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name);

	virtual void CacheSetXformWorld(const Fmatrix& M);
	virtual void CacheSetCullMode(ERHI_CULLMODE);

	virtual void zb_enable(u32 val) {};

	virtual Irect GetScissor() const { return CurrentScissor; };

	xr_vector<FUIVertex> Vertexes;
	xr_vector<FXRayUIPrimitive> Primitivs;
	IUIShader* CurrentShader = nullptr;
	Irect CurrentScissor;

private:
};

extern CDS0_UIRender GUIRender;
