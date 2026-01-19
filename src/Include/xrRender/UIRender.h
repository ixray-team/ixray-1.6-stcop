#ifndef	UIRender_included
#define	UIRender_included
#pragma once

class IUIShader;

class IUIRender
{
public:
	enum	ePrimitiveType
	{
		ptNone = -1,
		ptTriList,
//		ptTriFan,
		ptTriStrip,
		ptLineStrip,
		ptLineList
	};

	enum ePointType
	{
		pttNone = -1,
		pttTL,
		pttLIT
	};

	struct LITFast
	{
		Fvector		p;
		u32			color;
		Fvector2	t;
	};

public:
	virtual void CreateUIGeom() = 0;
	virtual void DestroyUIGeom() = 0;

	virtual void SetShader(IUIShader &shader) = 0;
	virtual void SetAlphaRef(int aref) = 0;
	virtual void SetScissor(Irect* rect=NULL) = 0;
	virtual Irect GetScissor() const = 0;
	virtual void GetActiveTextureResolution(Fvector2 &res) = 0;

	virtual void PushPoint(float x, float y, float z, u32 C, float u, float v) = 0;

	virtual void StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType) = 0;

	virtual LITFast** StartPrimitiveLITFast(u32 iMaxVerts, ePrimitiveType primType) { return nullptr; };

	virtual LITFast** GetLITFastBuffer() { return nullptr; };

	virtual void FlushPrimitive() = 0;

	virtual LPCSTR	UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name) = 0;

	virtual void	CacheSetXformWorld	(const Fmatrix& M) = 0;
	virtual void	CacheSetCullMode	(ERHI_CULLMODE) = 0;

	virtual void zb_enable(u32 val) = 0;
};

#endif	//	UIRender_included