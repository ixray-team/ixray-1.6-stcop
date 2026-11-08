#ifndef	UIRender_included
#define	UIRender_included
#pragma once
#include "UITransform.h"

namespace UIColors
{
inline u32 ScaleAlpha(u32 Color, float Factor)
{
	if (Factor >= 1.0f)
	{
		return Color;
	}
	const u32 Alpha = static_cast<u32>(((Color >> 24) & 255) * Factor + 0.5f);
	return (Color & 0x00ffffff) | (Alpha << 24);
}

inline u32 WithAlpha(u32 Color, float Value)
{
	const float Factor = std::isfinite(Value) ? std::clamp(Value, 0.0f, 1.0f) : 1.0f;
	return ScaleAlpha(Color | 0xff000000, Factor);
}
} // namespace UIColors

class IUIShader;

class IUIRender
{
public:
	UITransform2D Transform2D;
	float VertexOpacity = 1.0f;
	u32 ApplyOpacity(u32 Color) const
	{
		return UIColors::ScaleAlpha(Color, VertexOpacity);
	}

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
		pttL,
		pttTL,
		pttLIT
	};

	template<u32 verts_count = 1>
	struct r_vertL
	{
		struct
		{
			Fvector v; u32 clr;
		} vertices[verts_count];
	};

	template<u32 verts_count = 1>
	struct r_vertLIT
	{
		struct
		{
			Fvector v; u32 clr; Fvector2 uv;
		} vertices[verts_count];
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

	virtual void** StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType) { return nullptr; };

	virtual void FlushPrimitive() = 0;
	virtual bool BeginRoundedRect(const UIRoundedRect& Rect) { return false; }
	virtual void EndRoundedRect() {}

	virtual const char*	UpdateShaderName(const char* tex_name, const char* sh_name) = 0;

	virtual void CacheSetXformWorld(const Fmatrix& M) = 0;
	virtual void CacheSetProject(const Fmatrix& M){};
	virtual void CacheSetView(const Fmatrix& M){};

	virtual void CacheSetCullMode(ERHI_CULLMODE) = 0;

	virtual void zb_enable(u32 val) = 0;

	virtual u32 VBuffMaxSize() { return 0U; }
};

#endif	//	UIRender_included