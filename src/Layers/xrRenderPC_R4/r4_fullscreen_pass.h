#pragma once
#include "R_Backend.h"

template <class TBlender>
inline void CreateEffectSQ(ref_shader& Shader, const char* ShadernName = nullptr, const char* Textures = nullptr, const char* Constants = nullptr, const char* Matrices = nullptr)
{
	TBlender Blender;
	Shader.create(&Blender, ShadernName, Textures, Constants, Matrices);
}

template <class TBind>
inline void CRenderTarget::DrawPassSQ(const ref_shader& Shader, u32 Element, TBind&& Bind)
{
	RCache.set_Element(Shader->E[Element]);
	Bind();
	RCache.set_Geometry(FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
}

inline void CRenderTarget::DrawPassSQ(const ref_shader& shader, u32 Element)
{
	DrawPassSQ(shader, Element, [] {});
}

template <class TBind>
inline void CRenderTarget::DrawSQ(const ref_shader& Shader, const ref_rt& Target, u32 Element, TBind&& Bind)
{
	u_setrt(Target, nullptr, nullptr, nullptr);
	DrawPassSQ(Shader, Element, static_cast<Bind&&>(Bind));
}

inline void CRenderTarget::DrawSQ(const ref_shader& Shader, const ref_rt& Target, u32 Element)
{
	DrawSQ(Shader, Target, Element, [] {});
}
