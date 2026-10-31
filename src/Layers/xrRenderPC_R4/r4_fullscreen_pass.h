#pragma once
#include "R_Backend.h"

template <class TBlender>
inline void CreateEffectSQ(ref_shader& Shader, const char* ShadernName = nullptr, const char* Textures = nullptr, const char* Constants = nullptr, const char* Matrices = nullptr)
{
	TBlender Blender;
	Shader.create(&Blender, ShadernName, Textures, Constants, Matrices);
}

template <class TBind>
inline void CRenderTarget::DrawPassSQ(GfxCmdList& Cmd, const ref_shader& Shader, u32 Element, TBind&& Bind)
{
	Cmd.SetPass(Shader->E[Element]);
	Bind();
	Cmd.DrawNoInput(3);
}

inline void CRenderTarget::DrawPassSQ(GfxCmdList& Cmd, const ref_shader& shader, u32 Element)
{
	DrawPassSQ(Cmd, shader, Element, [] {});
}

template <class TBind>
inline void CRenderTarget::DrawSQ(GfxCmdList& Cmd, const ref_shader& Shader, const ref_rt& Target, u32 Element, TBind&& Bind)
{
	u_setrt(Cmd, Target, nullptr, nullptr, nullptr);
	DrawPassSQ(Cmd, Shader, Element, Bind);
}

inline void CRenderTarget::DrawSQ(GfxCmdList& Cmd, const ref_shader& Shader, const ref_rt& Target, u32 Element)
{
	DrawSQ(Cmd, Shader, Target, Element, [] {});
}
