#pragma once
#include "R_Backend.h"

// Records the pass set on Cmd through NRI; false when it needs something that path lacks
bool DrawFullscreenNri(GfxCmdList& Cmd, u32 VertexCount);

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
	if (!DrawFullscreenNri(Cmd, 3))
	{
		Cmd.DrawNoInput(3);
	}
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
