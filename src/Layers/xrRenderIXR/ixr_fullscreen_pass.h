/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once
#include "R_Backend.h"

// The pass set on Cmd over the screen, recorded through NRI
void DrawFullscreen(GfxCmdList& Cmd, u32 VertexCount);

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
	DrawFullscreen(Cmd, 3);
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
