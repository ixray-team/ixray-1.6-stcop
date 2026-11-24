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

struct ImDrawData;
struct ImDrawList;
struct ImDrawCmd;
class CTexture;
class GfxCmdList;

// The main viewport's ImGui frame on the dynamic geometry stream, into the target bound on the command list. The platform
// windows are drawn by the old backend, which reads the same engine images
namespace ImGuiNri
{
	void Draw(GfxCmdList& Cmd, ref_shader& Pass, ImDrawData& Data);
	// Added to a draw list with a blend state of the old backend: the images after it are drawn with no blending, until
	// it is added again with none
	void BlendCallback(const ImDrawList* List, const ImDrawCmd* Command);
	// The engine image of a texture, once it has loaded
	void* Image(CTexture* Texture);
}
