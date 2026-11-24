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
class IRHISurface;
class IRHIShaderResourceView;

// What draws ImGui through the old backend: the editors, and the game's platform windows. The game's main viewport is
// drawn through NRI by the renderer
namespace RHIUtils::ImGui
{
	// EngineTextures: ImGui's own textures are engine images made by UpdateTextures, which the renderer calls
	RHI_API void Init(bool EngineTextures = false);
	RHI_API void NewFrame();
	RHI_API void DrawData();
	RHI_API void Destroy();
	RHI_API void Reset();
	RHI_API void* GetBlenderState();
	RHI_API void UpdateTextures();
	// The old backend's view of an engine image while it is drawn; none of an array, a cube or a multisampled surface
	RHI_API IRHIShaderResourceView* ImageView(IRHISurface* Surface);

	// An engine image: the ImTextureID of a surface, told from the native views of the editors by its low bit
	inline void* Image(IRHISurface* Surface)
	{
		return Surface ? reinterpret_cast<void*>(reinterpret_cast<uintptr_t>(Surface) | 1) : nullptr;
	}

	inline IRHISurface* ImageSurface(void* Id)
	{
		const uintptr_t Value = reinterpret_cast<uintptr_t>(Id);
		return Value & 1 ? reinterpret_cast<IRHISurface*>(Value & ~uintptr_t(1)) : nullptr;
	}
}
