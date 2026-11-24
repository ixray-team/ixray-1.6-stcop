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
#include "SH_Texture.h"
#include "SH_RT.h"
#include "SH_Atomic.h"

class GfxCmdList;
class IRHISurface;

// The sun's shadow over the level's heightfield, where the sun cascades end at high lighting quality
// (r4_heightfield_shadows). textures\heightfields\<level>.ltx and .dds are read the first frame that asks for them; every
// frame the grid is scanned along the sun into the shadow surface_sun_shadow reads
class HeightfieldShadowMap final
{
public:
	HeightfieldShadowMap() = default;
	HeightfieldShadowMap(const HeightfieldShadowMap&) = delete;
	HeightfieldShadowMap& operator=(const HeightfieldShadowMap&) = delete;
	~HeightfieldShadowMap();

	// Lets go of the level's heightfield, the scan's targets and shaders; the next level reads its own
	void Destroy();
	// The scan, and the heightfield's constants of CBLight: off unless it scans this frame
	void Render(GfxCmdList& Cmd);
	// What this frame's scan wrote, null without one
	IRHISurface* Shadow() const;

private:
	bool Load(const char* LevelName);

	ref_texture Heights;
	ref_rt Target;
	ref_rt Local;
	ref_rt Blocks;
	ref_rt Prefix;
	ref_cs Passes[3];
	Fvector4 WorldToGrid{};
	Fvector4 Decode{};
	u32 Width = 0;
	u32 Height = 0;
	u32 Frame = u32(-1);
	bool IsRead = false;
};
