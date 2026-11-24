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

ENGINE_API extern bool g_RenderPresetLocked;

// What the renderer does, one switch per thing. Code asks for the feature it depends on; renderer_simplified is the one
// preset of these
struct RenderFeatures
{
	bool SunShadows = false;
	bool LampShadows = false;
	bool Volumetrics = false;
	bool DepthOfField = false;
	bool MotionVectors = false;
	bool Upscaling = false;
	bool SpriteGlows = false;
};

ENGINE_API extern RenderFeatures g_RenderFeatures;

// A console switch of one feature on top of the preset: 0 leaves it to the preset, 1 is off, 2 is on. Read when the
// renderer starts
struct RenderFeatureSwitch
{
	const char* Command;
	bool RenderFeatures::* Field;
	u32 Value = 0;
};

ENGINE_API extern RenderFeatureSwitch g_RenderFeatureSwitches[7];

ENGINE_API void RenderPresetApply();
ENGINE_API void RenderPresetLockActive();
