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
#include "stdafx.h"
#include "EngineAPI.h"
#include "../xrCore/Collision/xrCDB.h"
#include "../xrCore/IniCache.h"
#include <filesystem>

ENGINE_API bool g_RenderPresetLocked = false;

ENGINE_API RenderFeatureSwitch g_RenderFeatureSwitches[7] =
{
	{ "r_sun_shadows", &RenderFeatures::SunShadows },
	{ "r_lamp_shadows", &RenderFeatures::LampShadows },
	{ "r_volumetrics", &RenderFeatures::Volumetrics },
	{ "r_depth_of_field", &RenderFeatures::DepthOfField },
	{ "r_motion_vectors", &RenderFeatures::MotionVectors },
	{ "r_upscaling", &RenderFeatures::Upscaling },
	{ "r_sprite_glows", &RenderFeatures::SpriteGlows },
};

namespace
{
	RenderFeatures RenderPresetFeatures()
	{
		RenderFeatures Features;
		Features.LampShadows = Features.DepthOfField = Features.Volumetrics = Features.MotionVectors = Features.Upscaling = true;
		Features.SpriteGlows = true;
		return Features;
	}

	// The one place a combination the renderer cannot draw is corrected
	void RenderFeaturesResolve(RenderFeatures& Features)
	{
		// The forward level frame has no G-buffer; its velocity is the camera's, found from the depth. Its sun shadow is
		// over the lightmaps, which the renderer turns on by the shading quality, not by a switch. The depth of field
		// and the light the air scatters are its own: they read the frame's colour, depth and shadow maps
		Features.SunShadows = false;
		Features.Upscaling = Features.Upscaling && Features.MotionVectors;
	}
}

ENGINE_API RenderFeatures g_RenderFeatures = RenderPresetFeatures();

ENGINE_API void RenderPresetApply()
{
	g_RenderFeatures = RenderPresetFeatures();

	for (const RenderFeatureSwitch& Switch : g_RenderFeatureSwitches)
	{
		if (Switch.Value)
		{
			g_RenderFeatures.*Switch.Field = Switch.Value == 2;
		}
	}

	RenderFeaturesResolve(g_RenderFeatures);
	psDeviceFlags.set(rsR4, true);
}

ENGINE_API void RenderPresetLockActive()
{
	RenderPresetApply();
	g_RenderPresetLocked = true;

	for (const RenderFeatureSwitch& Switch : g_RenderFeatureSwitches)
	{
		if (Switch.Value)
		{
			Msg("* %s %s: %s", Switch.Command, Switch.Value == 2 ? "on" : "off", g_RenderFeatures.*Switch.Field ? "enabled" : "disabled");
		}
	}
}
