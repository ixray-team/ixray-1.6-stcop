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
#include "ixr.h"
#include "../../xrEngine/EngineAPI.h"

ShaderExternalMap CRender::GetShaderDefines(const ShaderExternalMap& Defines) const
{
	string16 SmapSize, SunShafts, ShadowQuality;
	xr_sprintf(ShadowQuality, "%u", ps_r_shadow_quality);
	xr_sprintf(SmapSize, "%04u", u32(o.smapsize));
	xr_sprintf(SunShafts, "%d", ps_r_sun_shafts);

	const struct
	{
		bool Enabled;
		const char* Name;
		const char* Value;
	} Flags[] =
	{
		{ true, "SMAP_size", SmapSize },
		{ ps_r2_ls_flags.test(R2FLAG_SOFT_WATER), "USE_SOFT_WATER", "1" },
		{ !!o.offscreen_reflecitons, "USE_OFFSCREEN_REFLECTIONS", "1" },
		// r_offscreen_reflection_quality medium and up: the mirrors march the cube; low looks it up along the ray
		{ !!o.offscreen_reflecitons && ps_r_offscreen_reflection_quality >= 2, "USE_OFFSCREEN_MARCH", "1" },
		{ !!o.ProceduralRainPuddles, "USE_RAIN_PUDDLES", "1" },
		{ Target && Target->SnowInMaterial, "USE_DYNAMIC_SNOW", "1" },
		{ Target && Target->SnowInMaterial && Target->SnowIsPbr, "SNOW_PBR", "1" },
		{ !!o.GtaoHq, "USE_GTAO_HQ", "1" },
		{ !!o.SslrHq, "USE_SSLR_HQ", "1" },
		{ g_RenderFeatures.DepthOfField, "USE_DOF", "1" },
		{ ps_r2_ls_flags_ext.test(R4FLAG_SSLR_ON_WATER), "USE_SSLR_ON_WATER", "1" },
		{ g_RenderFeatures.Volumetrics && ps_r_sun_shafts > 0, "SUN_SHAFTS_QUALITY", SunShafts },
		{ true, "Volumetric_Alternative", g_SunShaftsAlternative ? "1" : "0" },
		{ ps_r2_ls_flags.test(R2FLAG_STEEP_PARALLAX), "ALLOW_STEEPPARALLAX", "1" },
		{ !!o.dx11_disable_motion_vectors, "DISABLE_MOTION_VECTORS", "1" },
		{ Device.IsEditorMode(), "EDITOR_MODE", "1" },
		{ IsEditorInstancing(), "USE_EDITOR_INSTANCING", "1" },
		{ !o.sunstatic, "USE_SURFACE_SUN_SHADOWS", "1" },
		{ true, "USE_SURFACE_LIGHTS", "1" },
		{ ps_r2_ls_flags_ext.test(RFLAG_CLOUD_SHADOWS), "USE_SURFACE_SUNMASK", "1" },
		{ true, "SURFACE_SHADOW_QUALITY", ShadowQuality },
		{ ForwardDynamicLight(), "USE_SURFACE_DYNAMIC", "1" },
		{ ps_r_lighting_quality < 2, "SURFACE_SINGLE_CASCADE", "1" },
		{ true, "USE_SURFACE_LAMP_SHADOWS", "1" },
		{ true, "USE_SURFACE_DECALS", "1" },
		{ ps_r_shading_quality == 0, "USE_SURFACE_CLASSIC", "1" },
		{ ps_r_shading_quality >= 1, "USE_LINEAR_SCENE", "1" },
		{ ps_r2_ls_flags.test(R2FLAG_SOFT_PARTICLES), "USE_SURFACE_SOFT_PARTICLES", "1" },
		{ ps_r_shading_quality >= 2, "USE_SURFACE_HIGH", "1" },
		{ GpuScene::IsCoarseAlpha(), "USE_COARSE_ALPHA", "1" },
		{ ForwardOcclusion(), "USE_SURFACE_OCCLUSION", "1" },
		{ ForwardOcclusionNormals(), "USE_SURFACE_OCCLUSION_NORMALS", "1" },
		{ ForwardOcclusionNormals() && ForwardOcclusionHistory(), "USE_GTAO_SURFACE_NORMALS", "1" },
		{ ForwardOcclusionHistory(), "USE_GTAO_HISTORY", "1" },
		{ ForwardHudShadows(), "USE_SURFACE_HUD_SHADOWS", "1" },
		{ ForwardReflections(), "USE_SURFACE_REFLECTIONS", "1" },
		{ ForwardVelocityPass(), "USE_SURFACE_VELOCITY", "1" },
#if defined(IXR_APPLE_SERIES)
		// vkd3d HLSL lacks some intrinsics (e.g. RWTexture2D::GetDimensions); shaders branch on this.
		{ true, "IXR_VKD3D_COMPILER", "1" },
#endif
	};

	ShaderExternalMap Result = EngineExternal().ShadersOptions;
	Result.erase(xr_string("ALLOW_WBOIT_TRANSPARENCY"));
	Result.erase(xr_string("USE_WBOIT_TRANSPARENCY"));

	for (const auto& [Name, Value] : Defines)
	{
		Result[Name] = Value;
	}
	for (const auto& Flag : Flags)
	{
		if (Flag.Enabled)
		{
			Result[Flag.Name] = Flag.Value;
		}
	}
	return Result;
}
