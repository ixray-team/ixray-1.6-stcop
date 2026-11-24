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
#include "dxRenderDeviceRender.h"
#include "xrRender_console.h"

// In the forward level frame the shading quality is the whole choice: low is plain baked shading; medium adds bump,
// highlights and the sun cascade; high adds the sky mirrored in bumped surfaces and gives the occlusion pass the
// mapped normals of the surfaces. All but the cascade is taken when a material is compiled, so a change compiles
// the materials again. The level's compute shading reads the value every frame
void CRender::SyncShadingQuality()
{
	ApplyVideoSettings();
	g_RenderFeatures.SunShadows = ps_r_shading_quality >= 1 && ps_r_lighting_quality >= 1;
	g_ShadingQualityFrame = ps_r_shading_quality;
	g_OcclusionModeFrame = ps_r_ssao_mode;
	g_ForwardFlagsFrame = ps_r2_ls_flags_ext.get();
	g_MotionVectorsFrame = WantObjectVelocity();
	SyncForwardMsaa();

	// Everything a material or the final pass takes when it is compiled: one change compiles them once
	const u32 Passes = u32(ForwardOcclusion()) | u32(ForwardHudShadows()) << 1 | u32(ForwardReflections()) << 2 | u32(GpuScene::IsCoarseAlpha()) << 3 |
		u32(ForwardVelocityPass()) << 4 | u32(ForwardWboit()) << 5;
	const u32 Options = u32(o.GtaoHq) | u32(o.SslrHq) << 1 | u32(o.offscreen_reflecitons) << 2 | u32(o.ProceduralRainPuddles) << 3 |
		u32(g_SunShaftsAlternative) << 4;
	const u32 Key[] = { ps_r_shading_quality, ps_r_lighting_quality, ps_r_shadow_quality, ps_r_msaa, Passes, Options, g_RenderFeatures.Volumetrics ? ps_r_sun_shafts : 0,
		ps_r2_ls_flags.get() & (R2FLAG_SOFT_WATER | R2FLAG_SOFT_PARTICLES | R2FLAG_STEEP_PARALLAX | R2FLAG_DETAIL_BUMP),
		ps_r2_ls_flags_ext.get() & (R4FLAG_SSLR_ON_WATER | RFLAG_CLOUD_SHADOWS) };

	u64 Wanted = 14695981039346656037ull;

	for (const u32 Value : Key)
	{
		Wanted = (Wanted ^ Value) * 1099511628211ull;
	}

	if (ShaderKeyApplied == Wanted)
	{
		return;
	}

	const bool IsFirst = ShaderKeyApplied == 0;
	ShaderKeyApplied = Wanted;

	if (!IsFirst)
	{
		dxRenderDeviceRender::Instance().Resources->RecompileShaders();

		// The final pass is made by its blender, which the recompile does not reach: low shading quality decodes its
		// gamma scene in it
		Target->s_combine.create(Target->b_combine, "r2\\combine");
		Target->ResetExposure(Imm());
	}
}
