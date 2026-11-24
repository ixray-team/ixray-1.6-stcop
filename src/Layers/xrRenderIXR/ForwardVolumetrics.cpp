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

// The light the air scatters, of the sun and of the lamps that cast a shadow, over the finished forward frame. Both
// march the shadow maps the surfaces of the frame have read, against its depth
void CRenderTarget::ForwardVolumetrics(GfxCmdList& Cmd)
{
	m_bHasActiveVolumetric = false;

	if (!g_RenderFeatures.Volumetrics)
	{
		return;
	}

	// A lamp that shines all around casts its shadow as six parts, each a lamp of its own here
	const auto EachShaft = [](auto&& Visit)
	{
		const auto Try = [&](light* Part)
		{
			if (Part->flags.bVolumetric && Part->ShadowFrame == RFrame.dwFrame)
			{
				Visit(Part);
			}
		};

		for (light* Lamp : RImplementation.SurfaceLights)
		{
			if (Lamp->flags.type == IRender_Light::SPOT)
			{
				Try(Lamp);
			}
			else if (Lamp->flags.type == IRender_Light::POINT && Lamp->omnipart[0])
			{
				for (light* Part : Lamp->omnipart)
				{
					if (Part)
					{
						Try(Part);
					}
				}
			}
		}
	};

	u32 Lamps = 0;

	if (ps_r2_ls_flags.is(R2FLAG_VOLUMETRIC_LIGHTS))
	{
		EachShaft([&](light*) { ++Lamps; });
	}

	const bool HasSun = ps_r_sun_shafts > 0 && RImplementation.Lights.sun_adapted && RImplementation.m_sun_cascades.back().IsValid && need_to_render_sunshafts();

	if (!HasSun && Lamps == 0)
	{
		return;
	}

	GPU_EVENT(ForwardVolumetrics);
	copy_position();
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);

	if (HasSun)
	{
		accum_direct_volumetric(Cmd);
	}

	if (Lamps > 0)
	{
		EachShaft([&](light* Part) { accum_volumetric_lv(Cmd, Part); });
	}

	if (!m_bHasActiveVolumetric)
	{
		return;
	}

	u_setrt(Cmd, rt_Generic_0, nullptr, nullptr);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);
	phase_combine_volumetric(Cmd);
}
