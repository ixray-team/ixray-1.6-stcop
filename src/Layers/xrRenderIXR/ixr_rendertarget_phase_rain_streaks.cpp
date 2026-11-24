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
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

void CRenderTarget::CreateRainStreakTargets(ERHI_FORMAT Format)
{
	IRHISurface* Depth = RDepth->GetSurface();
	const u32 Width = rt_Generic_0->dwWidth;
	const u32 Height = rt_Generic_0->dwHeight;

	RainRefraction.create(r2_RT_rain_refraction, std::max(Width / 4u, 1u), std::max(Height / 4u, 1u), Format);
	RainDepth.create(r2_RT_rain_depth, Depth->GetWidth(), Depth->GetHeight(), Depth->GetFormat());
	RainRefractionBlur.create("rain_refraction_blur");
	RainLayers.create("rain_layers");
}

void CRenderTarget::PhaseRainStreaksBegin(GfxCmdList& Cmd)
{
	GPU_EVENT(PhaseRainStreaks);

	Cmd.SetDepthStencil(nullptr);
	Cmd.CopySurface(RainDepth->pSurface, RDepth->GetSurface());

	u_setrt(Cmd, RainRefraction, nullptr, nullptr, nullptr);
	RImplementation.rmNormal();
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);

	Cmd.SetPass(RainRefractionBlur->E[0]);
	DrawFullscreen(Cmd, 3);

	if (!g_RenderFeatures.Upscaling)
	{
		u_setrt(Cmd, rt_Generic_0, nullptr, RDepth);
	}
	else if (RImplementation.o.dx11_disable_motion_vectors)
	{
		u_setrt(Cmd, rt_Generic_0, ReactiveMask, RDepth);
	}
	else
	{
		u_setrt(Cmd, rt_Generic_0, rt_Velocity, ReactiveMask, RDepth);
	}

	RImplementation.rmNormal();
}

void CRenderTarget::PhaseRainLayers(GfxCmdList& Cmd, float SkyVisibility)
{
	GPU_EVENT(PhaseRainLayers);

	const CEnvironment& Env = g_pGamePersistent->Environment();
	const CEnvDescriptorMixer& Desc = RFrame.Env();

	const float Pitch = Env.drop_max_angle * (Desc.rain_angle / Env.drop_max_wind_vel) - PI_DIV_2;
	Fvector Direction;
	Direction.setHP(Desc.rain_angle_rotation, Pitch);

	const float Speed = 0.5f * (Desc.rain_speed_min + Desc.rain_speed_max);

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetPass(RainLayers->E[0]);
	Cmd.Constants.Write<CBPost>().rain_layers_params.set(Desc.rain_density, SkyVisibility, Speed, g_RainRefraction);
	Cmd.Constants.Write<CBPost>().rain_layers_direction.set(Direction.x, Direction.y, Direction.z, 0.0f);
	DrawFullscreen(Cmd, 3);
}

void CRenderTarget::PhaseRainStreaksEnd(GfxCmdList& Cmd)
{
	if (RImplementation.o.dx11_disable_motion_vectors || !g_RenderFeatures.MotionVectors)
	{
		u_setrt(Cmd, rt_Generic_0, nullptr, RDepth);
	}
	else
	{
		u_setrt(Cmd, rt_Generic_0, rt_Velocity, RDepth);
	}
}
