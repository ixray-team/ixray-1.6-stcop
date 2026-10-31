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
	Cmd.SetGeometry(FSTriangleGeom);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 3, 0, 0);

	if (LightingModeIsStaticRender())
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
	const CEnvDescriptorMixer& Desc = *Env.CurrentEnv;

	const float Pitch = Env.drop_max_angle * (Desc.rain_angle / Env.drop_max_wind_vel) - PI_DIV_2;
	Fvector Direction;
	Direction.setHP(Desc.rain_angle_rotation, Pitch);

	const float Speed = 0.5f * (Desc.rain_speed_min + Desc.rain_speed_max);

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetPass(RainLayers->E[0]);
	Cmd.SetGeometry(FSTriangleGeom);
	Cmd.Constants.Write<CBPost>().rain_layers_params.set(Desc.rain_density, SkyVisibility, Speed, g_RainRefraction);
	Cmd.Constants.Write<CBPost>().rain_layers_direction.set(Direction.x, Direction.y, Direction.z, 0.0f);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 3, 0, 0);
}

void CRenderTarget::PhaseRainStreaksEnd(GfxCmdList& Cmd)
{
	if (RImplementation.o.dx11_disable_motion_vectors || LightingModeIsStaticRender())
	{
		u_setrt(Cmd, rt_Generic_0, nullptr, RDepth);
	}
	else
	{
		u_setrt(Cmd, rt_Generic_0, rt_Velocity, RDepth);
	}
}
