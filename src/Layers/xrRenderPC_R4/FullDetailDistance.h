#pragma once

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "xrRender_console.h"

namespace FullDetailDistance
{
	constexpr float BaseFov = 67.f;

	inline float Get()
	{
		if (ps_r1_full_detail_distance_scale >= 1.f || !g_pGamePersistent || !g_pGamePersistent->Environment().CurrentEnv)
		{
			return 0.f;
		}

		return std::max(100.f, g_pGamePersistent->Environment().CurrentEnv->far_plane * ps_r1_full_detail_distance_scale);
	}

	inline bool IsBeyond(const ISpatial& Spatial, float Distance)
	{
		if (Distance <= 0.f)
		{
			return false;
		}

		float Limit = Distance;
		if (Spatial.ssa_d_cam > ISpatial::DefaultSsaDCam || Spatial.ssa_dyn_factor < ISpatial::DefaultSsaDynFactor)
		{
			Limit = std::max(Limit, Spatial.ssa_d_cam * BaseFov / Device.fFOV);
		}

		return Device.vCameraPosition.distance_to_sqr(Spatial.sphere.P) > _sqr(Limit + Spatial.sphere.R);
	}
}
