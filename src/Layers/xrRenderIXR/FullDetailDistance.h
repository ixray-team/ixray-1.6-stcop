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

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "xrRender_console.h"

namespace FullDetailDistance
{
	constexpr float BaseFov = 67.f;

	inline float Get()
	{
		if (ps_r1_full_detail_distance_scale >= 1.f || !RFrame.HasEnv())
		{
			return 0.f;
		}

		return std::max(100.f, RFrame.Env().far_plane * ps_r1_full_detail_distance_scale);
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
			Limit = std::max(Limit, Spatial.ssa_d_cam * BaseFov / RFrame.fFOV);
		}

		return RFrame.vCameraPosition.distance_to_sqr(Spatial.sphere.P) > _sqr(Limit + Spatial.sphere.R);
	}
}
