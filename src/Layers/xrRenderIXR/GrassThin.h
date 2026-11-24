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

#include <bit>
#include "xrRender_console.h"

// r_grass_quality: how far from the eye the grass keeps every blade, and how far it keeps its whole mesh
inline float GrassThinStart()
{
	static constexpr float Starts[] = { 24.0f, 40.0f, 80.0f, 144.0f };
	return Starts[std::min<u32>(ps_r_grass_quality, 3)];
}

inline float GrassFarStart()
{
	static constexpr float Starts[] = { 20.0f, 32.0f, 56.0f, 80.0f };
	return Starts[std::min<u32>(ps_r_grass_quality, 3)];
}

inline float GrassFraction(float Value)
{
	return Value - std::floor(Value);
}

inline float GrassLot(const Fvector& At)
{
	u32 Bits = std::bit_cast<u32>(At.x) * 0x9E3779B1u ^ std::bit_cast<u32>(At.z) * 0x85EBCA77u;
	Bits ^= Bits >> 15;
	Bits *= 0x2C1B3C6Du;
	Bits ^= Bits >> 12;
	return float(Bits >> 8) * (1.0f / 16777216.0f);
}

// The share of the blades kept at a distance
inline float GrassKept(float DistanceSqr)
{
	return std::clamp(GrassThinStart() * GrassThinStart() / std::max(DistanceSqr, 1e-4f), 1.0f / 16.0f, 1.0f);
}

// GrassSize of scene_tables.hlsli, with a lot of its own
inline float GrassThinSize(const Fvector& At, const Fvector& Eye)
{
	const float Kept = GrassKept(At.distance_to_sqr(Eye));
	const float Lot = GrassLot(At);

	if (Lot >= Kept)
	{
		return 0.0f;
	}

	return std::min((Kept - Lot) / (0.25f * Kept), 1.0f) / std::sqrt(std::sqrt(Kept));
}

// The far mesh rule of scene_grass_cull.cs.hlsl
inline bool GrassIsFar(const Fvector& At, const Fvector& Eye)
{
	return At.distance_to(Eye) > GrassFarStart() * (0.75f + 0.5f * GrassFraction(GrassLot(At) * 17.0f));
}
