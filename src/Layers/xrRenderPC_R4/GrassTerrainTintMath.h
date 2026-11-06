#pragma once

#include <algorithm>
#include <cmath>

namespace GrassTerrainTint
{
inline unsigned MapResolution(int Requested, unsigned Source = 2048, float Scale = 1.0f)
{
	unsigned Result = 256;
	const float Base = Requested > 0 ? float(Requested) : float(Source ? Source : 2048);
	const float Multiplier = std::isfinite(Scale) ? std::clamp(Scale, 1.0f, 4.0f) : 1.0f;
	const unsigned Limit = static_cast<unsigned>(std::ceil(std::clamp(Base * Multiplier, 256.0f, 8192.0f)));
	while (Result < Limit)
	{
		Result *= 2;
	}
	return Result;
}

inline float SmoothingMip(float Width, float Depth, unsigned Resolution, float Radius)
{
	if (Resolution == 0 || !std::isfinite(Width) || !std::isfinite(Depth) || !std::isfinite(Radius) || Width <= 0.0f || Depth <= 0.0f)
	{
		return 0.0f;
	}
	const float TexelSize = std::max(Width, Depth) / static_cast<float>(Resolution);
	return std::clamp(std::log2(std::max(1.0f, Radius / TexelSize)), 0.0f, std::log2(static_cast<float>(Resolution)));
}
} // namespace GrassTerrainTint
