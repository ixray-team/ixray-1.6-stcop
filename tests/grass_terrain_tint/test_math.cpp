#include "../../src/Layers/xrRenderPC_R4/GrassTerrainTintMath.h"

#include <iostream>
#include <limits>
#include <utility>

int main()
{
	int Failures = 0;
	auto Check = [&](bool Condition, const char* Name)
	{
		if (!Condition)
		{
			std::cerr << "FAIL: " << Name << '\n';
			++Failures;
		}
	};
	for (const auto& [Requested, Expected] : {
			 std::pair{-1, 2048u}, {0, 2048u}, {256, 256u}, {257, 512u}, {511, 512u}, {512, 512u}, {1024, 1024u}, {1025, 2048u}, {2048, 2048u}, {4096, 4096u}, {8192, 8192u}, {16384, 8192u}
		 })
	{
		Check(GrassTerrainTint::MapResolution(Requested) == Expected, "bounded power-of-two map size");
	}
	Check(GrassTerrainTint::MapResolution(0, 2048) == 2048, "auto matches a 2K terrain texture");
	Check(GrassTerrainTint::MapResolution(0, 4096) == 4096, "auto matches a 4K terrain texture");
	Check(GrassTerrainTint::MapResolution(0, 4096, 2) == 8192, "two-times supersampling for a 4K terrain");
	Check(GrassTerrainTint::MapResolution(0, 3000) == 4096, "auto never downsamples a non-power-of-two texture");
	Check(GrassTerrainTint::MapResolution(0, 0) == 2048, "missing source dimensions use a useful fallback");
	Check(GrassTerrainTint::MapResolution(256, 8192, 1.0001f) == 512, "fractional scale always rounds upward");
	Check(GrassTerrainTint::MapResolution(1024, 8192) == 1024, "manual override can lower the memory footprint");
	Check(GrassTerrainTint::MapResolution(0, 8192, 4) == 8192, "supersampling is bounded");
	Check(GrassTerrainTint::MapResolution(0, 4096, std::numeric_limits<float>::quiet_NaN()) == 4096, "invalid scale stays finite");
	auto Near = [](float A, float B)
	{ return std::abs(A - B) < 0.0001f; };
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, 2), 1), "two-meter filter at one meter per texel");
	Check(Near(GrassTerrainTint::SmoothingMip(2048, 1024, 1024, 2), 0), "rectangular level uses larger texel footprint");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, 4), 2), "four-meter filter");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, 0), 0), "zero filter radius");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, -5), 0), "negative filter radius");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, 1e20f), 10), "mip never exceeds allocated chain");
	Check(Near(GrassTerrainTint::SmoothingMip(0, 1024, 1024, 2), 0), "empty map bounds");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 0, 2), 0), "unallocated map");
	Check(Near(GrassTerrainTint::SmoothingMip(std::numeric_limits<float>::infinity(), 1024, 1024, 2), 0), "nonfinite map bounds");
	Check(Near(GrassTerrainTint::SmoothingMip(1024, 1024, 1024, std::numeric_limits<float>::quiet_NaN()), 0), "nonfinite filter radius");
	if (Failures)
	{
		return 1;
	}
	std::cout << "PASS: source-sized auto maps, supersampling, manual limits, and spatial filter footprint\n";
	return 0;
}
