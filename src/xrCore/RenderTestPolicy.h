#pragma once

#include "RenderDebugPolicy.h"

#include <cstdint>
#include <string_view>

// Shared deterministic contract for renderer GPU tests. The values are kept
// here rather than in individual runners so the game, editors and shaders use
// the same clock/seed/weather baseline.
struct FRenderDeterministicTestPolicy
{
	bool Enabled = false;
	bool RequiredRdbgPresent = false;
	std::uint32_t RandomSeed = 0x13572468u;
	float FixedDeltaSeconds = 1.0f / 60.0f;
	float FixedShaderTimeSeconds = 123.0f;
	float FixedWeatherTimeSeconds = 12.0f * 60.0f * 60.0f;
	float FixedExposure = 1.0f;

	[[nodiscard]] constexpr bool IsValid() const noexcept
	{
		return !Enabled || RequiredRdbgPresent;
	}
};

[[nodiscard]] inline FRenderDeterministicTestPolicy
ResolveRenderDeterministicTestPolicy(
	const std::string_view CommandLine) noexcept
{
	FRenderDeterministicTestPolicy Result;
	Result.Enabled =
		HasRenderCommandLineFlag(CommandLine, "-render-deterministic");
	// Acceptance runs deliberately require the exact documented spelling.
	// -rdebug may remain a renderer debug alias, but it must not accidentally
	// make a deterministic test count as an accepted -rdbg run.
	Result.RequiredRdbgPresent =
		HasRenderCommandLineFlag(CommandLine, "-rdbg");
	return Result;
}
