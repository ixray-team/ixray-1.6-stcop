#pragma once

namespace Cinema
{
struct ShakeSample
{
	float Values[6];
};

struct ShakePreset
{
	const char* Name;
	const ShakeSample* Samples;
	u32 Count;
	float FramesPerSecond;
};

inline constexpr u32 ShakePresetCount = 9;
extern const ShakePreset ShakePresets[ShakePresetCount];
}
