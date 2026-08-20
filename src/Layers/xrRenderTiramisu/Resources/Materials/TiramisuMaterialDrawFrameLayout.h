#pragma once

#include "TiramisuRenderTypes.h"

#include <MaterialRuntime.h>

// Делит per-draw GPU table на независимые регионы queued frames.
class TiramisuMaterialDrawFrameLayout final
{
public:
	static constexpr u32 BufferedFrameCount = 3;
	static constexpr u32 MaxDrawsPerFrame = 65536;
	static constexpr u64 BufferSize =
		u64(BufferedFrameCount) * MaxDrawsPerFrame *
		MaterialDrawGpuDataSize;

	[[nodiscard]] static constexpr u32 GetAbsoluteDrawIndex(
		const u32 FrameSlot,
		const u32 LocalDrawIndex
	) noexcept
	{
		return FrameSlot * MaxDrawsPerFrame + LocalDrawIndex;
	}
};

static_assert(TiramisuMaterialDrawFrameLayout::BufferedFrameCount >= 2);
static_assert(TiramisuMaterialDrawFrameLayout::BufferSize % 16 == 0);
