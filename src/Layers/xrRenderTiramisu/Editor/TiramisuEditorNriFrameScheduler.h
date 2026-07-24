#pragma once

#include "../../../xrCore/xrCore.h"

#include <cstdint>
#include <optional>

// План кадровых NRI-операций editor bridge без передачи NRI в UI-код.
struct FEditorNriFramePlan
{
	u32 FrameContextIndex = 0;
	u32 RecycledSemaphoreIndex = 0;
	u64 ReuseFenceValue = 0;
	u64 SignalFenceValue = 0;
};

[[nodiscard]] inline xr_optional<FEditorNriFramePlan>
MakeEditorNriFramePlan(const u64 FrameIndex, const u32 QueuedFrameCount, const u32 SwapTextureCount) noexcept
{
	if (QueuedFrameCount == 0 || SwapTextureCount == 0)
	{
		return std::nullopt;
	}
	FEditorNriFramePlan Result;
	Result.FrameContextIndex = static_cast<u32>(
		FrameIndex % QueuedFrameCount
	);
	Result.RecycledSemaphoreIndex = static_cast<u32>(
		FrameIndex % SwapTextureCount
	);
	Result.ReuseFenceValue = FrameIndex >= QueuedFrameCount
								 ? 1 + FrameIndex - QueuedFrameCount
								 : 0;
	Result.SignalFenceValue = 1 + FrameIndex;
	return Result;
}
