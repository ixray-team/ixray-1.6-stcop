#pragma once

#include "_types.h"

#include <cstdint>

struct FRenderFrameStatistics
{
	u64 FrameIndex = 0;
	u64 CpuFrameNanoseconds = 0;
	u64 GpuFrameNanoseconds = 0;
	u32 PassCount = 0;
	u32 DrawCallCount = 0;
	u32 DispatchCallCount = 0;
	u64 TriangleCount = 0;
	u64 LineCount = 0;
	u64 UploadBytes = 0;
	bool GpuTimingValid = false;
};

// Counts are explicitly named "Tracked": API allocation size and driver VRAM
// residency are not interchangeable. A future budget extension can publish
// those values separately without changing the meaning of this baseline ABI.
struct FRenderResourceStatistics
{
	u32 TrackedBufferCount = 0;
	u32 TrackedTextureCount = 0;
	u32 TrackedPipelineCount = 0;
	u32 TrackedDescriptorCount = 0;
	u32 DeferredResourceCount = 0;
	u64 TrackedBufferBytes = 0;
	u64 TrackedTextureBytes = 0;
};

struct FRenderStatisticsSnapshot
{
	u32 Version = 1;
	u64 Revision = 0;
	FRenderFrameStatistics Frame;
	FRenderResourceStatistics Resources;
};

class FRenderStatisticsTracker
{
public:
	void Reset() noexcept
	{
		Snapshot = {};
		FrameOpen = false;
	}

	void BeginFrame(const u64 FrameIndex) noexcept
	{
		Snapshot.Frame = {};
		Snapshot.Frame.FrameIndex = FrameIndex;
		FrameOpen = true;
	}

	void RecordPass(const u32 Count = 1) noexcept
	{
		if (FrameOpen)
		{
			Snapshot.Frame.PassCount += Count;
		}
	}

	void RecordDraw(const u64 TriangleCount, const u64 LineCount = 0) noexcept
	{
		if (!FrameOpen)
		{
			return;
		}
		++Snapshot.Frame.DrawCallCount;
		Snapshot.Frame.TriangleCount += TriangleCount;
		Snapshot.Frame.LineCount += LineCount;
	}

	void RecordDispatch(const u32 Count = 1) noexcept
	{
		if (FrameOpen)
		{
			Snapshot.Frame.DispatchCallCount += Count;
		}
	}

	void RecordUpload(const u64 ByteCount) noexcept
	{
		if (FrameOpen)
		{
			Snapshot.Frame.UploadBytes += ByteCount;
		}
	}

	void SetResources(const FRenderResourceStatistics& Resources) noexcept
	{
		Snapshot.Resources = Resources;
	}

	void EndFrame(const u64 CpuFrameNanoseconds, const u64 GpuFrameNanoseconds = 0, const bool GpuTimingValid = false) noexcept
	{
		if (!FrameOpen)
		{
			return;
		}
		Snapshot.Frame.CpuFrameNanoseconds = CpuFrameNanoseconds;
		Snapshot.Frame.GpuFrameNanoseconds =
			GpuTimingValid ? GpuFrameNanoseconds : 0;
		Snapshot.Frame.GpuTimingValid = GpuTimingValid;
		++Snapshot.Revision;
		FrameOpen = false;
	}

	[[nodiscard]] const FRenderStatisticsSnapshot& GetSnapshot() const noexcept
	{
		return Snapshot;
	}

private:
	FRenderStatisticsSnapshot Snapshot;
	bool FrameOpen = false;
};
