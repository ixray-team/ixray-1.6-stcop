#include "../../xrCore/RenderStatistics.h"

#include <cstdlib>
#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return EXIT_FAILURE;
}
} // namespace

int main()
{
	FRenderStatisticsTracker Tracker;
	Tracker.BeginFrame(17);
	Tracker.RecordPass();
	Tracker.RecordPass(2);
	Tracker.RecordDraw(12);
	Tracker.RecordDraw(2, 4);
	Tracker.RecordDispatch(3);
	Tracker.RecordUpload(4096);

	FRenderResourceStatistics Resources;
	Resources.TrackedBufferCount = 4;
	Resources.TrackedTextureCount = 3;
	Resources.TrackedPipelineCount = 2;
	Resources.TrackedDescriptorCount = 9;
	Resources.DeferredResourceCount = 1;
	Resources.TrackedBufferBytes = 8192;
	Resources.TrackedTextureBytes = 16384;
	Tracker.SetResources(Resources);
	Tracker.EndFrame(2500000);

	const FRenderStatisticsSnapshot First = Tracker.GetSnapshot();
	if (First.Version != 1 || First.Revision != 1 ||
		First.Frame.FrameIndex != 17 ||
		First.Frame.CpuFrameNanoseconds != 2500000 ||
		First.Frame.GpuTimingValid ||
		First.Frame.GpuFrameNanoseconds != 0 ||
		First.Frame.PassCount != 3 ||
		First.Frame.DrawCallCount != 2 ||
		First.Frame.DispatchCallCount != 3 ||
		First.Frame.TriangleCount != 14 ||
		First.Frame.LineCount != 4 ||
		First.Frame.UploadBytes != 4096 ||
		First.Resources.TrackedBufferCount != 4 ||
		First.Resources.TrackedTextureCount != 3 ||
		First.Resources.TrackedPipelineCount != 2 ||
		First.Resources.TrackedDescriptorCount != 9 ||
		First.Resources.DeferredResourceCount != 1 ||
		First.Resources.TrackedBufferBytes != 8192 ||
		First.Resources.TrackedTextureBytes != 16384)
	{
		return Fail("The first renderer statistics snapshot is incorrect");
	}

	// A new frame clears transient counters while retaining the latest resource
	// census. Calls outside an open frame must not mutate it.
	Tracker.RecordDraw(99);
	Tracker.BeginFrame(18);
	Tracker.RecordDraw(1);
	Tracker.EndFrame(3000000, 700000, true);
	const FRenderStatisticsSnapshot Second = Tracker.GetSnapshot();
	if (Second.Revision != 2 || Second.Frame.FrameIndex != 18 ||
		Second.Frame.PassCount != 0 || Second.Frame.DrawCallCount != 1 ||
		Second.Frame.TriangleCount != 1 ||
		!Second.Frame.GpuTimingValid ||
		Second.Frame.GpuFrameNanoseconds != 700000 ||
		Second.Resources.TrackedTextureBytes != 16384)
	{
		return Fail("Frame reset or GPU timing validity is incorrect");
	}

	Tracker.Reset();
	const FRenderStatisticsSnapshot Reset = Tracker.GetSnapshot();
	if (Reset.Revision != 0 || Reset.Frame.DrawCallCount != 0 ||
		Reset.Resources.TrackedBufferCount != 0)
	{
		return Fail("Renderer statistics reset is incorrect");
	}

	return EXIT_SUCCESS;
}
