#include "../../Layers/xrRenderTiramisu/Editor/TEditorBoundedAsyncQueue.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <thread>

int main()
{
	constexpr size_t MaximumConcurrency = 3;
	constexpr u32 RequestCount = 24;
	std::atomic<size_t> Running = 0;
	std::atomic<size_t> ObservedPeak = 0;
	TEditorBoundedAsyncQueue<u32, u32> Queue(
		MaximumConcurrency,
		[&](const u32 Value)
		{
			const size_t Current = Running.fetch_add(1) + 1;
			size_t Peak = ObservedPeak.load();
			while (Peak < Current &&
				   !ObservedPeak.compare_exchange_weak(Peak, Current))
			{
			}
			std::this_thread::sleep_for(std::chrono::milliseconds(2));
			Running.fetch_sub(1);
			return Value * Value;
		}
	);

	for (u32 Index = 0; Index < RequestCount; ++Index)
	{
		Queue.Enqueue(Index);
	}
	if (Queue.ActiveCount() != MaximumConcurrency ||
		Queue.PendingCount() != RequestCount - MaximumConcurrency)
	{
		std::cerr << "Bounded queue did not retain excess work\n";
		return 1;
	}

	u32 Completed = 0;
	u64 Sum = 0;
	while (Completed < RequestCount)
	{
		for (const u32 Result : Queue.PollReady())
		{
			++Completed;
			Sum += Result;
		}
		std::this_thread::yield();
	}

	constexpr u64 ExpectedSum = 4324;
	if (Sum != ExpectedSum || ObservedPeak.load() > MaximumConcurrency ||
		Queue.PeakActiveCount() > MaximumConcurrency ||
		Queue.ActiveCount() != 0 || Queue.PendingCount() != 0)
	{
		std::cerr << "Bounded queue exceeded its limit or lost a result\n";
		return 1;
	}

	return 0;
}
