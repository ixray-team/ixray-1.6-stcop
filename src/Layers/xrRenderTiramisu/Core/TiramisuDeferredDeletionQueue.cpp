#include "TiramisuDeferredDeletionQueue.h"

#include <algorithm>

namespace Tiramisu
{
bool TiramisuDeferredDeletionQueue::Enqueue(const u64 RetireFence, FDeleteFunction Function)
{
	if (!Function)
	{
		return false;
	}

	Entries.push_back({RetireFence, NextSequence++, std::move(Function)});
	std::ranges::stable_sort(Entries, [](const FEntry& Left, const FEntry& Right)
							 {
        if (Left.RetireFence != Right.RetireFence){
            return Left.RetireFence < Right.RetireFence;
}
        return Left.Sequence < Right.Sequence; });
	return true;
}

size_t TiramisuDeferredDeletionQueue::Collect(const u64 CompletedFence)
{
	const auto ReadyEnd = std::upper_bound(Entries.begin(), Entries.end(), CompletedFence, [](const u64 Fence, const FEntry& Entry)
										   { return Fence < Entry.RetireFence; });
	const size_t Count = static_cast<size_t>(ReadyEnd - Entries.begin());
	for (auto It = Entries.begin(); It != ReadyEnd; ++It)
	{
		It->Function();
	}
	Entries.erase(Entries.begin(), ReadyEnd);
	return Count;
}

size_t TiramisuDeferredDeletionQueue::Flush()
{
	const size_t Count = Entries.size();
	for (FEntry& Entry : Entries)
	{
		Entry.Function();
	}
	Entries.clear();
	return Count;
}
} // namespace Tiramisu
