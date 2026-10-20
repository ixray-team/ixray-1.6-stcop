#pragma once

#include <atomic>

namespace ResourceLifetime
{
template <typename T>
inline bool IsAlive(const T* Resource)
{
	return Resource->dwReference.load(std::memory_order_acquire) != 0;
}

template <typename T, typename RefType>
bool TryAcquire(T* Resource, RefType& Out)
{
	auto Count = Resource->dwReference.load(std::memory_order_acquire);
	do
	{
		if (Count == 0)
		{
			return false;
		}
	} while (!Resource->dwReference.compare_exchange_weak(Count, Count + 1, std::memory_order_acq_rel, std::memory_order_acquire));

	RefType(Resource, false).swap(Out);
	return true;
}
}  
