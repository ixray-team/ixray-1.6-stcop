#include "stdafx.h"
#include "Atomic.h"

u32 Platform::AtomicCompareExchange(u32* Destination, u32 Expected, u32 Desired)
{
#ifdef IXR_WINDOWS
	static_assert(sizeof(long) == sizeof(u32));
	return static_cast<u32>(_InterlockedCompareExchange(reinterpret_cast<long*>(Destination), static_cast<long>(Desired), static_cast<long>(Expected)));
#else
	__atomic_compare_exchange_n(Destination, &Expected, Desired, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
	return Expected;
#endif
}

