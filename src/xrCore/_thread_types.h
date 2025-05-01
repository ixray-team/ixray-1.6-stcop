#pragma once

#ifdef IXR_WINDOWS
#	include <ppl.h>
#	include <concurrent_unordered_map.h>
#	include <concurrent_vector.h>
#else
#	include <tbb/task_group.h>
#	include <tbb/parallel_for.h>
#	include <tbb/blocked_range.h>
#	include <tbb/parallel_for_each.h>
#	include <tbb/concurrent_unordered_map.h>
#	include <tbb/concurrent_vector.h>
#endif
#include <atomic>

// Atomic types
using xr_atomic_u8  = std::atomic_uint8_t;
using xr_atomic_u32  = std::atomic_uint32_t;
using xr_atomic_s32  = std::atomic_int;
using xr_atomic_bool = std::atomic_bool;
using xr_atomic_float = std::atomic<float>;

// Tasks Redefinition
#ifdef IXR_WINDOWS
using xr_task_group = concurrency::task_group;

template <typename T, typename U>
using xr_concurrent_unordered_map = concurrency::concurrent_unordered_map<T, U>;

template <typename T>
using xr_concurrent_vector = concurrency::concurrent_vector<T>;
#else
using xr_task_group = tbb::task_group;

template <typename T, typename U>
using xr_concurrent_unordered_map = tbb::concurrent_unordered_map<T, U>;
template <typename T>
using xr_concurrent_vector = tbb::concurrent_vector<T>;
#endif

template<typename BlockRangeType, typename Body>
inline void xr_parallel_for(BlockRangeType Begin, BlockRangeType End, Body Functor)
{
#ifdef IXR_WINDOWS
	concurrency::parallel_for(Begin, End, Functor);
#else
	using RangeType = tbb::blocked_range<BlockRangeType>;
	RangeType RangeBlock(Begin, End);

	tbb::parallel_for
	(
		RangeBlock,
		[&Functor](const RangeType& Range)
		{
			for (BlockRangeType Iter = Range.begin(); Iter != Range.end(); ++Iter)
			{
				Functor(Iter);
			}
		}
	);
#endif
}

template<typename Index, typename Body>
inline void xr_parallel_foreach(Index Begin, Index End, Body Functor)
{
#ifdef IXR_WINDOWS
	concurrency::parallel_for_each(Begin, End, Functor);
#else
	tbb::parallel_for_each(Begin, End, Functor);
#endif
}
