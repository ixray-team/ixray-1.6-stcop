#pragma once

#include <tbb/task_group.h>
#include <tbb/parallel_for.h>
#include <tbb/blocked_range.h>
#include <tbb/parallel_for_each.h>
#include <tbb/concurrent_unordered_map.h>

#include <atomic>

// TBB Redefinition
using xr_task_group = tbb::task_group;

template <typename T>
using xr_blocked_range = tbb::blocked_range<T>;

template <typename T, typename U>
using xr_concurrent_unordered_map = tbb::concurrent_unordered_map<T, U>;

template<typename BlockRange, typename Body>
inline void xr_parallel_for(BlockRange Range, Body Functor)
{
	tbb::parallel_for(Range, Functor);
}

template<typename Index, typename Body>
inline void xr_parallel_for(Index Begin, Index End, Body Functor)
{
	tbb::parallel_for_each(Begin, End, Functor);
}

// Atomic types
using xr_atomic_u32 = std::atomic_uint32_t;
using xr_atomic_s32 = std::atomic_int;
using xr_atomic_bool = std::atomic_bool;