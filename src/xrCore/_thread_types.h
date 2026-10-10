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
#if __has_include(<version>)
#	include <version>
#endif
#include <type_traits>

// Atomic types
using xr_atomic_u8   = std::atomic_uint8_t;
using xr_atomic_u32  = std::atomic_uint32_t;
using xr_atomic_s32  = std::atomic_int;
using xr_atomic_u64  = std::atomic_uint64_t;
using xr_atomic_s64  = std::atomic_int64_t;
using xr_atomic_bool = std::atomic_bool;
using xr_atomic_float = std::atomic<float>;

#if defined(__cpp_lib_atomic_ref)
template <typename T>
using xr_atomic_ref = std::atomic_ref<T>;
#else
template <typename T>
class xr_atomic_ref
{
public:
	using value_type = T;
	static constexpr size_t required_alignment = alignof(T);
	static constexpr bool is_always_lock_free = true;

	explicit xr_atomic_ref(T& obj) noexcept : ptr(&obj) {}
	xr_atomic_ref(const xr_atomic_ref&) noexcept = default;
	xr_atomic_ref& operator=(const xr_atomic_ref&) = delete;

	void store(T desired, std::memory_order order = std::memory_order_seq_cst) const noexcept
	{
#if defined(__GNUC__) || defined(__clang__)
		__atomic_store_n(ptr, desired, static_cast<int>(order));
#else
		*ptr = desired;
#endif
	}

	T load(std::memory_order order = std::memory_order_seq_cst) const noexcept
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_load_n(ptr, static_cast<int>(order));
#else
		return *ptr;
#endif
	}

	operator T() const noexcept
	{
		return load();
	}

	T exchange(T desired, std::memory_order order = std::memory_order_seq_cst) const noexcept
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_exchange_n(ptr, desired, static_cast<int>(order));
#else
		T old = *ptr;
		*ptr = desired;
		return old;
#endif
	}

	bool compare_exchange_weak(T& expected, T desired,
		std::memory_order success, std::memory_order failure) const noexcept
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_compare_exchange_n(ptr, &expected, desired, true,
			static_cast<int>(success), static_cast<int>(failure));
#else
		if (*ptr == expected)
		{
			*ptr = desired;
			return true;
		}
		expected = *ptr;
		return false;
#endif
	}

	bool compare_exchange_strong(T& expected, T desired,
		std::memory_order success, std::memory_order failure) const noexcept
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_compare_exchange_n(ptr, &expected, desired, false,
			static_cast<int>(success), static_cast<int>(failure));
#else
		if (*ptr == expected)
		{
			*ptr = desired;
			return true;
		}
		expected = *ptr;
		return false;
#endif
	}

	bool compare_exchange_strong(T& expected, T desired,
		std::memory_order order = std::memory_order_seq_cst) const noexcept
	{
		return compare_exchange_strong(expected, desired, order,
			order == std::memory_order_acq_rel ? std::memory_order_acquire :
			(order == std::memory_order_release ? std::memory_order_relaxed : order));
	}

	bool compare_exchange_weak(T& expected, T desired,
		std::memory_order order = std::memory_order_seq_cst) const noexcept
	{
		return compare_exchange_weak(expected, desired, order,
			order == std::memory_order_acq_rel ? std::memory_order_acquire :
			(order == std::memory_order_release ? std::memory_order_relaxed : order));
	}

	T fetch_add(T arg, std::memory_order order = std::memory_order_seq_cst) const noexcept
		requires (std::is_integral_v<T> || std::is_pointer_v<T>)
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_fetch_add(ptr, arg, static_cast<int>(order));
#else
		T old = *ptr;
		*ptr += arg;
		return old;
#endif
	}

	T fetch_sub(T arg, std::memory_order order = std::memory_order_seq_cst) const noexcept
		requires (std::is_integral_v<T> || std::is_pointer_v<T>)
	{
#if defined(__GNUC__) || defined(__clang__)
		return __atomic_fetch_sub(ptr, arg, static_cast<int>(order));
#else
		T old = *ptr;
		*ptr -= arg;
		return old;
#endif
	}

	T operator++() const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_add(1) + 1; }
	T operator++(int) const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_add(1); }
	T operator--() const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_sub(1) - 1; }
	T operator--(int) const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_sub(1); }
	T operator+=(T arg) const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_add(arg) + arg; }
	T operator-=(T arg) const noexcept requires (std::is_integral_v<T> || std::is_pointer_v<T>) { return fetch_sub(arg) - arg; }

private:
	T* ptr = nullptr;
};

template <typename T>
xr_atomic_ref(T&) -> xr_atomic_ref<T>;
#endif

template<typename T>
ISaveObject& operator<<(ISaveObject& obj, std::atomic<T>& Value)
{
	T temp = Value.load();
	obj << temp;
	Value.store(temp);
	return obj;
}

// Tasks Redefinition
#ifdef IXR_WINDOWS
using xr_task_group = concurrency::task_group;
using xr_structured_task_group = concurrency::structured_task_group;
#define xr_make_task(a) Concurrency::make_task(a)

template <typename T, typename U, typename H = ::std::hash<T>>
using xr_concurrent_unordered_map = concurrency::concurrent_unordered_map<T, U, H>;

template <typename T>
using xr_concurrent_vector = concurrency::concurrent_vector<T>;
#else
using xr_task_group = tbb::task_group;
using xr_structured_task_group = tbb::task_group;
#define xr_make_task(a) a

template <typename T, typename U, typename H = std::hash<T>>
using xr_concurrent_unordered_map = tbb::concurrent_unordered_map<T, U, H>;
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

inline const size_t xr_max_concurrency()
{
#ifdef IXR_WINDOWS
	return Concurrency::CurrentScheduler::Get()->GetNumberOfVirtualProcessors();
#elif defined(IXR_LINUX)
	return tbb::this_task_arena::max_concurrency();
#else
	return std::thread::hardware_concurrency();
#endif
}

template<typename BlockRangeType, typename Body>
inline void xr_parallel_for(BlockRangeType Begin, BlockRangeType End, BlockRangeType Grain, Body Functor)
{
#ifdef IXR_WINDOWS
	concurrency::parallel_for(Begin, End, Grain, Functor);
#else
	using RangeType = tbb::blocked_range<BlockRangeType>;
	tbb::parallel_for(RangeType(Begin, End, Grain), [&](const RangeType& Range)
	{
		for (BlockRangeType i = Range.begin(); i != Range.end(); ++i)
		{
			Functor(i);
		}
	});
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

// Run Threads
inline void xr_std_parallel_for(std::function<void()>&& function_to_call, u32 ThreadsMax)
{
 	xr_vector<std::thread> threads;
	for (auto i = 0; i < ThreadsMax; i++)
	{
		threads.emplace_back(std::thread(function_to_call));
	}
	for (auto i = 0; i < ThreadsMax; i++)
	{
		threads[i].join();
 	}
 	threads.clear();
}