#pragma once

#include "../../../xrCore/xrCore.h"

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <deque>
#include <future>
#include <functional>
#include <utility>
#include <vector>

// Small editor-side scheduler for expensive background work. Enqueue never
// creates more than MaxConcurrency worker threads; excess requests remain
// owned by the queue until a running job completes.
template <typename TRequest, typename TResult>
// Ограниченная очередь фоновых editor jobs с контролируемой остановкой.
class TEditorBoundedAsyncQueue
{
public:
	using FWorker = std::function<TResult(TRequest)>;

	explicit TEditorBoundedAsyncQueue(
		const size_t InMaxConcurrency, FWorker InWorker
	)
		: MaxConcurrency(std::max<size_t>(1, InMaxConcurrency)), Worker(std::move(InWorker))
	{
	}

	void Enqueue(TRequest Request)
	{
		Pending.push_back(std::move(Request));
		Pump();
	}

	template <typename TPredicate>
	void ErasePendingIf(TPredicate&& Predicate)
	{
		std::erase_if(Pending, std::forward<TPredicate>(Predicate));
	}

	[[nodiscard]] xr_vector<TResult> PollReady()
	{
		using namespace std::chrono_literals;
		xr_vector<TResult> Ready;
		for (auto It = Active.begin(); It != Active.end();)
		{
			if (It->wait_for(0s) != std::future_status::ready)
			{
				++It;
				continue;
			}
			Ready.push_back(It->get());
			It = Active.erase(It);
		}
		Pump();
		return Ready;
	}

	void CancelPendingAndWait()
	{
		Pending.clear();
		for (std::future<TResult>& Job : Active)
		{
			if (Job.valid())
			{
				Job.wait();
			}
		}
		Active.clear();
	}

	[[nodiscard]] size_t PendingCount() const noexcept
	{
		return Pending.size();
	}

	[[nodiscard]] size_t ActiveCount() const noexcept
	{
		return Active.size();
	}

	[[nodiscard]] size_t PeakActiveCount() const noexcept
	{
		return PeakActive;
	}

private:
	void Pump()
	{
		while (Active.size() < MaxConcurrency && !Pending.empty())
		{
			TRequest Request = std::move(Pending.front());
			Pending.pop_front();
			Active.push_back(std::async(std::launch::async, [Worker = Worker, Request = std::move(Request)]() mutable
										{ return Worker(std::move(Request)); }));
			PeakActive = std::max(PeakActive, Active.size());
		}
	}

	size_t MaxConcurrency = 1;
	FWorker Worker;
	std::deque<TRequest> Pending;
	xr_vector<std::future<TResult>> Active;
	size_t PeakActive = 0;
};
