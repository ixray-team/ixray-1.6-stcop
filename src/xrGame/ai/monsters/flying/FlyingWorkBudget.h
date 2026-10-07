#pragma once

// Shared primary-thread budgets. Intrusive pending links keep scheduling work
// proportional to the granted jobs, with no queue allocation on every frame.
class CFlyingWorkBudget
{
public:
	enum class EKind : u32
	{
		AirValidation,
		RouteStart,
		LandingProbe,
		BehaviorProbe,
		SearchExpansion,
		Count
	};

	// The first limit supplied for a kind establishes its global budget.
	// Use Configure explicitly when reloading the common flying settings.
	static bool TryAcquire(const void* Owner, EKind Kind, u32 Limit)
	{
		if (!Owner || u32(Kind) >= u32(EKind::Count))
		{
			return false;
		}
		auto& Queue = GetQueue(Kind);
		if (!Queue.Configured)
		{
			Queue.Limit = Limit;
			Queue.Configured = true;
		}
		AdvanceFrame(Queue);
		auto& Request = Queue.Requests.try_emplace(Owner).first->second;
		if (Request.AcceptedFrame == Device.dwFrame)
		{
			return false;
		}
		if (Queue.ReadyHead)
		{
			const u32 OldestAccepted = Queue.Requests.find(Queue.ReadyHead)->second.AcceptedFrame;
			if (Request.AcceptedFrame != u32(-1) &&
				(OldestAccepted == u32(-1) || Request.AcceptedFrame > OldestAccepted))
			{
				if (!Request.Ready && !Request.Pending)
				{
					Enqueue(Queue, Owner, Request);
				}
				return false;
			}
		}
		if (Request.Ready)
		{
			// Object updates can skip frames. Keep the reservation until this
			// client returns, but charge execution to the current frame's quota.
			if (!Queue.AcceptRemaining)
			{
				// A late caller gets another FIFO turn instead of retaining a
				// ready job that earlier update-order clients can starve forever.
				RemoveReady(Queue, Request);
				Request.Ready = false;
				Enqueue(Queue, Owner, Request);
				return false;
			}
			--Queue.AcceptRemaining;
			RemoveReady(Queue, Request);
			Request.Ready = false;
			Request.AcceptedFrame = Device.dwFrame;
			return true;
		}
		if (Queue.Remaining && Queue.AcceptRemaining && !Queue.Head && !Request.Pending)
		{
			--Queue.Remaining;
			--Queue.AcceptRemaining;
			Request.AcceptedFrame = Device.dwFrame;
			return true;
		}
		if (!Request.Pending)
		{
			Enqueue(Queue, Owner, Request);
		}
		return false;
	}

	// Reserved jobs always remain in the FIFO order established by requests.
	// A configuration change takes effect at the start of the following frame.
	static void Configure(EKind Kind, u32 Limit)
	{
		if (u32(Kind) >= u32(EKind::Count))
		{
			return;
		}
		auto& Queue = GetQueue(Kind);
		Queue.Limit = Limit;
		Queue.Configured = true;
	}

	static u32 EstimatedWaitFrames(EKind Kind)
	{
		if (u32(Kind) >= u32(EKind::Count))
		{
			return 0u;
		}
		const auto& Queue = GetQueue(Kind);
		if (!Queue.Limit)
		{
			return 0u;
		}
		return (Queue.PendingCount + Queue.Outstanding + Queue.Limit - 1u) / Queue.Limit;
	}

	struct SCounts
	{
		u32 Pending = 0, Ready = 0;
	};
	static SCounts Counts(EKind Kind)
	{
		if (u32(Kind) >= u32(EKind::Count))
		{
			return {};
		}
		const auto& Queue = GetQueue(Kind);
		return {Queue.PendingCount, Queue.Outstanding};
	}

	// Abandon an unused reservation without forgetting this client's last turn.
	// Erasing its fairness history would favour frequently changing tasks.
	static void CancelPending(const void* Owner, EKind Kind)
	{
		if (!Owner || u32(Kind) >= u32(EKind::Count))
		{
			return;
		}
		auto& Queue = GetQueue(Kind);
		auto Iterator = Queue.Requests.find(Owner);
		if (Iterator == Queue.Requests.end())
		{
			return;
		}
		auto& Request = Iterator->second;
		if (Request.Ready)
		{
			RemoveReady(Queue, Request);
			Request.Ready = false;
		}
		if (Request.Pending)
		{
			RemovePending(Queue, Request);
		}
	}

	// Call when a controller/behavior is destroyed, or cancels its pending job.
	// Owner pointers are identity keys only and are never dereferenced.
	static void Cancel(const void* Owner)
	{
		if (!Owner)
		{
			return;
		}
		for (u32 Index = 0; Index < u32(EKind::Count); ++Index)
		{
			auto& Queue = GetQueue(EKind(Index));
			auto Iterator = Queue.Requests.find(Owner);
			if (Iterator == Queue.Requests.end())
			{
				continue;
			}
			auto& Request = Iterator->second;
			if (Request.Ready)
			{
				RemoveReady(Queue, Request);
			}
			if (Request.Pending)
			{
				RemovePending(Queue, Request);
			}
			Queue.Requests.erase(Iterator);
			if (Queue.Requests.empty())
			{
				// Return storage before the level's clients and allocator shut down.
				xr_hash_map<const void*, SRequest>().swap(Queue.Requests);
			}
		}
	}

private:
	struct SRequest
	{
		const void* Previous = nullptr;
		const void* Next = nullptr;
		bool Ready = false;
		u32 ReadyFrame = 0;
		u32 AcceptedFrame = u32(-1);
		bool Pending = false;
	};

	struct SQueue
	{
		xr_hash_map<const void*, SRequest> Requests;
		const void* Head = nullptr;
		const void* Tail = nullptr;
		const void* ReadyHead = nullptr;
		const void* ReadyTail = nullptr;
		u32 Frame = u32(-1);
		u32 Limit = 0;
		u32 Remaining = 0;
		u32 AcceptRemaining = 0;
		u32 Outstanding = 0, PendingCount = 0;
		bool Configured = false;
	};

	static SQueue& GetQueue(EKind Kind)
	{
		static SQueue Queues[u32(EKind::Count)];
		return Queues[u32(Kind)];
	}

	static void Enqueue(SQueue& Queue, const void* Owner, SRequest& Request)
	{
		Request.Previous = Queue.Tail;
		Request.Next = nullptr;
		Request.Pending = true;
		++Queue.PendingCount;
		if (Queue.Tail)
		{
			Queue.Requests.find(Queue.Tail)->second.Next = Owner;
		}
		else
		{
			Queue.Head = Owner;
		}
		Queue.Tail = Owner;
	}

	static void RemovePending(SQueue& Queue, SRequest& Request)
	{
		if (Request.Previous)
		{
			Queue.Requests.find(Request.Previous)->second.Next = Request.Next;
		}
		else
		{
			Queue.Head = Request.Next;
		}
		if (Request.Next)
		{
			Queue.Requests.find(Request.Next)->second.Previous = Request.Previous;
		}
		else
		{
			Queue.Tail = Request.Previous;
		}
		Request.Previous = Request.Next = nullptr;
		Request.Pending = false;
		--Queue.PendingCount;
	}

	static void RemoveReady(SQueue& Queue, SRequest& Request)
	{
		if (Request.Previous)
		{
			Queue.Requests.find(Request.Previous)->second.Next = Request.Next;
		}
		else
		{
			Queue.ReadyHead = Request.Next;
		}
		if (Request.Next)
		{
			Queue.Requests.find(Request.Next)->second.Previous = Request.Previous;
		}
		else
		{
			Queue.ReadyTail = Request.Previous;
		}
		Request.Previous = Request.Next = nullptr;
		--Queue.Outstanding;
	}

	static void AdvanceFrame(SQueue& Queue)
	{
		if (Queue.Frame == Device.dwFrame)
		{
			return;
		}
		Queue.Frame = Device.dwFrame;
		Queue.AcceptRemaining = Queue.Limit;
		// State changes can abandon a reservation without destroying its owner.
		// Only inspect the bounded ready list, never all birds on the level.
		while (Queue.ReadyHead)
		{
			auto& Request = Queue.Requests.find(Queue.ReadyHead)->second;
			if (Device.dwFrame - Request.ReadyFrame < 128u)
			{
				break;
			}
			RemoveReady(Queue, Request);
			Request.Ready = false;
		}
		// Reservations are not execution. Cadenced behavior may consume its
		// grant much later, so it must not stop granting work to other clients.
		// Actual execution is still capped by AcceptRemaining for each frame.
		Queue.Remaining = Queue.Limit;
		while (Queue.Remaining && Queue.Head)
		{
			const void* Owner = Queue.Head;
			auto& Request = Queue.Requests.find(Owner)->second;
			RemovePending(Queue, Request);
			Request.Ready = true;
			Request.ReadyFrame = Device.dwFrame;
			Request.Previous = Queue.ReadyTail;
			if (Queue.ReadyTail)
			{
				Queue.Requests.find(Queue.ReadyTail)->second.Next = Owner;
			}
			else
			{
				Queue.ReadyHead = Owner;
			}
			Queue.ReadyTail = Owner;
			++Queue.Outstanding;
			--Queue.Remaining;
		}
	}
};
