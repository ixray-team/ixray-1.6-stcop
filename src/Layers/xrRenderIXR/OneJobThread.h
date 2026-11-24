/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once

#include <atomic>
#include <functional>
#include <thread>

// A thread that does one piece of work at a time for the thread that owns it: the owner hands the work over, goes on
// with its frames and takes the result when it is done. For what would stop a frame: reading a file, creating a
// large resource
class OneJobThread
{
public:
	~OneJobThread() { Stop(); }

	void Start()
	{
		if (!Thread.joinable())
		{
			Thread = std::jthread([this](std::stop_token Token) { Run(Token); });
		}
	}

	void Stop()
	{
		if (Thread.joinable())
		{
			Thread.request_stop();
			Signal.fetch_add(1, std::memory_order_release);
			Signal.notify_one();
			Thread.join();
		}

		State.store(Idle, std::memory_order_release);
	}

	// False while the last piece of work is not taken back with Finish
	bool Begin(std::function<void()> Job)
	{
		u32 Expected = Idle;

		if (!Thread.joinable() || !State.compare_exchange_strong(Expected, Taking, std::memory_order_acquire))
		{
			return false;
		}

		Work = std::move(Job);
		State.store(Working, std::memory_order_release);
		Signal.fetch_add(1, std::memory_order_release);
		Signal.notify_one();
		return true;
	}

	bool IsIdle() const { return State.load(std::memory_order_acquire) == Idle; }

	// True once, when the work is done: the thread is free again
	bool Finish()
	{
		u32 Expected = Done;
		return State.compare_exchange_strong(Expected, Idle, std::memory_order_acquire);
	}

	// Blocks until the work in hand, if any, is done
	void Wait()
	{
		while (State.load(std::memory_order_acquire) == Working)
		{
			State.wait(Working, std::memory_order_acquire);
		}
	}

private:
	enum : u32
	{
		Idle,
		Taking,
		Working,
		Done
	};

	void Run(std::stop_token Token)
	{
		u32 Seen = Signal.load(std::memory_order_acquire);

		while (!Token.stop_requested())
		{
			if (State.load(std::memory_order_acquire) == Working)
			{
				Work();
				Work = {};
				State.store(Done, std::memory_order_release);
				State.notify_all();
			}

			Signal.wait(Seen, std::memory_order_acquire);
			Seen = Signal.load(std::memory_order_acquire);
		}
	}

	std::jthread Thread;
	std::function<void()> Work;
	std::atomic<u32> State{ Idle };
	std::atomic<u32> Signal{ 0 };
};
