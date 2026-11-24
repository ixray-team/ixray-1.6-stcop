/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Core
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
#include "stdafx.h"
#include "../../FrameStackSampler.h"
#include <dbghelp.h>
#include <atomic>
#include <cstdio>
#include <thread>
#include <timeapi.h>

#pragma comment(lib, "winmm.lib")

extern xrCriticalSection g_ThreadStackSymbolLock;

namespace
{
constexpr u32 MaxDepth = 48;
constexpr u32 MaxSamples = 1024;
constexpr u32 MaxReports = 64; // a run
constexpr u32 MaxThreads = 4; // the primary one first

struct Sample
{
	u64 Addresses[MaxDepth];
	u32 Depth;
};

std::jthread Watcher;
std::jthread Writer;

// A frame that ended late, handed from the watcher to the thread that writes reports: naming the functions takes
// a tenth of a second, which the watcher has no time for and the game must not wait on
struct Late
{
	xr_vector<Sample> Stacks[MaxThreads];
	float FrameMs;
	float LimitMs;
	u32 Stride;
};

xrCriticalSection LateGuard;
xr_vector<Late> LateFrames;
std::atomic<u32> LateSignal{ 0 };
constexpr size_t MaxWaiting = 8;
std::atomic<u64> FrameStart{ 0 };
std::atomic<u32> FrameId{ 0 };
std::atomic<float> FrameTook{ 0.0f }; // of the frame that ended last, by the primary thread

struct Followed
{
	std::atomic<u32> Id{ 0 };
	char Name[48] = "the primary thread";
};

Followed Threads[MaxThreads];
std::atomic<u32> ThreadCount{ 1 };
xrCriticalSection FollowGuard;
xr_string ReportFolder;
float Limit = 8.0f; // the least a frame has to take to be reported
std::atomic<float> LimitNow{ 8.0f }; // that, or several times the usual frame when it is more
float Usual = 0.0f; // of the primary thread's frames, by that thread alone
u32 Stride = 1; // milliseconds between the samples of the frame being watched
Sample* Samples[MaxThreads] = {};
u32 Reports = 0;

// The stack is read by the unwinder alone: nothing here may take a lock the stopped thread could hold
u32 WalkStopped(HANDLE Thread, u64* Addresses)
{
	u32 Depth = 0;
#if defined(_M_X64)
	CONTEXT Context = {};
	Context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;

	if (!GetThreadContext(Thread, &Context))
	{
		return 0;
	}

	while (Depth < MaxDepth && Context.Rip != 0)
	{
		Addresses[Depth++] = Context.Rip;
		DWORD64 Base = 0;
		const PRUNTIME_FUNCTION Function = RtlLookupFunctionEntry(Context.Rip, &Base, nullptr);

		if (!Function)
		{
			// A leaf: the return address is on top of the stack
			if (IsBadReadPtr(reinterpret_cast<const void*>(Context.Rsp), sizeof(DWORD64)))
			{
				break;
			}

			Context.Rip = *reinterpret_cast<const DWORD64*>(Context.Rsp);
			Context.Rsp += sizeof(DWORD64);
			continue;
		}

		void* Handler = nullptr;
		DWORD64 Establisher = 0;
		RtlVirtualUnwind(UNW_FLAG_NHANDLER, Base, Context.Rip, Function, &Context, &Handler, &Establisher, nullptr);
	}
#endif
	return Depth;
}

void Name(HANDLE Process, u64 Address, char* Text, size_t Size)
{
	alignas(SYMBOL_INFO) char Buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME] = {};
	SYMBOL_INFO* Symbol = reinterpret_cast<SYMBOL_INFO*>(Buffer);
	Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	Symbol->MaxNameLen = MAX_SYM_NAME;
	IMAGEHLP_MODULE64 Module = {};
	Module.SizeOfStruct = sizeof(Module);
	DWORD64 Offset = 0;
	const bool HasModule = SymGetModuleInfo64(Process, Address, &Module) != FALSE;
	const bool HasSymbol = SymFromAddr(Process, Address, &Offset, Symbol) != FALSE;
	std::snprintf(Text, Size, "%s!%s", HasModule ? Module.ModuleName : "?", HasSymbol ? Symbol->Name : "?");
}

void WriteThread(FILE* File, HANDLE Process, const xr_vector<Sample>& Stacks)
{
	// A function by the samples it is anywhere in: what the time was spent under
	xr_hash_map<u64, u32> Inside;
	xr_hash_map<u64, u32> OnTop;
	xr_vector<const Sample*> Taken;

	for (const Sample& Item : Stacks)
	{
		if (!Item.Depth)
		{
			continue;
		}

		Taken.push_back(&Item);
		xr_hash_set<u64> Seen;

		for (u32 Level = 0; Level < Item.Depth; ++Level)
		{
			DWORD64 Offset = 0;
			alignas(SYMBOL_INFO) char Buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME] = {};
			SYMBOL_INFO* Symbol = reinterpret_cast<SYMBOL_INFO*>(Buffer);
			Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
			Symbol->MaxNameLen = MAX_SYM_NAME;
			const u64 Start = SymFromAddr(Process, Item.Addresses[Level], &Offset, Symbol) ? Symbol->Address : Item.Addresses[Level];

			if (Seen.insert(Start).second)
			{
				++Inside[Start];
			}

			if (Level == 0)
			{
				++OnTop[Start];
			}
		}
	}

	if (Taken.empty())
	{
		std::fprintf(File, "\r\nNo sample of it.\r\n");
		return;
	}

	const auto Print = [&](const char* Title, const xr_hash_map<u64, u32>& Counts, u32 Most)
	{
		xr_vector<std::pair<u32, u64>> Order;

		for (const auto& [Address, Hits] : Counts)
		{
			Order.emplace_back(Hits, Address);
		}

		std::sort(Order.begin(), Order.end(), [](const auto& A, const auto& B) { return A.first > B.first; });
		std::fprintf(File, "\r\n%s\r\n", Title);

		for (u32 Index = 0; Index < std::min<u32>(Most, u32(Order.size())); ++Index)
		{
			char Text[1024];
			Name(Process, Order[Index].second, Text, sizeof(Text));
			std::fprintf(File, "  %5.1f %%  %s\r\n", 100.0f * float(Order[Index].first) / float(Taken.size()), Text);
		}
	};

	Print("Where the thread was (the function on top of the stack):", OnTop, 16);
	Print("What it was under (the function anywhere in the stack):", Inside, 48);

	// The stack of the middle sample whole, from the top down
	const Sample& Middle = *Taken[Taken.size() / 2];
	std::fprintf(File, "\r\nThe stack of the middle sample:\r\n");

	for (u32 Level = 0; Level < Middle.Depth; ++Level)
	{
		char Text[1024];
		Name(Process, Middle.Addresses[Level], Text, sizeof(Text));
		std::fprintf(File, "  %02u %s\r\n", Level, Text);
	}
}

void WriteReport(const Late& Frame)
{
	const u32 Count = u32(Frame.Stacks[0].size());
	const float FrameMs = Frame.FrameMs;

	if (!Count || Reports >= MaxReports)
	{
		return;
	}

	++Reports;
	xrCriticalSectionGuard Lock(g_ThreadStackSymbolLock);
	const HANDLE Process = GetCurrentProcess();
	SymSetOptions(SymGetOptions() | SYMOPT_DEFERRED_LOADS | SYMOPT_UNDNAME);

	if (!SymInitialize(Process, nullptr, TRUE))
	{
		return;
	}

	SYSTEMTIME Time;
	GetLocalTime(&Time);
	char Path[MAX_PATH * 2];
	std::snprintf(Path, sizeof(Path), "%s%scpu_spike_%02u-%02u-%02u-%03u_%ums.txt", ReportFolder.c_str(), !ReportFolder.empty() && (ReportFolder.back() == '\\' || ReportFolder.back() == '/') ? "" : "\\",
		Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds, u32(FrameMs));
	FILE* File = nullptr;

	if (fopen_s(&File, Path, "wb") == 0 && File)
	{
		std::fprintf(File, "A frame of %.1f ms (a frame is reported over %.1f ms): %u samples of the primary thread, one every %u ms from its start\r\n", FrameMs, Frame.LimitMs, Count, Frame.Stride);
		WriteThread(File, Process, Frame.Stacks[0]);

		for (u32 Slot = 1; Slot < ThreadCount.load(std::memory_order_acquire); ++Slot)
		{
			std::fprintf(File, "\r\n======== %s, sampled at the same moments ========\r\n", Threads[Slot].Name);
			WriteThread(File, Process, Frame.Stacks[Slot]);
		}

		std::fclose(File);
		Msg("! CPU spike: a frame of %.1f ms, %u samples written to %s", FrameMs, Count, Path);
	}

	SymCleanup(Process);
}

void Watch(std::stop_token Stop)
{
	// Never put aside while the primary thread stands stopped: it may hold a lock the workers wait for
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
	HANDLE Handles[MaxThreads] = {};
	u32 Current = FrameId.load(std::memory_order_acquire);
	u32 Count = 0;
	u32 Tick = 0;
	Stride = 1;

	// Every frame is sampled from its start and judged when it ends: sampled only once it is late, a frame that
	// is half as long again as the limit left one stack of its tail
	while (!Stop.stop_requested())
	{
		Sleep(1);
		const u32 Frame = FrameId.load(std::memory_order_acquire);

		if (Frame != Current)
		{
			// The watcher may sleep through a frame's end: the samples then lie in several frames, none of them late
			// by itself, and are thrown away
			const float Took = FrameTook.load(std::memory_order_acquire);
			const bool IsOneFrame = Frame == Current + 1 && FrameId.load(std::memory_order_acquire) == Frame;
			const float LimitMs = LimitNow.load(std::memory_order_acquire);

			if (IsOneFrame && Count && Took > LimitMs)
			{
				xrCriticalSectionGuard Lock(LateGuard);

				if (LateFrames.size() < MaxWaiting)
				{
					Late& Frame = LateFrames.emplace_back();
					Frame.FrameMs = Took;
					Frame.LimitMs = LimitMs;
					Frame.Stride = Stride;

					for (u32 Slot = 0; Slot < MaxThreads; ++Slot)
					{
						Frame.Stacks[Slot].assign(Samples[Slot], Samples[Slot] + Count);
					}

					LateSignal.fetch_add(1, std::memory_order_release);
					LateSignal.notify_one();
				}
			}

			Current = FrameId.load(std::memory_order_acquire);
			Count = 0;
			Tick = 0;
			Stride = 1;
		}

		if (++Tick % Stride)
		{
			continue;
		}

		// A long frame keeps every second sample and takes them half as often from there on
		if (Count == MaxSamples)
		{
			for (u32 Slot = 0; Slot < MaxThreads; ++Slot)
			{
				for (u32 Index = 0; Index < MaxSamples / 2; ++Index)
				{
					Samples[Slot][Index] = Samples[Slot][Index * 2];
				}
			}

			Count = MaxSamples / 2;
			Stride *= 2;
		}

		// One thread stands stopped at a time: the others go on, so what a sample shows of a worker is a moment
		// later than what it shows of the primary thread
		for (u32 Slot = 0; Slot < MaxThreads; ++Slot)
		{
			Sample& Item = Samples[Slot][Count];
			Item.Depth = 0;

			if (!Handles[Slot] && Slot < ThreadCount.load(std::memory_order_acquire))
			{
				const u32 Id = Threads[Slot].Id.load(std::memory_order_acquire);
				Handles[Slot] = Id ? OpenThread(THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT | THREAD_QUERY_INFORMATION, FALSE, Id) : nullptr;
			}

			if (Handles[Slot] && SuspendThread(Handles[Slot]) != DWORD(-1))
			{
				Item.Depth = WalkStopped(Handles[Slot], Item.Addresses);
				ResumeThread(Handles[Slot]);
			}
		}

		Count += Samples[0][Count].Depth ? 1 : 0;
	}

	for (const HANDLE Thread : Handles)
	{
		if (Thread)
		{
			CloseHandle(Thread);
		}
	}
}

void Write(std::stop_token Stop)
{
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	u32 Seen = 0;

	while (!Stop.stop_requested())
	{
		LateSignal.wait(Seen, std::memory_order_acquire);
		Seen = LateSignal.load(std::memory_order_acquire);
		xr_vector<Late> Taken;
		{
			xrCriticalSectionGuard Lock(LateGuard);
			Taken.swap(LateFrames);
		}

		for (const Late& Frame : Taken)
		{
			WriteReport(Frame);
		}
	}
}
} // namespace

void FrameStackSampler::Start(const char* Folder, float LimitMs)
{
	if (Watcher.joinable())
	{
		return;
	}

	ReportFolder = Folder;
	Limit = LimitMs;
	LimitNow.store(LimitMs, std::memory_order_release);
	Usual = 0.0f;

	for (Sample*& Item : Samples)
	{
		Item = xr_alloc<Sample>(MaxSamples);
	}

	timeBeginPeriod(1);
	FrameStart.store(CPU::QPC(), std::memory_order_release);
	Watcher = std::jthread(Watch);
	Writer = std::jthread(Write);
}

void FrameStackSampler::Stop()
{
	if (!Watcher.joinable())
	{
		return;
	}

	Watcher.request_stop();
	Watcher.join();
	Writer.request_stop();
	LateSignal.fetch_add(1, std::memory_order_release);
	LateSignal.notify_one();
	Writer.join();
	LateFrames.clear();
	timeEndPeriod(1);

	for (Sample*& Item : Samples)
	{
		xr_free(Item);
	}
}

void FrameStackSampler::BeginFrame()
{
	// What a frame usually takes, a slow one left out: the limit follows it
	const u64 Now = CPU::QPC();
	const float Took = float(double(Now - FrameStart.load(std::memory_order_acquire)) * 1000.0 / double(CPU::qpc_freq));

	if (Usual == 0.0f || Took < Usual * 3.0f)
	{
		Usual = Usual == 0.0f ? Took : Usual + (Took - Usual) * 0.02f;
		LimitNow.store(std::max(Limit, Usual * 3.0f), std::memory_order_release);
	}

	Threads[0].Id.store(GetCurrentThreadId(), std::memory_order_release);
	FrameStart.store(Now, std::memory_order_release);
	FrameTook.store(Took, std::memory_order_release);
	FrameId.fetch_add(1, std::memory_order_release);
}

void FrameStackSampler::Follow(const char* Name)
{
	xrCriticalSectionGuard Lock(FollowGuard);
	const u32 Slot = ThreadCount.load(std::memory_order_acquire);

	if (Slot < MaxThreads)
	{
		xr_strcpy(Threads[Slot].Name, Name);
		Threads[Slot].Id.store(GetCurrentThreadId(), std::memory_order_release);
		ThreadCount.store(Slot + 1, std::memory_order_release);
	}
}
