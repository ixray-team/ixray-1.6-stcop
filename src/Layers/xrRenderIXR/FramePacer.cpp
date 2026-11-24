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
#include "stdafx.h"
#include "FramePacer.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

void FramePacer::Hold()
{
	constexpr float Margin = 300000.0f;
	constexpr float Longest = 8000000.0f;
	const float Left = Handed ? Tail - float(SDL_GetTicksNS() - Handed) - Move - Margin : 0.0f;

	if (Left > 0.0f)
	{
		PROF_EVENT("FramePacer");
		SDL_DelayPrecise(u64(std::min(Left, Longest)));
	}

	Polled = SDL_GetTicksNS();
}

void FramePacer::HandOver()
{
	Handed = SDL_GetTicksNS();
}

void FramePacer::Land()
{
	Landed = SDL_GetTicksNS();
}

void FramePacer::Meet(u64 Entry)
{
	if (!GRHI->Nri)
	{
		return;
	}

	PROF_EVENT("FramePacer");
	const u64 Frame = GRHI->Nri->FrameFence();
	const u64 Queue = u64(std::clamp(psFrameQueue, 1, 3));
	const u64 Before = SDL_GetTicksNS();

	if (Frame > Queue)
	{
		GRHI->Nri->Wait(Frame - Queue);
	}

	const u64 After = SDL_GetTicksNS();
	Collect();
	InputTimes[Frame % SlotCount] = Polled ? Polled : After;

	if (!Handed || !Polled || Entry < Polled || Landed < Handed)
	{
		return;
	}

	// The frame in flight: the render thread's time on it, as that thread marked its end, and what the GPU's frames
	// then kept this thread waiting
	constexpr float Rate = 0.05f;
	// A frame that took long must not hold the ones after it: the frame in flight is taken as short as it has just
	// been and the game's frame as long, and each drifts the other way slowly
	const float Flight = float(Landed - Handed) + float(After - Before);
	const float Game = float(Entry - Polled);
	Tail = Flight < Tail ? Flight : Tail + (Flight - Tail) * Rate;
	Move = Game > Move ? Game : Move + (Game - Move) * Rate;
}

void FramePacer::Wait()
{
	if (!GRHI->Nri)
	{
		return;
	}

	if (RenderThread::IsPipelined())
	{
		Hold();
		return;
	}

	PROF_EVENT("FramePacer");
	const u64 Frame = GRHI->Nri->FrameFence();
	const u64 Queue = u64(std::clamp(psFrameQueue, 1, 3));

	if (Frame > Queue)
	{
		GRHI->Nri->Wait(Frame - Queue);
	}

	Collect();
	InputTimes[Frame % SlotCount] = SDL_GetTicksNS();
}

void FramePacer::Collect()
{
	if (!GRHI->Nri)
	{
		return;
	}

	const u64 Now = SDL_GetTicksNS();
	const u64 Frame = GRHI->Nri->FrameFence();
	const u64 Done = std::min(GRHI->Nri->CompletedFence(), Frame - 1);

	// Frames older than the ring were never looked at (a loop that never waits, the editors): their slots hold newer ones
	Seen = std::max(Seen, Frame > SlotCount ? Frame - SlotCount - 1 : 0);

	while (Seen < Done)
	{
		u64& Input = InputTimes[++Seen % SlotCount];

		if (Input)
		{
			Latencies[LatencyCount++ % WindowSize] = float(Now - Input) * 1e-6f;
			Input = 0;
		}
	}
}

void FramePacer::Destroy()
{
	*this = FramePacer();
}

void FramePacer::LogLatency() const
{
	const u32 Count = std::min(LatencyCount, WindowSize);

	if (!Count)
	{
		Msg("* Frame latency: not measured");
		return;
	}

	float Sum = 0.f;
	float Worst = 0.f;

	for (u32 Index = 0; Index < Count; ++Index)
	{
		Sum += Latencies[Index];
		Worst = std::max(Worst, Latencies[Index]);
	}

	Msg("* Frame latency: %.2f ms mean, %.2f ms worst over %u frames, from the input to the end on the GPU, frame queue %d", Sum / float(Count), Worst, Count, psFrameQueue);
}
