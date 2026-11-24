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
#include "../RHI.h"
#include "NriSubmitThread.h"

#include <NRI.h>
#include <Extensions/NRISwapChain.h>

void NriSubmitThread::Start(nri::Queue& InQueue, const nri::CoreInterface& InCore, const nri::SwapChainInterface& InSwap, bool IsThreaded)
{
	Queue = &InQueue;
	Core = &InCore;
	Swap = &InSwap;

	if (IsThreaded)
	{
		Worker = xr_make_unique<XRayWorkerThread>([this] { Execute(); }, "X-Ray Submit Thread", EThreadRole::System);
	}
}

void NriSubmitThread::Idle()
{
	if (Worker)
	{
		PROF_EVENT("SubmitWait");
		Worker->Wait();
	}
}

void NriSubmitThread::Run(bool IsThreaded)
{
	IsQueued = Worker && IsThreaded;

	if (IsQueued)
	{
		Worker->Run();
		return;
	}

	Execute();
}

bool NriSubmitThread::IsThreaded() const
{
	return IsQueued;
}

bool NriSubmitThread::IsLost() const
{
	return IsDeviceLost.load(std::memory_order_acquire);
}

void NriSubmitThread::Execute()
{
	PROF_EVENT("SubmitThread");
	bool IsDone = false;

	{
		PROF_EVENT("QueueSubmit");
		const nri::FenceSubmitDesc Signal = { Packet.Fence, Packet.Value };
		nri::QueueSubmitDesc Desc = {};
		Desc.commandBuffers = Packet.Buffers.data();
		Desc.commandBufferNum = u32(Packet.Buffers.size());
		Desc.signalFences = &Signal;
		Desc.signalFenceNum = 1;
		IsDone = Core->QueueSubmit(*Queue, Desc) == nri::Result::SUCCESS;
	}

	if (IsDone)
	{
		PROF_EVENT("QueuePresent");
		IsDone = Swap->QueuePresent(*Packet.SwapChain, *Packet.SwapFence, Packet.PresentId) == nri::Result::SUCCESS;
		IsDone = IsDone && Swap->AcquireNextTexture(*Packet.SwapChain, *Packet.SwapFence, Packet.Texture) == nri::Result::SUCCESS;
	}

	if (!IsDone)
	{
		IsDeviceLost.store(true, std::memory_order_release);
	}
}
