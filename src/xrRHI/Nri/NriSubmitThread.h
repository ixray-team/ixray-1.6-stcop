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

namespace nri
{
struct Queue;
struct Fence;
struct SwapChain;
struct CommandBuffer;
struct CoreInterface;
struct SwapChainInterface;
} // namespace nri

// A frame as the queue takes it: its command buffers in order, the frame fence's signal, then the present
struct NriPacket
{
	xr_vector<nri::CommandBuffer*> Buffers;
	nri::Fence* Fence = nullptr;
	u64 Value = 0;
	nri::SwapChain* SwapChain = nullptr;
	nri::Fence* SwapFence = nullptr;
	u64 PresentId = 0;
	// The back buffer the swap chain gives after the present
	u32 Texture = 0;
};

// Submits and presents the frame handed over, on a thread of its own, or in place where the queue replays on the
// context the frame is recorded with (D3D11) or r_submit_thread is off. One packet at a time: it is filled only while
// the thread is idle
class NriSubmitThread
{
public:
	NriSubmitThread() = default;

	NriSubmitThread(const NriSubmitThread&) = delete;
	NriSubmitThread& operator=(const NriSubmitThread&) = delete;

	void Start(nri::Queue& Queue, const nri::CoreInterface& Core, const nri::SwapChainInterface& Swap, bool IsThreaded);
	// Returns once the packet handed over is submitted and presented
	void Idle();
	void Run(bool IsThreaded);
	bool IsThreaded() const;
	bool IsLost() const;

	NriPacket Packet;

private:
	void Execute();

	nri::Queue* Queue = nullptr;
	const nri::CoreInterface* Core = nullptr;
	const nri::SwapChainInterface* Swap = nullptr;
	std::atomic<bool> IsDeviceLost = false;
	bool IsQueued = false;
	xr_unique_ptr<XRayWorkerThread> Worker;
};
