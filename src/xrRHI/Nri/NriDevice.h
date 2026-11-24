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
#include "NriSubmitThread.h"

namespace nri
{
struct Device;
struct Queue;
struct SwapChain;
struct Fence;
struct CoreInterface;
struct Texture;
struct Buffer;
struct Descriptor;
struct DescriptorPool;
struct Pipeline;
struct PipelineLayout;
struct QueryPool;
struct CommandAllocator;
struct CommandBuffer;
struct TextureRegionDesc;
struct TextureDataLayoutDesc;
struct TextureUploadDesc;
struct BufferUploadDesc;
} // namespace nri

struct RHISwapChainDesc;

// Owns the graphics device, the queue and the frame: the frame's command buffers, its one fence and every submit.
// On D3D12 the frame's submit and present run on the submit thread; the rest of the queue's work waits for it to be idle.
// The native backends borrow the device, the queue and the frame's command buffer until they are ported.
class RHI_API NriDevice
{
public:
	explicit NriDevice(ERHI_API_LAYER Api);
	~NriDevice();

	NriDevice(const NriDevice&) = delete;
	NriDevice& operator=(const NriDevice&) = delete;

	bool HasBaseFeatures() const;
	void* NativeDevice() const;
	void* NativeQueue() const;
	const nri::CoreInterface& CoreApi() const;
	// Writes gpu_crash_<time>.txt to the logs folder; Reason is the backend's removal code
	void ReportLoss(u32 Reason, const char* Details = nullptr) const;

	// The window's swap chain: flip model, waitable where the device can, tearing without v-sync, as many frames
	// queued as Desc says. Made again for another size: no window means the one it had
	void CreateSwapChain(void* Window, const RHISwapChainDesc& Desc, u32 Textures);
	void DestroySwapChain();
	// The native texture of a back buffer, borrowed
	void* SwapChainTexture(u32 Index) const;
	// The back buffer the frame being recorded draws into: the flip model gives them in turn, one a present
	u32 AcquireTexture() const;

	// Waits until the swap chain takes another frame, then destroys what the GPU is done with: called before the
	// frame samples its input
	void BeginFrame();
	// The frame being recorded: the value its submit signals on the frame fence, one a frame, and the stamp of what is
	// retired or read back while it is recorded
	u64 FrameFence() const;
	u64 CompletedFence() const;
	void Wait(u64 Fence);

	// The frame's command buffer, begun: another follows it once it is chained or handed to the queue
	nri::CommandBuffer& FrameCommands();
	// The old D3D12 backend records into the frame's command buffer: OnClose is called before that buffer is closed
	void Borrow(void (*OnClose)(void*), void* Owner);
	// Buffers recorded elsewhere follow what the frame recorded so far, in this order, in the frame's submit
	void Chain(nri::CommandBuffer* const* Buffers, u32 Count);
	// What is recorded goes to the queue now, without the frame's signal: for a submitter of its own that runs after it
	void Flush();
	// Hands the frame's buffers in their order, its signal and its present to the submit thread; the next frame begins.
	// False once the device is lost
	bool EndFrame();
	// Returns once the frame handed over is submitted and presented
	void Idle();
	// Set by a submit or present that failed
	bool IsLost() const;
	// What is recorded goes to the queue and the GPU is waited for until idle; everything retired is destroyed. On the
	// recording thread, with no buffer recorded elsewhere still open
	void DrainAll();
	// NRI's upload helper: its copy goes to the queue after the frame handed over and is waited for
	bool UploadData(const nri::TextureUploadDesc* Textures, u32 TextureCount, const nri::BufferUploadDesc* Buffers, u32 BufferCount);

	// Destroyed once the GPU has run the frame being recorded: every command using the object is recorded before
	void Retire(nri::Texture* Object);
	void Retire(nri::Buffer* Object);
	void Retire(nri::Descriptor* Object);
	void Retire(nri::DescriptorPool* Object);
	void Retire(nri::Pipeline* Object);
	void Retire(nri::PipelineLayout* Object);
	void Retire(nri::QueryPool* Object);
	void Retire(nri::CommandBuffer* Object);
	void Retire(nri::CommandAllocator* Object);

	// A command buffer of the ring, begun; taken again once the GPU has run the frame that carried it
	nri::CommandBuffer& BeginCommands();

	// The copy is recorded into Commands of the frame being recorded; the ticket is 0 when no buffer could be made
	u32 ReadbackBuffer(nri::CommandBuffer& Commands, nri::Buffer& Source, u64 Offset, u64 Size);
	u32 ReadbackTexture(nri::CommandBuffer& Commands, nri::Texture& Source, const nri::TextureRegionDesc& Region, const nri::TextureDataLayoutDesc& Layout, u64 Size);
	// Null until the GPU has run the copy, never waiting for it; valid until the ticket is released
	const void* ReadbackData(u32 Ticket);
	void ReleaseReadback(u32 Ticket);

	nri::Device* Device = nullptr;
	nri::Queue* Queue = nullptr;

private:
	struct RetiredObject
	{
		void* Object;
		void (*Destroy)(void*);
		u64 Fence;
	};

	struct CommandSlot
	{
		nri::CommandAllocator* Allocator;
		nri::CommandBuffer* Buffer;
		u64 Fence;
	};

	struct ReadbackSlot
	{
		nri::Buffer* Buffer;
		u64 Size;
		u64 Fence;
		void* Data;
		bool IsTaken;
	};

	void WaitForPresent();
	void Push(void* Object, void (*Destroy)(void*));
	void DrainRetired(u64 Completed);
	nri::Buffer* TakeReadback(u64 Size, u32& Ticket);
	nri::CommandBuffer& TakeCommands();
	void Close();
	void Stamp(u64 Done);
	void SubmitChained();

	nri::SwapChain* SwapChain = nullptr;
	nri::Fence* SwapFence = nullptr;
	void* SwapWindow = nullptr;
	u64 PresentId = 0;
	u32 BackTexture = 0;
	u32 TextureCount = 1;
	bool HasPresented = false;
	NriSubmitThread Submitter;

	void (*CloseBorrowed)(void*) = nullptr;
	void* Borrower = nullptr;

	mutable xrCriticalSection Lock;
	nri::Fence* SubmitFence = nullptr;
	std::atomic<u64> Recording = 1;
	nri::CommandBuffer* Open = nullptr;
	xr_vector<nri::CommandBuffer*> Chained;
	xr_vector<RetiredObject> Retired;
	xr_vector<CommandSlot> CommandRing;
	xr_vector<ReadbackSlot> Readbacks;
};
