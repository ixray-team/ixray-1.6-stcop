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
#include "NriDevice.h"

#include <ctime>

#include <NRI.h>
#include <Extensions/NRIDeviceCreation.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRISwapChain.h>

static nri::CoreInterface NriCore;
static nri::HelperInterface NriHelp;
static nri::SwapChainInterface NriSwap;
static IWriter* LossFile = nullptr;

static void NRI_CALL NriMessage(nri::Message Type, const char*, u32, const char* Text, void*)
{
	static const char Prefix[] = { '*', '~', '!' };
	Msg("%c NRI: %s", Prefix[u32(Type)], Text);

	if (LossFile)
	{
		LossFile->w_string(Text);
	}

	if (Type == nri::Message::ERROR)
	{
		xrLogger::FlushLog();
	}
}

template <class T, auto Destroy>
static void DestroyAs(void* Object)
{
	(NriCore.*Destroy)(static_cast<T*>(Object));
}

NriDevice::NriDevice(ERHI_API_LAYER Api)
{
	nri::DeviceCreationDesc Desc = {};
	Desc.graphicsAPI = Api == ERHI_API_LAYER::D3D12 ? nri::GraphicsAPI::D3D12 : nri::GraphicsAPI::D3D11;
	Desc.callbackInterface.MessageCallback = NriMessage;
	Desc.enableGraphicsAPIValidation = Core.ParamsData.test(ECoreParams::dxdebug);
	// NRI validation stays off: it rejects NriPass's layout, where root descriptors and the texture
	// set share space 0 because the DXBC shaders have no register spaces
	const bool IsDumping = Desc.enableGraphicsAPIValidation || Core.ParamsData.test(ECoreParams::gpu_dump);
	Desc.deviceLostInfoLevel = IsDumping ? nri::DeviceLostInfoLevel::VERBOSE : nri::DeviceLostInfoLevel::NONE;
	// The borrowing D3D12 backend still issues legacy barriers on this device
	Desc.disableD3D12EnhancedBarriers = true;

	nri::Result Result = nri::nriCreateDevice(Desc, Device);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "Unable to create a graphics device. Please update the video driver.");
	Result = nri::nriGetInterface(*Device, NRI_INTERFACE(nri::CoreInterface), &NriCore);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The NRI core interface is unavailable.");
	Result = nri::nriGetInterface(*Device, NRI_INTERFACE(nri::HelperInterface), &NriHelp);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The NRI helper interface is unavailable.");
	Result = NriCore.GetQueue(*Device, nri::QueueType::GRAPHICS, 0, Queue);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The graphics device has no graphics queue.");
	Result = NriCore.CreateFence(*Device, { 0 }, SubmitFence);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "Unable to create the frame fence.");
	NriCore.SetDebugName(SubmitFence, "NriDevice frame fence");
	// D3D11's queue replays on the immediate context the frame is recorded with: it submits in place
	Submitter.Start(*Queue, NriCore, NriSwap, Api == ERHI_API_LAYER::D3D12);

	Msg("* NRI %u adapter: %s", u32(NRI_VERSION), NriCore.GetDeviceDesc(*Device).adapterDesc.name);
}

NriDevice::~NriDevice()
{
	DestroySwapChain();

	for (const CommandSlot& Slot : CommandRing)
	{
		NriCore.DestroyCommandBuffer(Slot.Buffer);
		NriCore.DestroyCommandAllocator(Slot.Allocator);
	}

	for (const ReadbackSlot& Slot : Readbacks)
	{
		NriCore.DestroyBuffer(Slot.Buffer);
	}

	NriCore.DestroyFence(SubmitFence);
	nri::nriDestroyDevice(Device);
}

void NriDevice::CreateSwapChain(void* Window, const RHISwapChainDesc& Made, u32 Textures)
{
	DestroySwapChain();
	SwapWindow = Window ? Window : SwapWindow;

	nri::Result Result = nri::nriGetInterface(*Device, NRI_INTERFACE(nri::SwapChainInterface), &NriSwap);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "The NRI swap chain interface is unavailable.");

	nri::SwapChainDesc Desc = {};
	Desc.window.windows.hwnd = SwapWindow;
	Desc.queue = Queue;
	Desc.width = nri::Dim_t(Made.Width);
	Desc.height = nri::Dim_t(Made.Height);
	Desc.textureNum = u8(Textures);
	Desc.format = nri::SwapChainFormat::BT709_G22_8BIT;
	Desc.flags = Made.IsVSync ? nri::SwapChainBits::VSYNC : nri::SwapChainBits::ALLOW_TEARING;
	Desc.flags |= NriCore.GetDeviceDesc(*Device).features.waitableSwapChain ? nri::SwapChainBits::WAITABLE : nri::SwapChainBits::NONE;
	Desc.queuedFrameNum = u8(std::clamp(Made.FrameQueue, 1u, 3u));
	Result = NriSwap.CreateSwapChain(*Device, Desc, SwapChain);
	CHECK_OR_EXIT(Result == nri::Result::SUCCESS, "Unable to create the swap chain.");

	HasPresented = false;
	const bool IsWaitable = NriSwap.WaitForPresent(*SwapChain, 0) != nri::Result::UNSUPPORTED;
	Msg("* Swap chain %ux%u: %u textures, %u frames queued, %s, %s", Made.Width, Made.Height, Textures, u32(Desc.queuedFrameNum), Made.IsVSync ? "vsync" : "tearing allowed", IsWaitable ? "waitable" : "not waitable");

	if (!SwapFence)
	{
		NriCore.CreateFence(*Device, { nri::SWAPCHAIN_SEMAPHORE }, SwapFence);
	}

	// The submit thread is idle: the drain above waited for it
	NriSwap.GetSwapChainTextures(*SwapChain, TextureCount);
	NriSwap.AcquireNextTexture(*SwapChain, *SwapFence, BackTexture);
	Submitter.Packet.Texture = BackTexture;
}

void NriDevice::DestroySwapChain()
{
	DrainAll();

	if (SwapChain)
	{
		NriSwap.DestroySwapChain(SwapChain);
		SwapChain = nullptr;
	}
}

void* NriDevice::SwapChainTexture(u32 Index) const
{
	u32 Count = 0;
	nri::Texture* const* Textures = NriSwap.GetSwapChainTextures(*SwapChain, Count);
	R_ASSERT(Index < Count);
	return reinterpret_cast<void*>(NriCore.GetTextureNativeObject(Textures[Index]));
}

u32 NriDevice::AcquireTexture() const
{
	return BackTexture;
}

void NriDevice::WaitForPresent()
{
	// A swap chain that has shown nothing yet is not waited for, at start or made again: the wait taken then is
	// never given back, and every frame after it waits for the one before to reach the screen
	if (!SwapChain || !HasPresented)
	{
		return;
	}

	// D3D waits on the swap chain's latency count, whatever the id: a present still on the submit thread is counted
	// once it is made, and the wait needs only presents made a frame before. The wait is alertable: a completion
	// routine queued to this thread ends it at once, and it is taken again
	for (u32 Try = 0; Try < 3 && NriSwap.WaitForPresent(*SwapChain, PresentId) == nri::Result::FAILURE; ++Try)
	{
	}
}

void NriDevice::BeginFrame()
{
	WaitForPresent();
	DrainRetired(CompletedFence());
}

u64 NriDevice::FrameFence() const
{
	return Recording.load(std::memory_order_acquire);
}

u64 NriDevice::CompletedFence() const
{
	return NriCore.GetFenceValue(*SubmitFence);
}

void NriDevice::Wait(u64 Fence)
{
	NriCore.Wait(*SubmitFence, Fence);
}

nri::CommandBuffer& NriDevice::FrameCommands()
{
	xrCriticalSectionGuard Guard(Lock);

	if (!Open)
	{
		Open = &TakeCommands();
	}

	return *Open;
}

void NriDevice::Borrow(void (*OnClose)(void*), void* Owner)
{
	CloseBorrowed = OnClose;
	Borrower = Owner;
}

void NriDevice::Close()
{
	if (CloseBorrowed)
	{
		CloseBorrowed(Borrower);
	}

	xrCriticalSectionGuard Guard(Lock);

	if (Open)
	{
		NriCore.EndCommandBuffer(*Open);
		Chained.push_back(Open);
		Open = nullptr;
	}
}

// The ring's slots carried by the chained buffers are taken again once the GPU has run frame Done
void NriDevice::Stamp(u64 Done)
{
	for (CommandSlot& Slot : CommandRing)
	{
		if (std::find(Chained.begin(), Chained.end(), Slot.Buffer) != Chained.end())
		{
			Slot.Fence = Done;
		}
	}
}

// Without the frame's signal, with the submit thread idle: done once the frame being recorded is
void NriDevice::SubmitChained()
{
	if (Chained.empty())
	{
		return;
	}

	Stamp(FrameFence());
	nri::QueueSubmitDesc Desc = {};
	Desc.commandBuffers = Chained.data();
	Desc.commandBufferNum = u32(Chained.size());
	NriCore.QueueSubmit(*Queue, Desc);
	Chained.clear();
}

void NriDevice::Chain(nri::CommandBuffer* const* Buffers, u32 Count)
{
	Close();
	xrCriticalSectionGuard Guard(Lock);
	Chained.insert(Chained.end(), Buffers, Buffers + Count);
}

void NriDevice::Flush()
{
	Close();
	xrCriticalSectionGuard Guard(Lock);
	Idle();
	SubmitChained();
}

bool NriDevice::EndFrame()
{
	PROF_EVENT("NriEndFrame");
	Close();
	Idle();

	NriPacket& Packet = Submitter.Packet;

	if (Packet.Texture != BackTexture)
	{
		Msg("! NRI: the swap chain gave back buffer %u, the frame drew into %u", Packet.Texture, BackTexture);
		BackTexture = Packet.Texture;
	}

	{
		xrCriticalSectionGuard Guard(Lock);
		const u64 Frame = FrameFence();
		Stamp(Frame);
		Packet.Buffers.clear();
		Packet.Buffers.swap(Chained);
		Packet.Fence = SubmitFence;
		Packet.Value = Frame;
		Packet.SwapChain = SwapChain;
		Packet.SwapFence = SwapFence;
		Packet.PresentId = ++PresentId;
		Recording.store(Frame + 1, std::memory_order_release);
	}

	Submitter.Run(psSubmitThread != 0);
	HasPresented = true;
	BackTexture = Submitter.IsThreaded() ? (BackTexture + 1) % TextureCount : Packet.Texture;
	return !Submitter.IsLost();
}

void NriDevice::Idle()
{
	Submitter.Idle();
}

bool NriDevice::IsLost() const
{
	return Submitter.IsLost();
}

void NriDevice::DrainAll()
{
	PROF_EVENT("NriDrainAll");
	Close();
	xrCriticalSectionGuard Guard(Lock);
	Idle();
	SubmitChained();
	NriCore.QueueWaitIdle(Queue);

	for (CommandSlot& Slot : CommandRing)
	{
		Slot.Fence = Slot.Fence == u64(-1) ? Slot.Fence : 0;
	}

	DrainRetired(u64(-1));
}

bool NriDevice::UploadData(const nri::TextureUploadDesc* Textures, u32 TextureCount, const nri::BufferUploadDesc* Buffers, u32 BufferCount)
{
	xrCriticalSectionGuard Guard(Lock);
	Idle();
	return NriHelp.UploadData(*Queue, Textures, TextureCount, Buffers, BufferCount) == nri::Result::SUCCESS;
}

void NriDevice::Push(void* Object, void (*Destroy)(void*))
{
	if (Object)
	{
		xrCriticalSectionGuard Guard(Lock);
		Retired.push_back({ Object, Destroy, FrameFence() });
	}
}

void NriDevice::Retire(nri::Texture* Object)
{
	Push(Object, DestroyAs<nri::Texture, &nri::CoreInterface::DestroyTexture>);
}

void NriDevice::Retire(nri::Buffer* Object)
{
	Push(Object, DestroyAs<nri::Buffer, &nri::CoreInterface::DestroyBuffer>);
}

void NriDevice::Retire(nri::Descriptor* Object)
{
	Push(Object, DestroyAs<nri::Descriptor, &nri::CoreInterface::DestroyDescriptor>);
}

void NriDevice::Retire(nri::DescriptorPool* Object)
{
	Push(Object, DestroyAs<nri::DescriptorPool, &nri::CoreInterface::DestroyDescriptorPool>);
}

void NriDevice::Retire(nri::Pipeline* Object)
{
	Push(Object, DestroyAs<nri::Pipeline, &nri::CoreInterface::DestroyPipeline>);
}

void NriDevice::Retire(nri::PipelineLayout* Object)
{
	Push(Object, DestroyAs<nri::PipelineLayout, &nri::CoreInterface::DestroyPipelineLayout>);
}

void NriDevice::Retire(nri::QueryPool* Object)
{
	Push(Object, DestroyAs<nri::QueryPool, &nri::CoreInterface::DestroyQueryPool>);
}

void NriDevice::Retire(nri::CommandBuffer* Object)
{
	Push(Object, DestroyAs<nri::CommandBuffer, &nri::CoreInterface::DestroyCommandBuffer>);
}

void NriDevice::Retire(nri::CommandAllocator* Object)
{
	Push(Object, DestroyAs<nri::CommandAllocator, &nri::CoreInterface::DestroyCommandAllocator>);
}

void NriDevice::DrainRetired(u64 Completed)
{
	xrCriticalSectionGuard Guard(Lock);
	size_t Count = 0;

	for (; Count < Retired.size() && Retired[Count].Fence <= Completed; ++Count)
	{
		Retired[Count].Destroy(Retired[Count].Object);
	}

	Retired.erase(Retired.begin(), Retired.begin() + Count);
}

nri::CommandBuffer& NriDevice::BeginCommands()
{
	xrCriticalSectionGuard Guard(Lock);
	return TakeCommands();
}

nri::CommandBuffer& NriDevice::TakeCommands()
{
	const u64 Completed = CompletedFence();
	auto Slot = std::find_if(CommandRing.begin(), CommandRing.end(), [&](const CommandSlot& Item) { return Item.Fence <= Completed; });

	if (Slot == CommandRing.end())
	{
		CommandSlot Made = {};
		CHECK_OR_EXIT(NriCore.CreateCommandAllocator(*Queue, Made.Allocator) == nri::Result::SUCCESS && NriCore.CreateCommandBuffer(*Made.Allocator, Made.Buffer) == nri::Result::SUCCESS, "Unable to create a command buffer.");
		NriCore.SetDebugName(Made.Buffer, "NriDevice commands");
		CommandRing.push_back(Made);
		Slot = CommandRing.end() - 1;
	}

	// Taken until a submit carries it
	Slot->Fence = u64(-1);
	NriCore.ResetCommandAllocator(*Slot->Allocator);
	CHECK_OR_EXIT(NriCore.BeginCommandBuffer(*Slot->Buffer, nullptr) == nri::Result::SUCCESS, "Unable to begin a command buffer.");
	return *Slot->Buffer;
}

nri::Buffer* NriDevice::TakeReadback(u64 Size, u32& Ticket)
{
	xrCriticalSectionGuard Guard(Lock);
	auto Slot = std::find_if(Readbacks.begin(), Readbacks.end(), [&](const ReadbackSlot& Item) { return !Item.IsTaken && Item.Size >= Size; });

	if (Slot == Readbacks.end())
	{
		nri::BufferDesc Desc = {};
		Desc.size = Size;
		nri::Buffer* Buffer = nullptr;

		if (NriCore.CreateCommittedBuffer(*Device, nri::MemoryLocation::HOST_READBACK, 0.0f, Desc, Buffer) != nri::Result::SUCCESS)
		{
			Ticket = 0;
			return nullptr;
		}

		NriCore.SetDebugName(Buffer, "NriDevice readback");
		Readbacks.push_back({ Buffer, Size });
		Slot = Readbacks.end() - 1;
	}

	Slot->Fence = FrameFence();
	Slot->Data = nullptr;
	Slot->IsTaken = true;
	Ticket = u32(Slot - Readbacks.begin()) + 1;
	return Slot->Buffer;
}

u32 NriDevice::ReadbackBuffer(nri::CommandBuffer& Commands, nri::Buffer& Source, u64 Offset, u64 Size)
{
	u32 Ticket = 0;

	if (nri::Buffer* Target = TakeReadback(Size, Ticket))
	{
		NriCore.CmdCopyBuffer(Commands, *Target, 0, Source, Offset, Size);
	}

	return Ticket;
}

u32 NriDevice::ReadbackTexture(nri::CommandBuffer& Commands, nri::Texture& Source, const nri::TextureRegionDesc& Region, const nri::TextureDataLayoutDesc& Layout, u64 Size)
{
	u32 Ticket = 0;

	if (nri::Buffer* Target = TakeReadback(Size, Ticket))
	{
		NriCore.CmdReadbackTextureToBuffer(Commands, *Target, Layout, Source, Region);
	}

	return Ticket;
}

const void* NriDevice::ReadbackData(u32 Ticket)
{
	if (!Ticket)
	{
		return nullptr;
	}

	xrCriticalSectionGuard Guard(Lock);
	ReadbackSlot& Slot = Readbacks[Ticket - 1];

	// D3D11 maps a read-back buffer with a wait for the GPU, and copies a texture's rows out of its staging copy there
	if (!Slot.Data && CompletedFence() >= Slot.Fence)
	{
		Slot.Data = NriCore.MapBuffer(*Slot.Buffer, 0, Slot.Size);
	}

	return Slot.Data;
}

void NriDevice::ReleaseReadback(u32 Ticket)
{
	if (!Ticket)
	{
		return;
	}

	xrCriticalSectionGuard Guard(Lock);
	ReadbackSlot& Slot = Readbacks[Ticket - 1];

	if (Slot.Data)
	{
		NriCore.UnmapBuffer(*Slot.Buffer);
	}

	Slot.Data = nullptr;
	Slot.IsTaken = false;
}

bool NriDevice::HasBaseFeatures() const
{
	const nri::DeviceDesc& Desc = NriCore.GetDeviceDesc(*Device);

	return Desc.graphicsAPI != nri::GraphicsAPI::D3D12 || (Desc.tiers.bindless >= 1 && Desc.tiers.resourceBinding >= 2);
}

void* NriDevice::NativeDevice() const
{
	return NriCore.GetDeviceNativeObject(Device);
}

void* NriDevice::NativeQueue() const
{
	return NriCore.GetQueueNativeObject(Queue);
}

const nri::CoreInterface& NriDevice::CoreApi() const
{
	return NriCore;
}

void NriDevice::ReportLoss(u32 Reason, const char* Details) const
{
	static bool IsReported = false;
	if (IsReported)
	{
		return;
	}

	IsReported = true;
	const unsigned long long Time = static_cast<unsigned long long>(std::time(nullptr));
	string_path Name;
	xr_sprintf(Name, "gpu_crash_%llu.txt", Time);
	LossFile = FS.w_open("$logs$", Name);
	Msg("! GPU device lost: 0x%08x, details in %s", Reason, Name);

	if (LossFile)
	{
		LossFile->w_printf("Device removed reason: 0x%08x\r\nAdapter: %s\r\n", Reason, NriCore.GetDeviceDesc(*Device).adapterDesc.name);

		if (!Core.ParamsData.test(ECoreParams::gpu_dump) && !Core.ParamsData.test(ECoreParams::dxdebug))
		{
			LossFile->w_string("Run with -gpu_dump to record what the GPU was doing.");
		}

		if (Details)
		{
			LossFile->w_string(Details);
		}
	}

	nri::DeviceLostDump Dump = {};
	nri::nriReportDeviceLostInfo(*Device, Dump);

	if (LossFile)
	{
		FS.w_close(LossFile);
	}

	if (Dump.data && Dump.size)
	{
		xr_sprintf(Name, "gpu_crash_%llu.bin", Time);
		if (IWriter* File = FS.w_open("$logs$", Name))
		{
			File->w(Dump.data, u32(Dump.size));
			FS.w_close(File);
		}
	}

	xrLogger::FlushLog();
}
