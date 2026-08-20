#include "TiramisuRenderViewport.h"

TiramisuRenderViewport::TiramisuRenderViewport() = default;

TiramisuRenderViewport::~TiramisuRenderViewport()
{
	Destroy();
}

void TiramisuRenderViewport::CreateOrReset(SDL_Window* InWindows, u32 InWidth, u32 InHeight, bool InVSync)
{
	auto LambdaCreateOrReset = [this, InWindows, InWidth, InHeight, InVSync]()
	{
		CheckIsRenderThread();
		VERIFY(bRenderer == false);
		if (SwapChain)
		{
			Destroy();
		}
		{
			bVSync = InVSync;
			Width = InWidth;
			Height = InHeight;
			NativeWindowHandle = SDL_GetPointerProperty(
				SDL_GetWindowProperties(InWindows),
				SDL_PROP_WINDOW_WIN32_HWND_POINTER,
				nullptr
			);

			nri::SwapChainDesc SwapChainDescription = {};
			SwapChainDescription.window.windows.hwnd =
				static_cast<HWND>(NativeWindowHandle);
			SwapChainDescription.queue = GRenderDevice.GraphicsQueue;
			SwapChainDescription.format = nri::SwapChainFormat::BT709_G22_8BIT;
			SwapChainDescription.flags = (bVSync ? nri::SwapChainBits::VSYNC : nri::SwapChainBits::NONE) | nri::SwapChainBits::ALLOW_TEARING;
			SwapChainDescription.width = static_cast<nri::Dim_t>(Width);
			SwapChainDescription.height = static_cast<nri::Dim_t>(Height);
			SwapChainDescription.textureNum = GetOptimalTextureNum();
			SwapChainDescription.queuedFrameNum = GetQueuedFrameNum();
			NRI_CHECK(GRenderDevice.SwapChainInterface.CreateSwapChain(*GRenderDevice.Device, SwapChainDescription, SwapChain));

			u32 SwapChainTextureNum;
			nri::Texture* const* InSwapChainTextures = GRenderDevice.SwapChainInterface.GetSwapChainTextures(*SwapChain, SwapChainTextureNum);

			SwapChainFormat = GRenderDevice.CoreInterface.GetTextureDesc(*InSwapChainTextures[0]).format;

			for (u32 i = 0; i < SwapChainTextureNum; i++)
			{
				nri::TextureViewDesc TextureViewDescription = {InSwapChainTextures[i], nri::TextureView::COLOR_ATTACHMENT, SwapChainFormat};

				nri::Descriptor* ColorAttachment = nullptr;
				NRI_CHECK(GRenderDevice.CoreInterface.CreateTextureView(TextureViewDescription, ColorAttachment));

				nri::Fence* AcquireSemaphore = nullptr;
				NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, AcquireSemaphore));

				nri::Fence* ReleaseSemaphore = nullptr;
				NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, ReleaseSemaphore));

				FSwapChainTexture& NewSwapChainTexture = SwapChainTextures.emplace_back();
				NewSwapChainTexture.AcquireSemaphore = AcquireSemaphore;
				NewSwapChainTexture.ReleaseSemaphore = ReleaseSemaphore;
				NewSwapChainTexture.Texture = InSwapChainTextures[i];
				NewSwapChainTexture.ColorAttachment = ColorAttachment;
				NewSwapChainTexture.AttachmentFormat = SwapChainFormat;
			}
		}
		NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, 0, FrameFence));

		QueuedFrames.resize(GetQueuedFrameNum());
		for (FQueuedFrame& QueuedFrame : QueuedFrames)
		{
			NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandAllocator(*GRenderDevice.GraphicsQueue, QueuedFrame.CommandAllocator));
			NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandBuffer(*QueuedFrame.CommandAllocator, QueuedFrame.CommandBuffer));
		}
	};

	if (Platform::GetCurrentThreadId() ==
		GRenderThreadId.load(std::memory_order_acquire))
	{
		LambdaCreateOrReset();
	}
	else
	{
		ENQUEUE_RENDER_COMMAND(TiramisuRenderViewport::CreateOrReset)(LambdaCreateOrReset);
	}
}

void TiramisuRenderViewport::Destroy()
{
	CheckIsRenderThread();

	GRenderDevice.CoreInterface.QueueWaitIdle(GRenderDevice.GraphicsQueue);

	Width = 0;
	Height = 0;
	NativeWindowHandle = nullptr;
	SwapChainFormat = nri::Format::UNKNOWN;
	for (FSwapChainTexture& SwapChainTexture : SwapChainTextures)
	{
		GRenderDevice.CoreInterface.DestroyFence(SwapChainTexture.AcquireSemaphore);
		GRenderDevice.CoreInterface.DestroyFence(SwapChainTexture.ReleaseSemaphore);
		GRenderDevice.CoreInterface.DestroyDescriptor(SwapChainTexture.ColorAttachment);
	}
	SwapChainTextures.clear();

	if (SwapChain)
	{
		GRenderDevice.SwapChainInterface.DestroySwapChain(SwapChain);
		SwapChain = nullptr;
	}


	for (FQueuedFrame& QueuedFrame : QueuedFrames)
	{
		GRenderDevice.CoreInterface.DestroyCommandBuffer(QueuedFrame.CommandBuffer);
		GRenderDevice.CoreInterface.DestroyCommandAllocator(QueuedFrame.CommandAllocator);
	}
	QueuedFrames.clear();

	if (FrameFence)
	{
		GRenderDevice.CoreInterface.DestroyFence(FrameFence);
		FrameFence = nullptr;
	}
	FrameIndex = 0;
}


nri::CommandBuffer& TiramisuRenderViewport::GetCurrentCommandBuffer()
{
	CheckIsRenderThread();
	VERIFY(bRenderer == true);
	VERIFY(QueuedFrames.size() > 0);
	return *QueuedFrames[FrameIndex % QueuedFrames.size()].CommandBuffer;
}


void TiramisuRenderViewport::BeginRender(nri::DescriptorPool* DescriptionPool)
{
	CheckIsRenderThread();

	VERIFY(bRenderer == false);
	VERIFY(SwapChain);

	bRenderer = true;

	u32 QueuedFrameIndex = FrameIndex % QueuedFrames.size();
	const FQueuedFrame& QueuedFrame = QueuedFrames[QueuedFrameIndex];

	GRenderDevice.CoreInterface.Wait(*FrameFence, FrameIndex >= QueuedFrames.size() ? 1 + FrameIndex - QueuedFrames.size() : 0);
	GRenderDevice.CoreInterface.ResetCommandAllocator(*QueuedFrame.CommandAllocator);

	u32 RecycledSemaphoreIndex = FrameIndex % static_cast<u32>(SwapChainTextures.size());
	GRenderDevice.SwapChainInterface.AcquireNextTexture(*SwapChain, *SwapChainTextures[RecycledSemaphoreIndex].AcquireSemaphore, CurrentSwapChainTextureIndex);


	const FSwapChainTexture& SwapChainTexture = SwapChainTextures[CurrentSwapChainTextureIndex];

	nri::CommandBuffer& CurrentCommandBuffer = *QueuedFrame.CommandBuffer;
	GRenderDevice.CoreInterface.BeginCommandBuffer(CurrentCommandBuffer, DescriptionPool);

	nri::TextureBarrierDesc TextureBarriers = {};
	TextureBarriers.texture = SwapChainTexture.Texture;
	TextureBarriers.after = {nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT};
	TextureBarriers.layerNum = 1;
	TextureBarriers.mipNum = 1;

	nri::BarrierDesc BarrierDescription = {};
	BarrierDescription.textureNum = 1;
	BarrierDescription.textures = &TextureBarriers;
	GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer, BarrierDescription);

	nri::AttachmentDesc ColorAttachmentDescription = {};
	ColorAttachmentDescription.descriptor = SwapChainTexture.ColorAttachment;
	ColorAttachmentDescription.clearValue.color.f = {0.0f, 0.0f, 0.0f, 1.0f};
	ColorAttachmentDescription.loadOp = nri::LoadOp::CLEAR;

	nri::RenderingDesc RenderingDescription = {};
	RenderingDescription.colorNum = 1;
	RenderingDescription.colors = &ColorAttachmentDescription;

	GRenderDevice.CoreInterface.CmdBeginRendering(CurrentCommandBuffer, RenderingDescription);
	GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer, "SwapChain", nri::BGRA_UNUSED);
}

void TiramisuRenderViewport::EndRender(nri::Fence* WaitSemaphore, nri::Fence* SignalSemaphore)
{
	CheckIsRenderThread();

	VERIFY(bRenderer == true);

	u32 QueuedFrameIndex = FrameIndex % QueuedFrames.size();
	const FQueuedFrame& QueuedFrame = QueuedFrames[QueuedFrameIndex];
	const FSwapChainTexture& SwapChainTexture = SwapChainTextures[CurrentSwapChainTextureIndex];
	nri::CommandBuffer& CurrentCommandBuffer = *QueuedFrame.CommandBuffer;

	GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
	GRenderDevice.CoreInterface.CmdEndRendering(CurrentCommandBuffer);

	nri::TextureBarrierDesc TextureBarriers = {};
	TextureBarriers.texture = SwapChainTexture.Texture;
	TextureBarriers.layerNum = 1;
	TextureBarriers.mipNum = 1;
	TextureBarriers.before = {nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT};
	TextureBarriers.after = {nri::AccessBits::NONE, nri::Layout::PRESENT, nri::StageBits::NONE};

	nri::BarrierDesc BarrierDescription = {};
	BarrierDescription.textureNum = 1;
	BarrierDescription.textures = &TextureBarriers;

	GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer, BarrierDescription);
	GRenderDevice.CoreInterface.EndCommandBuffer(CurrentCommandBuffer);


	{
		nri::FenceSubmitDesc WaitFencesSubmitDescription[2] = {};
		WaitFencesSubmitDescription[0].fence = SwapChainTexture.AcquireSemaphore;
		WaitFencesSubmitDescription[0].stages = nri::StageBits::COLOR_ATTACHMENT;
		WaitFencesSubmitDescription[1].fence = WaitSemaphore;
		WaitFencesSubmitDescription[1].stages = nri::StageBits::FRAGMENT_SHADER;

		nri::FenceSubmitDesc SignalFencesSubmitDescription[2] = {};
		SignalFencesSubmitDescription[0].fence = SwapChainTexture.ReleaseSemaphore;
		SignalFencesSubmitDescription[1].fence = SignalSemaphore;

		nri::QueueSubmitDesc QueueSubmitDescription = {};
		QueueSubmitDescription.waitFences = WaitFencesSubmitDescription;
		QueueSubmitDescription.waitFenceNum = WaitSemaphore ? 2 : 1;

		QueueSubmitDescription.commandBuffers = &QueuedFrame.CommandBuffer;
		QueueSubmitDescription.commandBufferNum = 1;

		QueueSubmitDescription.signalFences = SignalFencesSubmitDescription;
		QueueSubmitDescription.signalFenceNum = SignalSemaphore ? 2 : 1;

		GRenderDevice.CoreInterface.QueueSubmit(*GRenderDevice.GraphicsQueue, QueueSubmitDescription);
	}

	GRenderDevice.SwapChainInterface.QueuePresent(*SwapChain, *SwapChainTexture.ReleaseSemaphore);

	{
		nri::FenceSubmitDesc SignalFenceSubmitDescription = {};
		SignalFenceSubmitDescription.fence = FrameFence;
		SignalFenceSubmitDescription.value = ++FrameIndex;

		nri::QueueSubmitDesc QueueSubmitDescription = {};
		QueueSubmitDescription.signalFences = &SignalFenceSubmitDescription;
		QueueSubmitDescription.signalFenceNum = 1;

		GRenderDevice.CoreInterface.QueueSubmit(*GRenderDevice.GraphicsQueue, QueueSubmitDescription);
	}
	bRenderer = false;
}

bool TiramisuRenderViewport::IsValid() const
{
	CheckIsRenderThread();
	return SwapChain != nullptr;
}

u8 TiramisuRenderViewport::GetOptimalTextureNum() const
{
	return GetQueuedFrameNum() + 1;
}

u8 TiramisuRenderViewport::GetQueuedFrameNum() const
{
	return bVSync ? 2 : 3;
}
