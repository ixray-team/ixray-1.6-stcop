#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRender.h"

// Один back buffer swapchain и синхронизация его захвата и показа.
struct FSwapChainTexture
{
	nri::Fence* AcquireSemaphore = nullptr;
	nri::Fence* ReleaseSemaphore = nullptr;
	nri::Texture* Texture = nullptr;
	nri::Descriptor* ColorAttachment = nullptr;
	nri::Format AttachmentFormat = nri::Format::UNKNOWN;
};

// Командные ресурсы одного кадра в циклической очереди viewport.
struct FQueuedFrame
{
	nri::CommandAllocator* CommandAllocator = nullptr;
	nri::CommandBuffer* CommandBuffer = nullptr;
};

// Управляет swapchain, back buffers и записью команд для окна renderer.
class TiramisuRenderViewport
{
public:
	TiramisuRenderViewport();
	~TiramisuRenderViewport();

	// Создаёт swapchain или пересоздаёт его при resize и смене VSync.
	void CreateOrReset(SDL_Window* InWindows, u32 InWidth, u32 InHeight, bool InVSync = false);
	void Destroy();

	// Открывает запись команд для текущего swapchain image.
	void BeginRender(nri::DescriptorPool* DescriptionPool = nullptr);
	nri::CommandBuffer& GetCurrentCommandBuffer();
	// Завершает запись, отправляет команды и планирует present.
	void EndRender(nri::Fence* WaitSemaphore = nullptr, nri::Fence* SignalSemaphore = nullptr);
	bool IsValid() const;

	u32 GetWidth() const { return Width; }
	u32 GetHeight() const { return Height; }
	nri::Format GetSwapChainFormat() const { return SwapChainFormat; }
	void* GetNativeWindowHandle() const { return NativeWindowHandle; }
	bool HasPresentedFrame() const { return FrameIndex != 0; }
	u32 GetSwapChainTextureCount() const
	{
		return static_cast<u32>(SwapChainTextures.size());
	}

protected:
	u8 GetOptimalTextureNum() const;
	u8 GetQueuedFrameNum() const;

	nri::SwapChain* SwapChain = nullptr;

	nri::Format SwapChainFormat = nri::Format::UNKNOWN;
	xr_vector<FSwapChainTexture> SwapChainTextures;
	xr_vector<FQueuedFrame> QueuedFrames;

	u32 FrameIndex = 0;
	nri::Fence* FrameFence = nullptr;

	bool bVSync = false;
	u32 Width = 0;
	u32 Height = 0;
	void* NativeWindowHandle = nullptr;

private:
	bool bRenderer = false;
	u32 CurrentSwapChainTextureIndex = 0;
};
