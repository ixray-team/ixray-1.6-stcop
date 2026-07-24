#pragma once

#include "TiramisuRenderTypes.h"
#include <atomic>
#include <barrier>
#include <mutex>
#include <semaphore>

#include "TiramisuRenderViewport.h"
#include "TiramisuDeferredDeletionQueue.h"
#include "src/xrCore/RenderStatistics.h"
#include "Extensions/NRIImgui.h"

class TiramisuRenderTargetResourceProxy;
class TiramisuRenderDeferredPass;
class TiramisuRenderTexture;
class TiramisuRenderTarget2D;
class TiramisuRenderTexture2D;
class TiramisuRenderUIPass;
struct ImDrawData;

// Глобальные данные кадра и bindless-индексы, общие для material shader passes.
struct alignas(16) FXRayRenderConstantBuffer
{
	Fvector4 SceneView;
	Fmatrix ViewProjection;
	Fvector4 CameraPositionAndTime;
	u32 DrawDataBufferIndex;
	u32 MaterialInstanceBufferIndex;
	u32 MaterialParameterBufferIndex;
	u32 DefaultMaterialSamplerIndex;
	u32 LightDataBufferIndex;
	u32 LightDataOffset;
	u32 LightCount;
	u32 LightingFlags;
};

static_assert(sizeof(FXRayRenderConstantBuffer) == 128);

// Главный координатор Tiramisu: владеет render thread, кадрами, passes и глобальными GPU-ресурсами.
class TiramisuRender
{
public:
	TiramisuRender();
	~TiramisuRender();

	// Создаёт render thread и постоянные ресурсы renderer.
	void Initialize();
	// Останавливает поток, ожидает GPU и освобождает ресурсы в безопасном порядке.
	void Destroy();

	// Game-thread API подготовки и отправки очередного кадра.
	void SetViewport(TiramisuRenderViewport* ToViewport);
	void PrepareImguiFrame();
	void SubmitFrame();

	// Render-thread API; суффикс является частью контракта принадлежности потоку.
	void Render_RenderThread();
	void WaitGPU_RenderThread();
	void DeferDelete_RenderThread(Tiramisu::TiramisuDeferredDeletionQueue::FDeleteFunction Function);
	void RecordDrawStatistics_RenderThread(u64 TriangleCount, u64 LineCount = 0);
	void RecordUploadStatistics_RenderThread(u64 ByteCount);
	FRenderStatisticsSnapshot GetRenderStatistics() const;

	void ResizeRenderTarget(u32 InWidth, u32 InHeight);
	TiramisuRenderUIPass* UIPass = nullptr;

	void EnableRenderThread();
	void DisableRenderThread();
	void DisableRenderThreadWithWaitStoping();

protected:
	// Создаёт ABI-буфер, читаемый шейдерами через bindless descriptor index.
	void CreateGlobalConstantBuffer();
	void UpdateGlobalConstantBuffer();
	void Submit(TiramisuRenderViewport* ToViewport);
	FRenderResourceStatistics CollectResourceStatistics_RenderThread() const;

	TiramisuRenderViewport* CurrentViewport = nullptr;
	xr_vector<FQueuedFrame> QueuedFrames;
	static constexpr u32 QueuedFrameCount = 3;

	nri::Fence* FrameFence = nullptr;
	u32 FrameIndex = 0;
	nri::Fence* WaitSemaphore = nullptr;
	nri::Fence* SignalSemaphore = nullptr;

	nri::DescriptorSet* GlobalConstantDescriptorSet = nullptr;
	nri::Buffer* GlobalConstantBuffer = nullptr;
	nri::Descriptor* GlobalConstantDescriptor = nullptr;
	nri::Memory* GlobalConstantBufferMemory = nullptr;

	nri::Pipeline* Pipeline = nullptr;


	TiramisuRenderTarget2D* OutputRenderTarget = nullptr;
	TiramisuRenderTarget2D* DepthRenderTarget = nullptr;

	TiramisuRenderTargetResourceProxy* OutputRenderTarget_RenderThread = nullptr;
	TiramisuRenderTargetResourceProxy* DepthRenderTarget_RenderThread = nullptr;

	TiramisuRenderDeferredPass* GeometryPass = nullptr;

	nri::Imgui* ImGuiInstance = nullptr;
	Tiramisu::TiramisuDeferredDeletionQueue DeferredDeletionQueue;
	FRenderStatisticsTracker StatisticsTracker;
	mutable std::mutex StatisticsMutex;
	FRenderStatisticsSnapshot PublishedStatistics;

private:
	// Точка входа выделенного render thread.
	static void OnThread(void* p);
	void SpawnRenderThread();

	std::atomic_bool bRenderThreadEnable = true;
	std::barrier<> SyncPoint{2};
	std::binary_semaphore ImguiFrameConsumed{0};
	const ImDrawData* PendingImguiDrawData = nullptr;
	bool bImguiFramePrepared = false;
	ThreadID RenderThread = nullptr;
};
extern TiramisuRender* GRender;
