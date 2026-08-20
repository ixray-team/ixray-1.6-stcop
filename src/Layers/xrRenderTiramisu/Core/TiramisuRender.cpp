#include "TiramisuRender.h"

#include "src/xrCore/RenderDocIntegration.h"
#include "src/xrCore/RenderTestPolicy.h"
#include "Passes/Geometry/TiramisuRenderDeferredPass.h"
#include "Passes/UI/TiramisuRenderUIPass.h"
#include "Resources/Materials/TiramisuRenderMaterialGpuStorage.h"
#include "Resources/Materials/TiramisuRenderMaterialPipelineRegistry.h"
#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/Shaders/ShaderType.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesContainer.h"
#include "Resources/Shaders/Global/TiramisuGlobalShadersManager.h"
#include "Resources/Textures/RenderTargets/TiramisuRenderTarget2D.h"
#include "Scene/TiramisuRenderScene.h"
#include "src/xrEngine/ImGuiManager.h"

#include <imgui.h>
#include <algorithm>
#include <chrono>

void TiramisuRender::OnThread(void* p)
{
	TiramisuRender* This = static_cast<TiramisuRender*>(p);
	Platform::SetThreadName("Render Thread");
	PROF_THREAD("Render Thread");
	GRenderThreadId.store(Platform::GetCurrentThreadId(), std::memory_order_release);

	This->SyncPoint.arrive_and_wait();
	do
	{
		This->Render_RenderThread();
	} while (This->bRenderThreadEnable.load(std::memory_order_acquire));

	GRenderThreadId.store(GGameThreadId, std::memory_order_release);
}

void TiramisuRender::SpawnRenderThread()
{
	CheckIsGameThread();
	RenderThread = thread_spawn(&TiramisuRender::OnThread, "Render Thread", 0, this);
	SyncPoint.arrive_and_wait();
	VERIFY(GRenderThreadId.load(std::memory_order_acquire) != Platform::GetCurrentThreadId());
}

TiramisuRender* GRender = nullptr;

TiramisuRender::TiramisuRender()
{
	bRenderThreadEnable = !strstr(Core.Params, "-disable_render_thread");
}

TiramisuRender::~TiramisuRender()
{
}


void TiramisuRender::Initialize()
{
	CheckIsGameThread();
	Destroy();

	NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, WaitSemaphore));
	NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, SignalSemaphore));
	NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, 0, FrameFence));

	QueuedFrames.resize(QueuedFrameCount);
	for (FQueuedFrame& QueuedFrame : QueuedFrames)
	{
		NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandAllocator(*GRenderDevice.GraphicsQueue, QueuedFrame.CommandAllocator));
		NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandBuffer(*QueuedFrame.CommandAllocator, QueuedFrame.CommandBuffer));
	}
	CreateGlobalConstantBuffer();
	CreateDeferredLightingResources();

	OutputRenderTarget = new TiramisuRenderTarget2D(1024, 768, nri::Format::RGBA8_UNORM, {}, "Output");
	DepthRenderTarget = new TiramisuRenderTarget2D(
		1024, 768, TiramisuGBufferLayout::DepthFormat, {}, "GBufferDepth"
	);
	constexpr xr_array<const char*, TiramisuGBufferLayout::TargetCount>
		GBufferNames = {
			"GBufferBaseColorAO",
			"GBufferNormalRoughnessMetallic",
			"GBufferEmissiveMaterialFlags",
			"GBufferVelocity"
		};
	for (u32 TargetIndex = 0;
		 TargetIndex < TiramisuGBufferLayout::TargetCount;
		 ++TargetIndex)
	{
		GBufferRenderTargets[TargetIndex] = new TiramisuRenderTarget2D(
			1024,
			768,
			TiramisuGBufferLayout::TargetFormats[TargetIndex],
			{},
			GBufferNames[TargetIndex]
		);
	}

	UIPass = new TiramisuRenderUIPass;
	GeometryPass = new TiramisuRenderDeferredPass;

	nri::ImguiDesc imguiDesc = {};
	NRI_CHECK(GRenderDevice.ImGuiInterface.CreateImgui(*GRenderDevice.Device, imguiDesc, ImGuiInstance));

	R_ASSERT(!IsRenderThreadRunning());
}

void TiramisuRender::Destroy()
{
	CheckIsGameThread();
	R_ASSERT(!IsRenderThreadRunning());
	WaitGPU_RenderThread();
	DeferredDeletionQueue.Flush();

	{
		if (GlobalConstantDescriptor)
		{
			GRenderDevice.CoreInterface.DestroyDescriptor(GlobalConstantDescriptor);
			GlobalConstantDescriptor = nullptr;
		}
		if (GlobalConstantBuffer)
		{
			GRenderDevice.CoreInterface.DestroyBuffer(GlobalConstantBuffer);
			GlobalConstantBuffer = nullptr;
		}
		if (GlobalConstantBufferMemory)
		{
			GRenderDevice.CoreInterface.FreeMemory(GlobalConstantBufferMemory);
			GlobalConstantBufferMemory = nullptr;
		}
		GlobalConstantDescriptorSet = nullptr;
	}
	{
		if (DeferredLightingConstantDescriptor)
		{
			GRenderDevice.CoreInterface.DestroyDescriptor(
				DeferredLightingConstantDescriptor
			);
			DeferredLightingConstantDescriptor = nullptr;
		}
		if (DeferredLightingConstantBuffer)
		{
			GRenderDevice.CoreInterface.DestroyBuffer(
				DeferredLightingConstantBuffer
			);
			DeferredLightingConstantBuffer = nullptr;
		}
		if (DeferredLightingConstantBufferMemory)
		{
			GRenderDevice.CoreInterface.FreeMemory(
				DeferredLightingConstantBufferMemory
			);
			DeferredLightingConstantBufferMemory = nullptr;
		}
		DeferredLightingDescriptorSet = nullptr;
	}

	if (OutputRenderTarget)
	{
		delete OutputRenderTarget;
		OutputRenderTarget = nullptr;
	}

	if (DepthRenderTarget)
	{
		delete DepthRenderTarget;
		DepthRenderTarget = nullptr;
	}
	for (TiramisuRenderTarget2D*& Target : GBufferRenderTargets)
	{
		delete Target;
		Target = nullptr;
	}

	if (ImGuiInstance)
	{
		GRenderDevice.ImGuiInterface.DestroyImgui(ImGuiInstance);
		ImGuiInstance = nullptr;
	}

	if (UIPass)
	{
		delete UIPass;
		UIPass = nullptr;
	}

	if (GeometryPass)
	{
		delete GeometryPass;
		GeometryPass = nullptr;
	}

	if (Pipeline)
	{
		GRenderDevice.CoreInterface.DestroyPipeline(Pipeline);
		Pipeline = nullptr;
	}
	if (DeferredLightingPipeline)
	{
		GRenderDevice.CoreInterface.DestroyPipeline(
			DeferredLightingPipeline
		);
		DeferredLightingPipeline = nullptr;
	}

	if (WaitSemaphore)
	{
		GRenderDevice.CoreInterface.DestroyFence(WaitSemaphore);
		WaitSemaphore = nullptr;
	}

	if (SignalSemaphore)
	{
		GRenderDevice.CoreInterface.DestroyFence(SignalSemaphore);
		SignalSemaphore = nullptr;
	}

	if (FrameFence)
	{
		GRenderDevice.CoreInterface.DestroyFence(FrameFence);
		FrameFence = nullptr;
	}

	for (FQueuedFrame& QueuedFrame : QueuedFrames)
	{
		if (QueuedFrame.CommandBuffer)
		{
			GRenderDevice.CoreInterface.DestroyCommandBuffer(QueuedFrame.CommandBuffer);
			QueuedFrame.CommandBuffer = nullptr;
		}
		if (QueuedFrame.CommandAllocator)
		{
			GRenderDevice.CoreInterface.DestroyCommandAllocator(QueuedFrame.CommandAllocator);
			QueuedFrame.CommandAllocator = nullptr;
		}
	}
	FrameIndex = 0;
	bRenderDocCaptureAttempted = false;
	bRenderDocCaptureActive = false;
	StatisticsTracker.Reset();
	{
		std::scoped_lock Lock(StatisticsMutex);
		PublishedStatistics = {};
	}
}

void TiramisuRender::SetViewport(TiramisuRenderViewport* ToViewport)
{
	CheckIsGameThread();
	ENQUEUE_RENDER_COMMAND(TiramisuRender::SetViewport)([this, ToViewport]
														{
        CheckIsRenderThread();
        CurrentViewport = ToViewport; });
}


void TiramisuRender::Submit(TiramisuRenderViewport* ToViewport)
{
	CheckIsRenderThread();
	// Pipeline
	if (!Pipeline)
	{
		nri::VertexStreamDesc vertexStreamDesc = {};
		vertexStreamDesc.bindingSlot = 0;

		nri::VertexInputDesc vertexInputDesc = {};
		vertexInputDesc.attributes = FUIVertex::VertexAttributeDescription;
		vertexInputDesc.attributeNum = 3;
		vertexInputDesc.streams = &vertexStreamDesc;
		vertexInputDesc.streamNum = 1;

		nri::InputAssemblyDesc inputAssemblyDesc = {};
		inputAssemblyDesc.topology = nri::Topology::TRIANGLE_LIST;

		nri::RasterizationDesc rasterizationDesc = {};
		rasterizationDesc.fillMode = nri::FillMode::SOLID;
		rasterizationDesc.cullMode = nri::CullMode::FRONT;

		nri::ColorAttachmentDesc colorAttachmentDesc = {};
		colorAttachmentDesc.format = ToViewport->GetSwapChainFormat();
		colorAttachmentDesc.colorWriteMask = nri::ColorWriteBits::RGBA;
		colorAttachmentDesc.blendEnabled = true;
		colorAttachmentDesc.colorBlend = {nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};

		nri::OutputMergerDesc outputMergerDesc = {};
		outputMergerDesc.colors = &colorAttachmentDesc;
		outputMergerDesc.colorNum = 1;

		TiramisuShaderDefinesContainer ShaderDefinesContainer;

		nri::ShaderDesc shaderStages[2] = {};
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("ui_no_transform", EShaderType::Vertex, ShaderDefinesContainer);
			shaderStages[0].stage = nri::StageBits::VERTEX_SHADER;
			shaderStages[0].bytecode = ShaderCode.data();
			shaderStages[0].size = ShaderCode.size();
			shaderStages[0].entryPointName = "Main";
		}
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("output", EShaderType::Pixel, ShaderDefinesContainer);
			shaderStages[1].stage = nri::StageBits::FRAGMENT_SHADER;
			shaderStages[1].bytecode = ShaderCode.data();
			shaderStages[1].size = ShaderCode.size();
			shaderStages[1].entryPointName = "Main";
		}

		nri::GraphicsPipelineDesc graphicsPipelineDesc = {};
		graphicsPipelineDesc.pipelineLayout = GRenderResourcesManager->GlobalPipelineLayout;
		graphicsPipelineDesc.vertexInput = &vertexInputDesc;
		graphicsPipelineDesc.inputAssembly = inputAssemblyDesc;
		graphicsPipelineDesc.rasterization = rasterizationDesc;
		graphicsPipelineDesc.outputMerger = outputMergerDesc;
		graphicsPipelineDesc.shaders = shaderStages;
		graphicsPipelineDesc.shaderNum = 2;
		graphicsPipelineDesc.cache = nullptr;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(*GRenderDevice.Device, graphicsPipelineDesc, Pipeline));
	}


	ToViewport->BeginRender(GRenderResourcesManager->GlobalDescriptorPool);
	StatisticsTracker.RecordPass();
	{
		nri::Dim_t w = (nri::Dim_t)ToViewport->GetWidth();
		nri::Dim_t h = (nri::Dim_t)ToViewport->GetHeight();

		// helper::Annotation annotation(NRI, CommandBuffer, "Triangle");

		nri::CommandBuffer& CommandBuffer = ToViewport->GetCurrentCommandBuffer();

		GRenderDevice.CoreInterface.CmdSetPipelineLayout(CommandBuffer, nri::BindPoint::GRAPHICS, *GRenderResourcesManager->GlobalPipelineLayout);

		{
			const nri::Viewport viewport = {0.0f, 0.0f, (float)w, (float)h, 0.0f, 1.0f};
			GRenderDevice.CoreInterface.CmdSetViewports(CommandBuffer, &viewport, 1);
			const nri::Rect ScissorRect = {0, 0, w, h};
			GRenderDevice.CoreInterface.CmdSetScissors(CommandBuffer, &ScissorRect, 1);
		}

		GRenderDevice.CoreInterface.CmdSetPipeline(CommandBuffer, *Pipeline);

		nri::SetDescriptorSetDesc descriptorSet0 = {0, GRenderResourcesManager->ResourcesDescriptorSet};
		nri::SetDescriptorSetDesc descriptorSet1 = {1, GRenderResourcesManager->SamplerDescriptorSet};
		nri::SetDescriptorSetDesc descriptorSet2 = {2, GlobalConstantDescriptorSet};
		GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer, descriptorSet0);
		GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer, descriptorSet1);
		GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer, descriptorSet2);

		GRenderDevice.CoreInterface.CmdSetIndexBuffer(CommandBuffer, *GRenderResourcesManager->QuadGeometryBuffer, 0, nri::IndexType::UINT16);

		nri::VertexBufferDesc vertexBufferDesc = {};
		vertexBufferDesc.buffer = GRenderResourcesManager->QuadGeometryBuffer;
		vertexBufferDesc.offset = GRenderResourcesManager->QuadGeometryOffset;
		vertexBufferDesc.stride = sizeof(FUIVertex);
		GRenderDevice.CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &vertexBufferDesc, 1);


		GRenderDevice.CoreInterface.CmdDrawIndexed(CommandBuffer, {6, 1, 0, 0, OutputRenderTarget->ResourceProxy->GetOrCreateHeapID()});
		StatisticsTracker.RecordDraw(2);
	}

	ToViewport->EndRender(SignalSemaphore, nullptr);
}

void TiramisuRender::SubmitFrame()
{
	CheckIsGameThread();
	const bool WaitForImguiFrame = bImguiFramePrepared;
	GRenderResourcesManager->FlushNextFrame();

	if (!IsRenderThreadRunning())
	{
		if (bRenderThreadEnable)
		{
			SpawnRenderThread();
		}
		else
		{
			Render_RenderThread();
			if (WaitForImguiFrame)
			{
				ImguiFrameConsumed.acquire();
				bImguiFramePrepared = false;
			}
			return;
		}
	}

	SyncPoint.arrive_and_wait();
	if (WaitForImguiFrame)
	{
		ImguiFrameConsumed.acquire();
		bImguiFramePrepared = false;
	}
}

void TiramisuRender::PrepareImguiFrame()
{
	CheckIsGameThread();
	VERIFY(!bImguiFramePrepared);
	VERIFY(PendingImguiDrawData == nullptr);

#ifdef IXR_WINDOWS
	if (ImGui::GetCurrentContext() == nullptr)
	{
		return;
	}

	CImGuiManager& ImguiManager = CImGuiManager::Instance();
	ImguiManager.BeginRender();
	ImguiManager.Render();
	PendingImguiDrawData = ImGui::GetDrawData();
	ImguiManager.AfterRender();

	bImguiFramePrepared = PendingImguiDrawData != nullptr;
#endif
}

void TiramisuRender::Render_RenderThread()
{
	CheckIsRenderThread();
	const auto CpuFrameStart = std::chrono::steady_clock::now();
	GRenderDevice.CoreInterface.Wait(*FrameFence, FrameIndex >= QueuedFrames.size() ? 1 + FrameIndex - QueuedFrames.size() : 0);
	DeferredDeletionQueue.Collect(
		GRenderDevice.CoreInterface.GetFenceValue(*FrameFence)
	);
	StatisticsTracker.BeginFrame(FrameIndex);
	const u32 QueuedFrameIndex =
		FrameIndex % static_cast<u32>(QueuedFrames.size());

	{
		if (IsRenderThreadRunning())
		{
			{
				PROF_EVENT("Wait Game Thread")
				SyncPoint.arrive_and_wait();
			}
		}

		{
			PROF_EVENT("Render Flush")
			Tiramisu::RenderCommands::ExecuteRenderCommands();
		}


		GRenderResourcesManager->MaterialGpuStorage->BeginFrame_RenderThread(
			QueuedFrameIndex
		);
		GRenderResourcesManager->RenderScene->Update();
		UpdateGlobalConstantBuffer();
		UpdateDeferredLightingConstants_RenderThread();
		GRenderResourcesManager->FlushNextFrame_RenderThread();
	}


	PROF_EVENT("Main Render")
	const bool RenderDocCaptureRequested =
		xrRenderDoc::IsAvailable() &&
		HasRenderCommandLineFlag(
			Core.Params ? Core.Params : "", "-renderdoc-capture"
		);
	const bool RenderDocSceneCaptureRequested =
		HasRenderCommandLineFlag(
			Core.Params ? Core.Params : "", "-renderdoc-capture-scene"
		);
	bool RenderDocSceneReady = false;
	if (RenderDocSceneCaptureRequested)
	{
		for (const TiramisuPrimitiveSceneProxy* SceneProxy :
			 GRenderResourcesManager->RenderScene->RenderSceneProxies)
		{
			if (SceneProxy && SceneProxy->GetNumMeshBatches() != 0)
			{
				RenderDocSceneReady = true;
				break;
			}
		}
	}
	const bool RenderDocCaptureContentReady =
		!RenderDocSceneCaptureRequested || RenderDocSceneReady;
	if (RenderDocCaptureRequested && !bRenderDocCaptureAttempted &&
		RenderDocCaptureContentReady && CurrentViewport &&
		CurrentViewport->IsValid() && CurrentViewport->HasPresentedFrame())
	{
		bRenderDocCaptureAttempted = true;
		void* RenderDocDeviceHandle = nullptr;
		if (GRenderDevice.GraphicsApi == nri::GraphicsAPI::D3D12)
		{
			RenderDocDeviceHandle =
				GRenderDevice.CoreInterface.GetDeviceNativeObject(
					GRenderDevice.Device
				);
		}
		bRenderDocCaptureActive = xrRenderDoc::BeginCapture(
			CurrentViewport->GetNativeWindowHandle(),
			RenderDocDeviceHandle
		);
		if (bRenderDocCaptureActive)
		{
			Msg("* RenderDoc: Tiramisu game frame capture started");
		}
		else if (xrRenderDoc::TriggerCapture())
		{
			// Некоторые D3D12 drivers не принимают explicit device/window pair,
			// но корректно захватывают следующую активную Present-пару.
			Msg("* RenderDoc: Tiramisu game frame capture scheduled");
		}
		else
		{
			Msg("! RenderDoc: Tiramisu game frame capture could not start");
		}
	}

	const FQueuedFrame& QueuedFrame = QueuedFrames[QueuedFrameIndex];

	nri::CommandBuffer& CurrentCommandBuffer = *QueuedFrame.CommandBuffer;

	GRenderDevice.CoreInterface.ResetCommandAllocator(*QueuedFrame.CommandAllocator);
	GRenderDevice.CoreInterface.BeginCommandBuffer(CurrentCommandBuffer, GRenderResourcesManager->GlobalDescriptorPool);

	GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer, "Main", nri::BGRA_UNUSED);
	{
		{
			UIPass->Upload(CurrentCommandBuffer);
		}
		{
			xr_array<nri::TextureBarrierDesc,
				TiramisuGBufferLayout::TargetCount + 1> TextureBarriers = {};
			for (u32 TargetIndex = 0;
				 TargetIndex < TiramisuGBufferLayout::TargetCount;
				 ++TargetIndex)
			{
				TiramisuRenderTarget2D* Target =
					GBufferRenderTargets[TargetIndex];
				TextureBarriers[TargetIndex].texture =
					Target->ResourceProxy->Texture;
				TextureBarriers[TargetIndex].layerNum = 1;
				TextureBarriers[TargetIndex].mipNum = 1;
				Target->RenderTargetResourceProxy->SetNewAccessLayoutStage(
					TextureBarriers[TargetIndex],
					{
						nri::AccessBits::COLOR_ATTACHMENT,
						nri::Layout::COLOR_ATTACHMENT
					}
				);
			}

			nri::TextureBarrierDesc& DepthBarrier =
				TextureBarriers[TiramisuGBufferLayout::TargetCount];
			DepthBarrier.texture = DepthRenderTarget->ResourceProxy->Texture;
			DepthBarrier.layerNum = 1;
			DepthBarrier.mipNum = 1;
			DepthRenderTarget->RenderTargetResourceProxy->SetNewAccessLayoutStage(
				DepthBarrier,
				{
					nri::AccessBits::DEPTH_STENCIL_ATTACHMENT_WRITE,
					nri::Layout::DEPTH_STENCIL_ATTACHMENT
				}
			);

			nri::BarrierDesc BarrierDescription = {};
			BarrierDescription.textureNum = TextureBarriers.size();
			BarrierDescription.textures = TextureBarriers.data();
			GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer, BarrierDescription);
		}

		const ImDrawData* ImguiDrawData = PendingImguiDrawData;
		if (ImguiDrawData)
		{
			nri::CopyImguiDataDesc CopyImguiDataDesc = {};
			CopyImguiDataDesc.drawLists = ImguiDrawData->CmdLists.Data;
			CopyImguiDataDesc.drawListNum = ImguiDrawData->CmdLists.Size;
			if (ImguiDrawData->Textures)
			{
				CopyImguiDataDesc.textures = ImguiDrawData->Textures->Data;
				CopyImguiDataDesc.textureNum = ImguiDrawData->Textures->Size;
			}

			GRenderDevice.ImGuiInterface.CmdCopyImguiData(CurrentCommandBuffer, *GRenderDevice.Streamer, *ImGuiInstance, CopyImguiDataDesc);
		}

		GeometryPass->Prepare_RenderThread();
		GRenderResourcesManager->MaterialGpuStorage
			->Upload_RenderThread(CurrentCommandBuffer);

		{
			xr_array<nri::AttachmentDesc,
				TiramisuGBufferLayout::TargetCount> ColorAttachments = {};
			for (u32 TargetIndex = 0;
				 TargetIndex < TiramisuGBufferLayout::TargetCount;
				 ++TargetIndex)
			{
				ColorAttachments[TargetIndex].descriptor =
					GBufferRenderTargets[TargetIndex]
						->RenderTargetResourceProxy->DescriptorAttachment;
				ColorAttachments[TargetIndex].loadOp = nri::LoadOp::CLEAR;
			}
			ColorAttachments[TiramisuGBufferLayout::GetTargetIndex(
				ETiramisuGBufferTarget::BaseColorAmbientOcclusion
			)].clearValue.color.f = {0.0f, 0.0f, 0.0f, 1.0f};
			ColorAttachments[TiramisuGBufferLayout::GetTargetIndex(
				ETiramisuGBufferTarget::NormalRoughnessMetallic
			)].clearValue.color.f = {0.5f, 0.5f, 1.0f, 0.0f};
			ColorAttachments[TiramisuGBufferLayout::GetTargetIndex(
				ETiramisuGBufferTarget::EmissiveMaterialFlags
			)].clearValue.color.f = {0.0f, 0.0f, 0.0f, 0.0f};
			ColorAttachments[TiramisuGBufferLayout::GetTargetIndex(
				ETiramisuGBufferTarget::Velocity
			)].clearValue.color.f = {0.0f, 0.0f, 0.0f, 0.0f};

			nri::RenderingDesc RenderingDescription = {};
			RenderingDescription.colorNum = ColorAttachments.size();
			RenderingDescription.colors = ColorAttachments.data();
			RenderingDescription.depth.clearValue = {1, 0x0};
			RenderingDescription.depth.loadOp = nri::LoadOp::CLEAR;
			RenderingDescription.depth.descriptor = DepthRenderTarget->RenderTargetResourceProxy->DescriptorAttachment;

			GRenderDevice.CoreInterface.CmdBeginRendering(CurrentCommandBuffer, RenderingDescription);
		}
		{
			GRenderDevice.CoreInterface.CmdSetPipelineLayout(CurrentCommandBuffer, nri::BindPoint::GRAPHICS, *GRenderResourcesManager->GlobalPipelineLayout);
			nri::SetDescriptorSetDesc descriptorSet0 = {0, GRenderResourcesManager->ResourcesDescriptorSet};
			nri::SetDescriptorSetDesc descriptorSet1 = {1, GRenderResourcesManager->SamplerDescriptorSet};
			nri::SetDescriptorSetDesc descriptorSet2 = {2, GlobalConstantDescriptorSet};
			GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer, descriptorSet0);
			GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer, descriptorSet1);
			GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer, descriptorSet2);
		}
		const nri::Viewport viewport = {0.0f, 0.0f, (float)OutputRenderTarget->TextureDescription.width, (float)OutputRenderTarget->TextureDescription.height, 0.0f, 1.0f};
		{
			GRenderDevice.CoreInterface.CmdSetViewports(CurrentCommandBuffer, &viewport, 1);

			const nri::Rect ScissorRect = {0, 0, OutputRenderTarget->TextureDescription.width, OutputRenderTarget->TextureDescription.height};
			GRenderDevice.CoreInterface.CmdSetScissors(CurrentCommandBuffer, &ScissorRect, 1);
		}

		StatisticsTracker.RecordPass();
		GeometryPass->Render(CurrentCommandBuffer);
		GRenderDevice.CoreInterface.CmdEndRendering(CurrentCommandBuffer);

		{
			xr_array<nri::TextureBarrierDesc,
				TiramisuGBufferLayout::TargetCount + 2> TextureBarriers = {};
			for (u32 TargetIndex = 0;
				 TargetIndex < TiramisuGBufferLayout::TargetCount;
				 ++TargetIndex)
			{
				TiramisuRenderTarget2D* Target =
					GBufferRenderTargets[TargetIndex];
				TextureBarriers[TargetIndex].texture =
					Target->ResourceProxy->Texture;
				TextureBarriers[TargetIndex].layerNum = 1;
				TextureBarriers[TargetIndex].mipNum = 1;
				Target->RenderTargetResourceProxy->SetNewAccessLayoutStage(
					TextureBarriers[TargetIndex],
					{
						nri::AccessBits::SHADER_RESOURCE,
						nri::Layout::SHADER_RESOURCE
					}
				);
			}

			nri::TextureBarrierDesc& DepthBarrier =
				TextureBarriers[TiramisuGBufferLayout::TargetCount];
			DepthBarrier.texture = DepthRenderTarget->ResourceProxy->Texture;
			DepthBarrier.layerNum = 1;
			DepthBarrier.mipNum = 1;
			DepthRenderTarget->RenderTargetResourceProxy->SetNewAccessLayoutStage(
				DepthBarrier,
				{
					nri::AccessBits::SHADER_RESOURCE,
					nri::Layout::SHADER_RESOURCE
				}
			);

			nri::TextureBarrierDesc& OutputBarrier =
				TextureBarriers[TiramisuGBufferLayout::TargetCount + 1];
			OutputBarrier.texture = OutputRenderTarget->ResourceProxy->Texture;
			OutputBarrier.layerNum = 1;
			OutputBarrier.mipNum = 1;
			OutputRenderTarget->RenderTargetResourceProxy
				->SetNewAccessLayoutStage(
					OutputBarrier,
					{
						nri::AccessBits::COLOR_ATTACHMENT,
						nri::Layout::COLOR_ATTACHMENT
					}
				);

			nri::BarrierDesc BarrierDescription = {};
			BarrierDescription.textureNum = TextureBarriers.size();
			BarrierDescription.textures = TextureBarriers.data();
			GRenderDevice.CoreInterface.CmdBarrier(
				CurrentCommandBuffer, BarrierDescription
			);
		}

		{
			nri::AttachmentDesc ColorAttachment = {};
			ColorAttachment.descriptor = OutputRenderTarget
				->RenderTargetResourceProxy->DescriptorAttachment;
			ColorAttachment.clearValue.color.f = {
				0.0f, 0.0f, 0.0f, 1.0f
			};
			ColorAttachment.loadOp = nri::LoadOp::CLEAR;

			nri::RenderingDesc RenderingDescription = {};
			RenderingDescription.colorNum = 1;
			RenderingDescription.colors = &ColorAttachment;
			GRenderDevice.CoreInterface.CmdBeginRendering(
				CurrentCommandBuffer, RenderingDescription
			);
		}

		CreateDeferredLightingPipeline_RenderThread();
		GRenderDevice.CoreInterface.CmdBeginAnnotation(
			CurrentCommandBuffer, "DeferredLighting", nri::BGRA_UNUSED
		);
		GRenderDevice.CoreInterface.CmdSetPipeline(
			CurrentCommandBuffer, *DeferredLightingPipeline
		);
		GRenderDevice.CoreInterface.CmdSetDescriptorSet(
			CurrentCommandBuffer,
			{2, DeferredLightingDescriptorSet}
		);
		GRenderDevice.CoreInterface.CmdDraw(
			CurrentCommandBuffer, {3, 1, 0, 0}
		);
		StatisticsTracker.RecordPass();
		StatisticsTracker.RecordDraw(1);
		GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);

		GRenderDevice.CoreInterface.CmdSetDescriptorSet(
			CurrentCommandBuffer,
			{2, GlobalConstantDescriptorSet}
		);
		StatisticsTracker.RecordPass();
		UIPass->Render(CurrentCommandBuffer, viewport);

		if (ImguiDrawData)
		{
			GRenderDevice.CoreInterface.CmdBeginAnnotation(
				CurrentCommandBuffer, "ImGui", nri::BGRA_UNUSED
			);
			StatisticsTracker.RecordPass();
			for (int ListIndex = 0;
				 ListIndex < ImguiDrawData->CmdListsCount;
				 ++ListIndex)
			{
				const ImDrawList* DrawList =
					ImguiDrawData->CmdLists[ListIndex];
				for (const ImDrawCmd& Command : DrawList->CmdBuffer)
				{
					if (!Command.UserCallback && Command.ElemCount != 0)
					{
						StatisticsTracker.RecordDraw(
							Command.ElemCount / 3
						);
					}
				}
			}
			nri::DrawImguiDesc DrawImguiDesc = {};
			DrawImguiDesc.drawLists = ImguiDrawData->CmdLists.Data;
			DrawImguiDesc.drawListNum = ImguiDrawData->CmdLists.Size;
			DrawImguiDesc.displaySize = {
				static_cast<nri::Dim_t>(ImguiDrawData->DisplaySize.x),
				static_cast<nri::Dim_t>(ImguiDrawData->DisplaySize.y)
			};
			DrawImguiDesc.hdrScale = 1.0f;
			DrawImguiDesc.attachmentFormat = nri::Format::RGBA8_UNORM;
			DrawImguiDesc.linearColor = false;
			GRenderDevice.ImGuiInterface.CmdDrawImgui(
				CurrentCommandBuffer, *ImGuiInstance, DrawImguiDesc
			);
			GRenderDevice.CoreInterface.CmdEndAnnotation(
				CurrentCommandBuffer
			);
		}

		GRenderDevice.CoreInterface.CmdEndRendering(CurrentCommandBuffer);

		if (ImguiDrawData)
		{
			PendingImguiDrawData = nullptr;
			ImguiFrameConsumed.release();
		}

		{
			nri::TextureBarrierDesc TextureBarrierDescription = {};
			TextureBarrierDescription.texture = OutputRenderTarget->ResourceProxy->Texture;
			TextureBarrierDescription.layerNum = 1;
			TextureBarrierDescription.mipNum = 1;
			OutputRenderTarget->RenderTargetResourceProxy->SetNewAccessLayoutStage(TextureBarrierDescription, {nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE});

			nri::BarrierDesc BarrierDescription = {};
			BarrierDescription.textureNum = 1;
			BarrierDescription.textures = &TextureBarrierDescription;
			GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer, BarrierDescription);
		}
	}

	GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
	GRenderDevice.CoreInterface.EndCommandBuffer(CurrentCommandBuffer);
	GRenderResourcesManager->DescriptorHeapAllocator->UpdateDescriptorRanges();

	{
		nri::FenceSubmitDesc SignalFencesSubmitDescription[2] = {};
		SignalFencesSubmitDescription[1].fence = SignalSemaphore;
		SignalFencesSubmitDescription[0].fence = FrameFence;
		SignalFencesSubmitDescription[0].value = ++FrameIndex;

		nri::QueueSubmitDesc QueueSubmitDescription = {};

		QueueSubmitDescription.commandBuffers = &QueuedFrame.CommandBuffer;
		QueueSubmitDescription.commandBufferNum = 1;

		QueueSubmitDescription.signalFences = SignalFencesSubmitDescription;
		QueueSubmitDescription.signalFenceNum = CurrentViewport && CurrentViewport->IsValid() ? 2 : 1;

		GRenderDevice.CoreInterface.QueueSubmit(*GRenderDevice.GraphicsQueue, QueueSubmitDescription);
		GRenderDevice.StreamerInterface.EndStreamerFrame(*GRenderDevice.Streamer);
	}
	if (CurrentViewport && CurrentViewport->IsValid())
	{
		Submit(CurrentViewport);
	}
	if (bRenderDocCaptureActive)
	{
		bRenderDocCaptureActive = false;
		void* RenderDocDeviceHandle = nullptr;
		if (GRenderDevice.GraphicsApi == nri::GraphicsAPI::D3D12)
		{
			RenderDocDeviceHandle =
				GRenderDevice.CoreInterface.GetDeviceNativeObject(
					GRenderDevice.Device
				);
		}
		const bool CaptureSucceeded = xrRenderDoc::EndCapture(
			CurrentViewport ? CurrentViewport->GetNativeWindowHandle() : nullptr,
			RenderDocDeviceHandle
		);
		Msg(CaptureSucceeded
			? "* RenderDoc: Tiramisu game frame capture completed"
			: "! RenderDoc: Tiramisu game frame capture could not complete");
	}
	StatisticsTracker.SetResources(CollectResourceStatistics_RenderThread());
	const auto CpuFrameEnd = std::chrono::steady_clock::now();
	StatisticsTracker.EndFrame(static_cast<u64>(
		std::chrono::duration_cast<std::chrono::nanoseconds>(
			CpuFrameEnd - CpuFrameStart
		)
			.count()
	));
	{
		std::scoped_lock Lock(StatisticsMutex);
		PublishedStatistics = StatisticsTracker.GetSnapshot();
	}
}


void TiramisuRender::WaitGPU_RenderThread()
{
	CheckIsRenderThread();
	GRenderDevice.CoreInterface.QueueWaitIdle(GRenderDevice.GraphicsQueue);
	DeferredDeletionQueue.Flush();

	if (GRenderResourcesManager)
	{
		GRenderResourcesManager->RenderScene->Update();
		GRenderResourcesManager->FlushNextFrame_RenderThread();
	}
}

void TiramisuRender::ResizeRenderTarget(u32 InWidth, u32 InHeight)
{
	CheckIsGameThread();

	ENQUEUE_RENDER_COMMAND(TiramisuRender::ResizeRenderTarget)([this,
																InOutputRenderTarget = OutputRenderTarget->RenderTargetResourceProxy,
																InDepthRenderTarget = DepthRenderTarget->RenderTargetResourceProxy]()
															   {
            CheckIsRenderThread();
            OutputRenderTarget_RenderThread = nullptr;
            DepthRenderTarget_RenderThread = nullptr; });


	xr_delete(OutputRenderTarget);
	xr_delete(DepthRenderTarget);
	for (TiramisuRenderTarget2D*& Target : GBufferRenderTargets)
	{
		xr_delete(Target);
	}

	OutputRenderTarget = new TiramisuRenderTarget2D(InWidth, InHeight, nri::Format::RGBA8_UNORM, {}, "Output");
	DepthRenderTarget = new TiramisuRenderTarget2D(
		InWidth,
		InHeight,
		TiramisuGBufferLayout::DepthFormat,
		{},
		"GBufferDepth"
	);
	constexpr xr_array<const char*, TiramisuGBufferLayout::TargetCount>
		GBufferNames = {
			"GBufferBaseColorAO",
			"GBufferNormalRoughnessMetallic",
			"GBufferEmissiveMaterialFlags",
			"GBufferVelocity"
		};
	for (u32 TargetIndex = 0;
		 TargetIndex < TiramisuGBufferLayout::TargetCount;
		 ++TargetIndex)
	{
		GBufferRenderTargets[TargetIndex] = new TiramisuRenderTarget2D(
			InWidth,
			InHeight,
			TiramisuGBufferLayout::TargetFormats[TargetIndex],
			{},
			GBufferNames[TargetIndex]
		);
	}

	ENQUEUE_RENDER_COMMAND(TiramisuRender::ResizeRenderTarget)([this,
																InOutputRenderTarget = OutputRenderTarget->RenderTargetResourceProxy,
																InDepthRenderTarget = DepthRenderTarget->RenderTargetResourceProxy]()
															   {
            CheckIsRenderThread();
            OutputRenderTarget_RenderThread = InOutputRenderTarget;
            DepthRenderTarget_RenderThread = InDepthRenderTarget; });
}

void TiramisuRender::EnableRenderThread()
{
	CheckIsGameThread();
	bRenderThreadEnable.store(true, std::memory_order_release);
}

void TiramisuRender::DeferDelete_RenderThread(Tiramisu::TiramisuDeferredDeletionQueue::FDeleteFunction Function)
{
	CheckIsRenderThread();
	// FrameIndex is the value of the last submitted main frame. A resource
	// retired while recording the next frame remains alive until that frame's
	// signal plus the full queued-frame window has completed.
	const u64 RetireFence = static_cast<u64>(FrameIndex) +
							QueuedFrames.size();
	VERIFY(DeferredDeletionQueue.Enqueue(RetireFence, std::move(Function)));
}

void TiramisuRender::RecordDrawStatistics_RenderThread(const u64 TriangleCount, const u64 LineCount)
{
	CheckIsRenderThread();
	StatisticsTracker.RecordDraw(TriangleCount, LineCount);
}

void TiramisuRender::RecordUploadStatistics_RenderThread(const u64 ByteCount)
{
	CheckIsRenderThread();
	StatisticsTracker.RecordUpload(ByteCount);
}

FRenderStatisticsSnapshot TiramisuRender::GetRenderStatistics() const
{
	std::scoped_lock Lock(StatisticsMutex);
	return PublishedStatistics;
}

FRenderResourceStatistics
TiramisuRender::CollectResourceStatistics_RenderThread() const
{
	CheckIsRenderThread();
	FRenderResourceStatistics Result;
	const auto AddBuffer = [&](const nri::Buffer* Buffer, const u64 Bytes = 0)
	{
		if (!Buffer)
		{
			return;
		}
		++Result.TrackedBufferCount;
		Result.TrackedBufferBytes += Bytes;
	};
	const auto AddTexture = [&](const nri::Texture* Texture, const u64 Bytes = 0)
	{
		if (!Texture)
		{
			return;
		}
		++Result.TrackedTextureCount;
		Result.TrackedTextureBytes += Bytes;
	};
	const auto AddDescriptor = [&](const void* Descriptor)
	{
		if (Descriptor)
		{
			++Result.TrackedDescriptorCount;
		}
	};

	AddBuffer(GlobalConstantBuffer, Align(sizeof(FXRayRenderConstantBuffer), GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset));
	AddDescriptor(GlobalConstantDescriptor);
	AddBuffer(
		DeferredLightingConstantBuffer,
		Align(
			sizeof(FTiramisuDeferredLightingConstants),
			GRenderDevice.DeviceDescription.memoryAlignment
				.constantBufferOffset
		)
	);
	AddDescriptor(DeferredLightingConstantDescriptor);
	if (UIPass)
	{
		constexpr u64 UiBufferBytes =
			16ull * 1024 * 6 * sizeof(FUIVertex);
		AddBuffer(UIPass->GeometryBuffer, UiBufferBytes);
		AddBuffer(UIPass->UploadBuffer, UiBufferBytes);
		if (UIPass->Pipeline)
		{
			++Result.TrackedPipelineCount;
		}
	}
	if (Pipeline)
	{
		++Result.TrackedPipelineCount;
	}
	if (DeferredLightingPipeline)
	{
		++Result.TrackedPipelineCount;
	}

	if (GRenderResourcesManager)
	{
		AddBuffer(GRenderResourcesManager->QuadGeometryBuffer);
		if (GRenderResourcesManager->MaterialGpuStorage)
		{
			Result.TrackedBufferCount += 3;
			Result.TrackedBufferBytes +=
				u64(TiramisuRenderMaterialGpuStorage::MaxDrawsPerFrame) *
					MaterialDrawGpuDataSize +
				u64(TiramisuRenderMaterialGpuStorage::MaxMaterialInstances) *
					MaterialInstanceGpuDataSize +
				TiramisuRenderMaterialGpuStorage::MaterialParameterCapacity;
			Result.TrackedDescriptorCount += 3;
		}
		if (GRenderResourcesManager->MaterialPipelineRegistry)
		{
			Result.TrackedPipelineCount += static_cast<u32>(
				GRenderResourcesManager->MaterialPipelineRegistry->GetActivePipelineCount()
			);
		}
	}

	const auto AddRenderTarget = [&](const TiramisuRenderTarget2D* Target,
		const u32 BytesPerPixel)
	{
		if (!Target || !Target->ResourceProxy)
		{
			return;
		}
		const nri::TextureDesc& Description = Target->TextureDescription;
		AddTexture(
			Target->ResourceProxy->Texture,
			static_cast<u64>(Description.width) *
				Description.height * BytesPerPixel
		);
		AddDescriptor(Target->ResourceProxy->Descriptor);
		if (Target->RenderTargetResourceProxy)
		{
			AddDescriptor(
				Target->RenderTargetResourceProxy->DescriptorAttachment
			);
		}
	};
	AddRenderTarget(OutputRenderTarget, 4);
	AddRenderTarget(DepthRenderTarget, 4);
	for (u32 TargetIndex = 0;
		 TargetIndex < TiramisuGBufferLayout::TargetCount;
		 ++TargetIndex)
	{
		AddRenderTarget(
			GBufferRenderTargets[TargetIndex],
			TiramisuGBufferLayout::TargetBytesPerPixel[TargetIndex]
		);
	}

	if (CurrentViewport)
	{
		const u64 SwapTextureBytes =
			static_cast<u64>(CurrentViewport->GetWidth()) *
			CurrentViewport->GetHeight() * 4;
		const u32 SwapTextureCount =
			CurrentViewport->GetSwapChainTextureCount();
		Result.TrackedTextureCount += SwapTextureCount;
		Result.TrackedTextureBytes +=
			SwapTextureBytes * SwapTextureCount;
		Result.TrackedDescriptorCount += SwapTextureCount;
	}
	Result.DeferredResourceCount = static_cast<u32>(
		DeferredDeletionQueue.Size()
	);
	return Result;
}

void TiramisuRender::DisableRenderThread()
{
	CheckIsGameThread();
	if (IsRenderThreadRunning())
	{
		ENQUEUE_RENDER_COMMAND(TiramisuRender::DisableRenderThread)([this]()
																	{
           CheckIsRenderThread();
           bRenderThreadEnable.store(false, std::memory_order_release); });
	}
	else
	{
		bRenderThreadEnable.store(false, std::memory_order_release);
	}
}

void TiramisuRender::DisableRenderThreadWithWaitStoping()
{
	CheckIsGameThread();
	if (IsRenderThreadRunning())
	{
		DisableRenderThread();
		Tiramisu::RenderCommands::FlushRenderCommands();
		Platform::WaitForSingleObject(RenderThread);
	}
}


void TiramisuRender::CreateGlobalConstantBuffer()
{
	CheckIsRenderThread();
	{ // Constant buffer
		nri::BufferDesc BufferDescription = {};
		BufferDescription.size = Align(sizeof(FXRayRenderConstantBuffer), GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset);
		BufferDescription.usage = nri::BufferUsageBits::CONSTANT_BUFFER;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateBuffer(*GRenderDevice.Device, BufferDescription, GlobalConstantBuffer));
	}
	{
		nri::ResourceGroupDesc ResourceGroupDescription = {};
		ResourceGroupDescription.memoryLocation = nri::MemoryLocation::HOST_UPLOAD;
		ResourceGroupDescription.bufferNum = 1;
		ResourceGroupDescription.buffers = &GlobalConstantBuffer;

		NRI_CHECK(GRenderDevice.HelperInterface.AllocateAndBindMemory(*GRenderDevice.Device, ResourceGroupDescription, &GlobalConstantBufferMemory));
	}

	{
		nri::BufferViewDesc BufferViewDescription = {};
		BufferViewDescription.buffer = GlobalConstantBuffer;
		BufferViewDescription.type = nri::BufferView::CONSTANT_BUFFER;
		BufferViewDescription.offset = 0;
		BufferViewDescription.size = Align(sizeof(FXRayRenderConstantBuffer), GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset);
		NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(BufferViewDescription, GlobalConstantDescriptor));
	}
	{
		NRI_CHECK(GRenderDevice.CoreInterface.AllocateDescriptorSets(*GRenderResourcesManager->GlobalDescriptorPool, *GRenderResourcesManager->GlobalPipelineLayout, 2, &GlobalConstantDescriptorSet, 1, 0));
		nri::UpdateDescriptorRangeDesc UpdateDescriptorRangeDescription = {GlobalConstantDescriptorSet, 0, 0, &GlobalConstantDescriptor, 1};
		GRenderDevice.CoreInterface.UpdateDescriptorRanges(&UpdateDescriptorRangeDescription, 1);
	}
}

void TiramisuRender::CreateDeferredLightingResources()
{
	CheckIsRenderThread();
	const u64 ConstantBufferSize = Align(
		sizeof(FTiramisuDeferredLightingConstants),
		GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset
	);

	nri::BufferDesc BufferDescription = {};
	BufferDescription.size = ConstantBufferSize;
	BufferDescription.usage = nri::BufferUsageBits::CONSTANT_BUFFER;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateBuffer(
		*GRenderDevice.Device,
		BufferDescription,
		DeferredLightingConstantBuffer
	));

	nri::ResourceGroupDesc ResourceGroupDescription = {};
	ResourceGroupDescription.memoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	ResourceGroupDescription.bufferNum = 1;
	ResourceGroupDescription.buffers = &DeferredLightingConstantBuffer;
	NRI_CHECK(GRenderDevice.HelperInterface.AllocateAndBindMemory(
		*GRenderDevice.Device,
		ResourceGroupDescription,
		&DeferredLightingConstantBufferMemory
	));

	nri::BufferViewDesc BufferViewDescription = {};
	BufferViewDescription.buffer = DeferredLightingConstantBuffer;
	BufferViewDescription.type = nri::BufferView::CONSTANT_BUFFER;
	BufferViewDescription.size = ConstantBufferSize;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(
		BufferViewDescription,
		DeferredLightingConstantDescriptor
	));

	NRI_CHECK(GRenderDevice.CoreInterface.AllocateDescriptorSets(
		*GRenderResourcesManager->GlobalDescriptorPool,
		*GRenderResourcesManager->GlobalPipelineLayout,
		2,
		&DeferredLightingDescriptorSet,
		1,
		0
	));
	const nri::UpdateDescriptorRangeDesc UpdateDescription = {
		DeferredLightingDescriptorSet,
		0,
		0,
		&DeferredLightingConstantDescriptor,
		1
	};
	GRenderDevice.CoreInterface.UpdateDescriptorRanges(
		&UpdateDescription, 1
	);
}

void TiramisuRender::UpdateDeferredLightingConstants_RenderThread()
{
	CheckIsRenderThread();
	auto* Constants = static_cast<FTiramisuDeferredLightingConstants*>(
		GRenderDevice.CoreInterface.MapBuffer(
			*DeferredLightingConstantBuffer,
			0,
			sizeof(FTiramisuDeferredLightingConstants)
		)
	);
	if (!Constants)
	{
		return;
	}

	Constants->InverseViewProjection.invert(DevicePtr->mFullTransform);
	Constants->CameraPosition = {
		DevicePtr->vCameraPosition.x,
		DevicePtr->vCameraPosition.y,
		DevicePtr->vCameraPosition.z,
		1.0f
	};
	Constants->LightDirectionAndIntensity = {
		0.35f, -0.9f, 0.25f, 4.0f
	};
	Constants->LightColorAndAmbientIntensity = {
		1.0f, 0.95f, 0.85f, 0.03f
	};
	Constants->PointLightPositionAndInverseRadiusSquared = {};
	Constants->BaseColorAmbientOcclusionIndex =
		GBufferRenderTargets[TiramisuGBufferLayout::GetTargetIndex(
			ETiramisuGBufferTarget::BaseColorAmbientOcclusion
		)]->ResourceProxy->GetOrCreateHeapID();
	Constants->NormalRoughnessMetallicIndex =
		GBufferRenderTargets[TiramisuGBufferLayout::GetTargetIndex(
			ETiramisuGBufferTarget::NormalRoughnessMetallic
		)]->ResourceProxy->GetOrCreateHeapID();
	Constants->EmissiveMaterialFlagsIndex =
		GBufferRenderTargets[TiramisuGBufferLayout::GetTargetIndex(
			ETiramisuGBufferTarget::EmissiveMaterialFlags
		)]->ResourceProxy->GetOrCreateHeapID();
	Constants->VelocityIndex =
		GBufferRenderTargets[TiramisuGBufferLayout::GetTargetIndex(
			ETiramisuGBufferTarget::Velocity
		)]->ResourceProxy->GetOrCreateHeapID();
	Constants->DepthIndex =
		DepthRenderTarget->ResourceProxy->GetOrCreateHeapID();
	Constants->SamplerIndex = 0;
	Constants->GBufferVersion = TiramisuGBufferLayout::Version;
	Constants->Padding = 0;

	GRenderDevice.CoreInterface.UnmapBuffer(
		*DeferredLightingConstantBuffer
	);
	StatisticsTracker.RecordUpload(
		sizeof(FTiramisuDeferredLightingConstants)
	);
}

void TiramisuRender::CreateDeferredLightingPipeline_RenderThread()
{
	CheckIsRenderThread();
	if (DeferredLightingPipeline)
	{
		return;
	}

	nri::InputAssemblyDesc InputAssemblyDescription = {};
	InputAssemblyDescription.topology = nri::Topology::TRIANGLE_LIST;

	nri::RasterizationDesc RasterizationDescription = {};
	RasterizationDescription.fillMode = nri::FillMode::SOLID;
	RasterizationDescription.cullMode = nri::CullMode::NONE;

	nri::ColorAttachmentDesc ColorAttachmentDescription = {};
	ColorAttachmentDescription.format = nri::Format::RGBA8_UNORM;
	ColorAttachmentDescription.colorWriteMask = nri::ColorWriteBits::RGBA;

	nri::OutputMergerDesc OutputMergerDescription = {};
	OutputMergerDescription.colors = &ColorAttachmentDescription;
	OutputMergerDescription.colorNum = 1;

	TiramisuShaderDefinesContainer ShaderDefines;
	nri::ShaderDesc ShaderDescriptions[2] = {};
	{
		const xr_vector<char>& ShaderCode =
			GRenderResourcesManager->GlobalShadersManager->GetShader(
				"fullscreen_triangle",
				EShaderType::Vertex,
				ShaderDefines
			);
		ShaderDescriptions[0].stage = nri::StageBits::VERTEX_SHADER;
		ShaderDescriptions[0].bytecode = ShaderCode.data();
		ShaderDescriptions[0].size = ShaderCode.size();
		ShaderDescriptions[0].entryPointName = "Main";
	}
	{
		const xr_vector<char>& ShaderCode =
			GRenderResourcesManager->GlobalShadersManager->GetShader(
				"deferred_directional_light",
				EShaderType::Pixel,
				ShaderDefines
			);
		ShaderDescriptions[1].stage = nri::StageBits::FRAGMENT_SHADER;
		ShaderDescriptions[1].bytecode = ShaderCode.data();
		ShaderDescriptions[1].size = ShaderCode.size();
		ShaderDescriptions[1].entryPointName = "Main";
	}

	nri::GraphicsPipelineDesc PipelineDescription = {};
	PipelineDescription.pipelineLayout =
		GRenderResourcesManager->GlobalPipelineLayout;
	PipelineDescription.inputAssembly = InputAssemblyDescription;
	PipelineDescription.rasterization = RasterizationDescription;
	PipelineDescription.outputMerger = OutputMergerDescription;
	PipelineDescription.shaders = ShaderDescriptions;
	PipelineDescription.shaderNum = 2;
	NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(
		*GRenderDevice.Device,
		PipelineDescription,
		DeferredLightingPipeline
	));
}

void TiramisuRender::UpdateGlobalConstantBuffer()
{
	CheckIsRenderThread();
	// Update constants
	if (FXRayRenderConstantBuffer* ConstantBuffer = (FXRayRenderConstantBuffer*)GRenderDevice.CoreInterface.MapBuffer(*GlobalConstantBuffer, 0, sizeof(FXRayRenderConstantBuffer)))
	{
		ConstantBuffer->SceneView = {(float)OutputRenderTarget->TextureDescription.width, (float)OutputRenderTarget->TextureDescription.height, 1.f / OutputRenderTarget->TextureDescription.width, 1.f / OutputRenderTarget->TextureDescription.height};
		ConstantBuffer->ViewProjection = DevicePtr->mFullTransform;
		ConstantBuffer->InverseViewProjection.invert(
			ConstantBuffer->ViewProjection
		);
		static const FRenderDeterministicTestPolicy DeterministicTest =
			ResolveRenderDeterministicTestPolicy(
				Core.Params ? Core.Params : ""
			);
		ConstantBuffer->CameraPositionAndTime =
			{DevicePtr->vCameraPosition.x, DevicePtr->vCameraPosition.y, DevicePtr->vCameraPosition.z, DeterministicTest.Enabled ? DeterministicTest.FixedShaderTimeSeconds : DevicePtr->fTimeGlobal};
		ConstantBuffer->DrawDataBufferIndex =
			GRenderResourcesManager->MaterialGpuStorage->GetDrawDataBufferIndex();
		ConstantBuffer->MaterialInstanceBufferIndex =
			GRenderResourcesManager->MaterialGpuStorage->GetMaterialInstanceBufferIndex();
		ConstantBuffer->MaterialParameterBufferIndex =
			GRenderResourcesManager->MaterialGpuStorage->GetMaterialParameterBufferIndex();
		ConstantBuffer->DefaultMaterialSamplerIndex = 0;
		ConstantBuffer->LightDataBufferIndex = 0;
		ConstantBuffer->LightDataOffset = 0;
		ConstantBuffer->LightCount = 0;
		ConstantBuffer->EnvironmentTextureIndex = UINT32_MAX;
		ConstantBuffer->SkinningPaletteBufferIndex = UINT32_MAX;
		std::fill_n(ConstantBuffer->MaterialGpuAbiPadding, 3, 0u);
		GRenderDevice.CoreInterface.UnmapBuffer(*GlobalConstantBuffer);
		StatisticsTracker.RecordUpload(sizeof(FXRayRenderConstantBuffer));
	}
}
