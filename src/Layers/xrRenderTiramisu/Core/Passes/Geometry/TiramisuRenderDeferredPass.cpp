#include "TiramisuRenderDeferredPass.h"

#include "Core/TiramisuRender.h"
#include "Legacy/Scene/TiramisuLegacyRenderGraph.h"
#include "Legacy/Scene/TiramisuLegacyScene.h"
#include "Legacy/Visual/XRayRenderVisual.h"
#include "Resources/Materials/TiramisuRenderMaterialGpuStorage.h"
#include "Resources/Materials/TiramisuRenderMaterialPipelineRegistry.h"
#include "Resources/Materials/Proxy/TiramisuMaterialRenderProxy.h"
#include "Scene/TiramisuRenderScene.h"

namespace
{
FMaterialDrawGpuData MakeIdentityDrawData(
	const u32 MaterialInstanceIndex, const u32 ObjectId
)
{
	FMaterialDrawGpuData Result;
	for (u32 Axis = 0; Axis < 4; ++Axis)
	{
		Result.LocalToWorld[Axis * 4 + Axis] = 1.0f;
		Result.PreviousLocalToWorld[Axis * 4 + Axis] = 1.0f;
	}
	Result.MaterialInstanceIndex = MaterialInstanceIndex;
	Result.ObjectId = ObjectId;
	return Result;
}
} // namespace

TiramisuRenderDeferredPass::TiramisuRenderDeferredPass()
{
	CheckIsRenderThread();
}

TiramisuRenderDeferredPass::~TiramisuRenderDeferredPass()
{
	CheckIsRenderThread();
}

void TiramisuRenderDeferredPass::Render(nri::CommandBuffer& CurrentCommandBuffer)
{
	CheckIsRenderThread();
	GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer, "DeferredPass", nri::BGRA_UNUSED);

	xr_vector<TiramisuPrimitiveSceneProxy*>& RenderSceneProxies =
		GRenderResourcesManager->RenderScene->RenderSceneProxies;
	u32 ObjectId = 0;
	for (TiramisuPrimitiveSceneProxy*& SceneProxy : RenderSceneProxies)
	{
		for (u32 i = 0; i < SceneProxy->GetNumMeshBatches(); ++i)
		{
			FMeshBatch MeshBatch;
			if (!SceneProxy->GetMeshBatch(i, MeshBatch) || !MeshBatch.Material ||
				!MeshBatch.VertexBuffer.VertexBuffer || !MeshBatch.IndexBuffer.IndexBuffer)
			{
				continue;
			}

			const u32 MaterialInstanceIndex =
				GRenderResourcesManager->MaterialGpuStorage->GetOrCreateMaterial_RenderThread(
					*MeshBatch.Material
				);
			if (MaterialInstanceIndex == INDEX_NONE)
			{
				continue;
			}

			auto PassProxy = MeshBatch.Material->ResolvePass(
				EMaterialPass::GBuffer, MeshBatch.VertexType
			);
			if (!PassProxy || !PassProxy->IsValid())
			{
				continue;
			}
			PassProxy->MaterialInstanceIndex = MaterialInstanceIndex;

			nri::Pipeline* Pipeline = GRenderResourcesManager->MaterialPipelineRegistry->ResolvePipeline_RenderThread(PassProxy->Pipeline);
			if (!Pipeline)
			{
				continue;
			}
			GRenderDevice.CoreInterface.CmdSetPipeline(CurrentCommandBuffer, *Pipeline);

			GRenderDevice.CoreInterface.CmdSetIndexBuffer(CurrentCommandBuffer, *MeshBatch.IndexBuffer.IndexBuffer, MeshBatch.IndexBuffer.Offset, MeshBatch.IndexBuffer.IndexType);

			nri::VertexBufferDesc VertexBufferDescription = {};
			VertexBufferDescription.buffer = MeshBatch.VertexBuffer.VertexBuffer;
			VertexBufferDescription.offset = MeshBatch.VertexBuffer.Offset;
			VertexBufferDescription.stride = MeshBatch.VertexBuffer.Stride;
			GRenderDevice.CoreInterface.CmdSetVertexBuffers(
				CurrentCommandBuffer, 0, &VertexBufferDescription, 1
			);

			for (const FMeshBatchElement& Element : MeshBatch.Elements)
			{
				const auto DrawData = MakeIdentityDrawData(MaterialInstanceIndex, ObjectId++);
				const u32 DrawIndex =
					GRenderResourcesManager->MaterialGpuStorage->AddDraw_RenderThread(DrawData);
				if (DrawIndex == INDEX_NONE)
				{
					break;
				}
				GRenderDevice.CoreInterface.CmdDrawIndexed(CurrentCommandBuffer, {Element.CountIndex, 1, Element.OffsetIndex, Element.OffsetVertex, DrawIndex});
				if (GRender)
				{
					GRender->RecordDrawStatistics_RenderThread(
						Element.CountIndex / 3
					);
				}
			}
		}
	}
	GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
}
