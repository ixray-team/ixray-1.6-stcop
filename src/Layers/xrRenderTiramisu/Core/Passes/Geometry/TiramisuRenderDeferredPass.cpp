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

void TiramisuRenderDeferredPass::Prepare_RenderThread()
{
	CheckIsRenderThread();
	PreparedDraws.clear();

	xr_vector<TiramisuPrimitiveSceneProxy*>& RenderSceneProxies =
		GRenderResourcesManager->RenderScene->RenderSceneProxies;
	u32 ObjectId = 0;
	for (TiramisuPrimitiveSceneProxy*& SceneProxy : RenderSceneProxies)
	{
		for (u32 i = 0; i < SceneProxy->GetNumMeshBatches(); ++i)
		{
			const FMeshBatch* MeshBatch = SceneProxy->GetMeshBatch(i);
			if (!MeshBatch || !MeshBatch->Material ||
				!MeshBatch->VertexBuffer.VertexBuffer ||
				!MeshBatch->IndexBuffer.IndexBuffer)
			{
				continue;
			}

			const u32 MaterialInstanceIndex =
				GRenderResourcesManager->MaterialGpuStorage->GetOrCreateMaterial_RenderThread(
					*MeshBatch->Material
				);
			if (MaterialInstanceIndex == INDEX_NONE)
			{
				continue;
			}

			auto PassProxy = MeshBatch->Material->ResolvePass(
				EMaterialPass::GBuffer, MeshBatch->VertexType
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
			for (const FMeshBatchElement& Element : MeshBatch->Elements)
			{
				const auto DrawData = MakeIdentityDrawData(MaterialInstanceIndex, ObjectId++);
				const u32 DrawIndex =
					GRenderResourcesManager->MaterialGpuStorage->AddDraw_RenderThread(DrawData);
				if (DrawIndex == INDEX_NONE)
				{
					break;
				}
				FPreparedDraw& Prepared = PreparedDraws.emplace_back();
				Prepared.Pipeline = Pipeline;
				Prepared.IndexBuffer = MeshBatch->IndexBuffer.IndexBuffer;
				Prepared.IndexBufferOffset = MeshBatch->IndexBuffer.Offset;
				Prepared.IndexType = MeshBatch->IndexBuffer.IndexType;
				Prepared.VertexBuffer = MeshBatch->VertexBuffer.VertexBuffer;
				Prepared.VertexBufferOffset = MeshBatch->VertexBuffer.Offset;
				Prepared.VertexStride = MeshBatch->VertexBuffer.Stride;
				Prepared.Draw = {
					Element.CountIndex,
					1,
					Element.OffsetIndex,
					Element.OffsetVertex,
					DrawIndex
				};
			}
		}
	}
}

void TiramisuRenderDeferredPass::Render(
	nri::CommandBuffer& CurrentCommandBuffer
)
{
	CheckIsRenderThread();
	GRenderDevice.CoreInterface.CmdBeginAnnotation(
		CurrentCommandBuffer,
		"DeferredPass",
		nri::BGRA_UNUSED
	);

	for (const FPreparedDraw& Prepared : PreparedDraws)
	{
		GRenderDevice.CoreInterface.CmdSetPipeline(
			CurrentCommandBuffer,
			*Prepared.Pipeline
		);
		GRenderDevice.CoreInterface.CmdSetIndexBuffer(
			CurrentCommandBuffer,
			*Prepared.IndexBuffer,
			Prepared.IndexBufferOffset,
			Prepared.IndexType
		);
		const nri::VertexBufferDesc VertexBufferDescription = {
			Prepared.VertexBuffer,
			Prepared.VertexBufferOffset,
			Prepared.VertexStride
		};
		GRenderDevice.CoreInterface.CmdSetVertexBuffers(
			CurrentCommandBuffer,
			0,
			&VertexBufferDescription,
			1
		);
		GRenderDevice.CoreInterface.CmdDrawIndexed(
			CurrentCommandBuffer,
			Prepared.Draw
		);
		if (GRender)
		{
			GRender->RecordDrawStatistics_RenderThread(
				Prepared.Draw.indexNum / 3
			);
		}
	}
	GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
}
