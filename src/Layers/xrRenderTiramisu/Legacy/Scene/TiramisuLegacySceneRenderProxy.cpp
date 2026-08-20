#include "TiramisuLegacySceneRenderProxy.h"

TiramisuLegacySceneRenderProxy::TiramisuLegacySceneRenderProxy()
{
	CheckIsGameThread();
}

TiramisuLegacySceneRenderProxy::~TiramisuLegacySceneRenderProxy()
{
	CheckIsRenderThread();
}

const FMeshBatch* TiramisuLegacySceneRenderProxy::GetMeshBatch(
	const u32 BatchIndex
)
{
	CheckIsRenderThread();
	if (!RenderData || MeshBatches.size() <= BatchIndex)
	{
		return nullptr;
	}

	FMeshBatch& MeshBatch = MeshBatches[BatchIndex];
	MeshBatch.VertexBuffer.VertexBuffer = RenderData->GeometryBuffer;
	MeshBatch.IndexBuffer.IndexBuffer = RenderData->GeometryBuffer;
	return &MeshBatch;
}

u32 TiramisuLegacySceneRenderProxy::GetNumMeshBatches() const
{
	CheckIsRenderThread();
	return static_cast<u32>(MeshBatches.size());
}
