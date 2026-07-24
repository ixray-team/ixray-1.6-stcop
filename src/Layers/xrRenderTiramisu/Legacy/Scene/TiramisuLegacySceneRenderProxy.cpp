#include "TiramisuLegacySceneRenderProxy.h"

TiramisuLegacySceneRenderProxy::TiramisuLegacySceneRenderProxy()
{
	CheckIsGameThread();
}

TiramisuLegacySceneRenderProxy::~TiramisuLegacySceneRenderProxy()
{
	CheckIsRenderThread();
}

bool TiramisuLegacySceneRenderProxy::GetMeshBatch(const u32 BatchIndex, FMeshBatch& OutMeshBatch)
{
	CheckIsRenderThread();
	if (MeshBatches.size() <= BatchIndex)
	{
		return false;
	}

	OutMeshBatch = MeshBatches[BatchIndex];
	OutMeshBatch.VertexBuffer.VertexBuffer = RenderData->GeometryBuffer;
	OutMeshBatch.IndexBuffer.IndexBuffer = RenderData->GeometryBuffer;
	return true;
}

u32 TiramisuLegacySceneRenderProxy::GetNumMeshBatches() const
{
	CheckIsRenderThread();
	return static_cast<u32>(MeshBatches.size());
}
