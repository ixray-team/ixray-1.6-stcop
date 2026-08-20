#include "TiramisuStaticMeshSceneProxy.h"

TiramisuStaticMeshRenderData::TiramisuStaticMeshRenderData()
{
	CheckIsGameThread();
}

TiramisuStaticMeshRenderData::~TiramisuStaticMeshRenderData()
{
	CheckIsRenderThread();
	if (GeometryBuffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(GeometryBuffer);
	}
}

TiramisuStaticMeshSceneProxy::TiramisuStaticMeshSceneProxy()
{
	CheckIsGameThread();
}

TiramisuStaticMeshSceneProxy::~TiramisuStaticMeshSceneProxy()
{
	CheckIsRenderThread();
}

const FMeshBatch* TiramisuStaticMeshSceneProxy::GetMeshBatch(
	const u32 BatchIndex
)
{
	CheckIsRenderThread();
	if (!RenderData || LODIndex >= RenderData->LODResources.size())
	{
		return nullptr;
	}

	const FStaticMeshLODResources& LOD = RenderData->LODResources[LODIndex];
	if (BatchIndex >= LOD.Sections.size())
	{
		return nullptr;
	}

	const FStaticMeshSection& Section = LOD.Sections[BatchIndex];
	if (Section.MaterialSlot >= Materials.size() || !Materials[Section.MaterialSlot])
	{
		return nullptr;
	}

	ResolvedMeshBatch.Elements.clear();
	ResolvedMeshBatch.VertexBuffer = LOD.VertexBuffer;
	ResolvedMeshBatch.IndexBuffer = LOD.IndexBuffer;
	ResolvedMeshBatch.Material = Materials[Section.MaterialSlot];
	ResolvedMeshBatch.VertexType = LOD.VertexType;
	ResolvedMeshBatch.LODIndex = LODIndex;
	ResolvedMeshBatch.MaterialSlot = Section.MaterialSlot;

	FMeshBatchElement Element;
	if (!BuildStaticMeshBatchElement(Section, Element))
	{
		return nullptr;
	}
	ResolvedMeshBatch.Elements.push_back(Element);
	return &ResolvedMeshBatch;
}

u32 TiramisuStaticMeshSceneProxy::GetNumMeshBatches() const
{
	CheckIsRenderThread();
	if (!RenderData || LODIndex >= RenderData->LODResources.size())
	{
		return 0;
	}
	return static_cast<u32>(RenderData->LODResources[LODIndex].Sections.size());
}
