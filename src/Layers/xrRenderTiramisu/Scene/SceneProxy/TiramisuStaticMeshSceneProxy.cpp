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

bool TiramisuStaticMeshSceneProxy::GetMeshBatch(const u32 BatchIndex, FMeshBatch& OutMeshBatch)
{
    CheckIsRenderThread();
    if (!RenderData || LODIndex >= RenderData->LODResources.size())
        return false;

    const FStaticMeshLODResources& LOD = RenderData->LODResources[LODIndex];
    if (BatchIndex >= LOD.Sections.size())
        return false;

    const FStaticMeshSection& Section = LOD.Sections[BatchIndex];
    if (Section.MaterialSlot >= Materials.size() || !Materials[Section.MaterialSlot])
    {
        return false;
    }

    OutMeshBatch = {};
    OutMeshBatch.VertexBuffer = LOD.VertexBuffer;
    OutMeshBatch.IndexBuffer = LOD.IndexBuffer;
    OutMeshBatch.Material = Materials[Section.MaterialSlot];
    OutMeshBatch.VertexType = LOD.VertexType;
    OutMeshBatch.LODIndex = LODIndex;
    OutMeshBatch.MaterialSlot = Section.MaterialSlot;

    FMeshBatchElement Element;
    if (!BuildStaticMeshBatchElement(Section, Element))
        return false;
    OutMeshBatch.Elements.push_back(Element);
    return true;
}

u32 TiramisuStaticMeshSceneProxy::GetNumMeshBatches() const
{
    CheckIsRenderThread();
    if (!RenderData || LODIndex >= RenderData->LODResources.size())
        return 0;
    return static_cast<u32>(RenderData->LODResources[LODIndex].Sections.size());
}
