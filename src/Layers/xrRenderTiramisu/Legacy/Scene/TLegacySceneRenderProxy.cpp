#include "TLegacySceneRenderProxy.h"

TLegacySceneRenderProxy::TLegacySceneRenderProxy()
{
}

TLegacySceneRenderProxy::~TLegacySceneRenderProxy()
{
}

bool TLegacySceneRenderProxy::GetMeshBath(uint32_t BathIndex, FRenderMeshBath& OutMeshBath)
{
    if (MeshBathes.size() <= BathIndex)
    {
        return false;
    }
    
    OutMeshBath = MeshBathes[BathIndex];
    OutMeshBath.VertexBuffer.VertexBuffer = RenderData->GeometryBuffer;
    OutMeshBath.IndexBuffer.IndexBuffer =  RenderData->GeometryBuffer;
    return true;
}

u32 TLegacySceneRenderProxy::GetNumMeshBatches() const
{
    return MeshBathes.size();
}
