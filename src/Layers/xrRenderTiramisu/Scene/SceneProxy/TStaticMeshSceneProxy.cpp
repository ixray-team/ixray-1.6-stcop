#include "TStaticMeshSceneProxy.h"

TStaticMeshRenderData::TStaticMeshRenderData()
{
}

TStaticMeshRenderData::~TStaticMeshRenderData()
{
    if (GeometryBuffer)
    {
        GRenderDevice.CoreInterface.DestroyBuffer(GeometryBuffer);
    }
}

TStaticMeshSceneProxy::TStaticMeshSceneProxy()
{
}

TStaticMeshSceneProxy::~TStaticMeshSceneProxy()
{
}

bool TStaticMeshSceneProxy::GetMeshBath(uint32_t BathIndex,FRenderMeshBath& OutMeshBath)
{
    return false;
}

u32 TStaticMeshSceneProxy::GetNumMeshBatches() const
{
    return 0;
}
