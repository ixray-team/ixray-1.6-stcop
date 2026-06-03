#pragma once
#include "Scene/SceneProxy/TPrimitiveSceneProxy.h"


class TLegacySceneRenderProxy:public TPrimitiveSceneProxy
{
public:
                                TLegacySceneRenderProxy   ();
                                ~TLegacySceneRenderProxy  ();
    virtual bool                GetMeshBath                 (uint32_t BathIndex, FRenderMeshBath&OutMeshBath) override;
    virtual u32                 GetNumMeshBatches           () const override;
    
    TStaticMeshRenderData*      RenderData = nullptr;
    xr_vector<FRenderMeshBath>  MeshBathes;
};
