#pragma once
#include "TPrimitiveSceneProxy.h"

class TMaterialRenderProxy;

class TStaticMeshRenderData
{
public:
                    TStaticMeshRenderData   ();
                    ~TStaticMeshRenderData  ();
    nri::Buffer*    GeometryBuffer = nullptr;
};

class TStaticMeshSceneProxy:public TPrimitiveSceneProxy
{
public:
                                TStaticMeshSceneProxy   ();
                                ~TStaticMeshSceneProxy  ();
    virtual bool                GetMeshBath             (uint32_t BathIndex, FRenderMeshBath&OutMeshBath) override;
    virtual u32                 GetNumMeshBatches       () const override;
    
    TStaticMeshRenderData*      RenderData = nullptr;
};
