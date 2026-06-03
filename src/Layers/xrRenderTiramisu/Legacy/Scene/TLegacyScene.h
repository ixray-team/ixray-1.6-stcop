#pragma once
#include "Legacy/Visual/XRayRenderVisual.h"
#include "TLegacySceneSector.h"
#include "TLegacyRenderGraph.h"
#include "Resources/Materials/TRenderMaterialInstanceDynamic.h"
#include "Scene/SceneProxy/TStaticMeshSceneProxy.h"


class TLegacySceneRenderProxy;
class TStaticMeshSceneProxy;

class TLegacyScene
{
public:
                                                        ~TLegacyScene       ();
    void                                                LoadLevel           (IReader* FileReader);
    void                                                Clear               ();
    CDS0_RenderVisual*                                  GetVisual           (uint32_t id) const { return Visuals[id];}
    TLegacyScenePortal*                                 GetPortal           (uint32_t id) const { return Portals[id].get();}
    TLegacySceneSector*                                 GetSector           (uint32_t id) const { return Sectors[id].get();}
    TLegacySceneSector*                                 GetSector           (Fvector Position);
    const TLegacyRenderGraph&                           GetRenderGraph      () const { return RenderGraph; }
    void                                                Calculate           ();
    
    const FLegacyVisualSceneVertexBuffer&               GetVertexBuffer     (uint32_t id) const { return VertexBuffers[id]; }
    const FLegacyVisualSceneIndexBuffer&                GetIndexBuffer      (uint32_t id) const { return IndexBuffers[id]; }
    TRenderMaterialInterface*                           GetShaders          (uint32_t id)const { return Shaders[id]; }

    float                                               SsaDiscardThreshold = 0.f;
    float                                               GlodSsaStartThreshold = 0.f;
    float                                               GlodSsaEndThreshold = 0.f;
    float                                               PortalFadeSsaStartThreshold = 0.f;
    float                                               PortalFadeSsaEndThreshold = 0.f;
    
    TLegacySceneRenderProxy*                            SceneRenderProxy = nullptr;
    TStaticMeshRenderData*                              StaticMeshRenderData = nullptr;
  
private:
    
    void								                LoadBuffers			(CStreamReader* Reader);
    void								                LoadVisuals			(IReader* Reader);
    void								                LoadSectors			(IReader* Reader);
    void								                LoadSWIs			(CStreamReader* Reader);
    TLegacySceneSector*                                 GetSectorByRay      (const Fvector& Position, const Fvector& Direction);
    static EVertexType	                                GetAndConvertFVF	(CStreamReader* Reader, uint32_t& OutSize);
    
    xr_vector<FLegacyVisualSceneVertexBuffer>           VertexBuffers;
    xr_vector<FLegacyVisualSceneIndexBuffer>            IndexBuffers;
    
    xr_vector<CDS0_RenderVisual*>                       Visuals;
    xr_vector<TRenderMaterialInterface*>                Shaders;
    xr_vector<xr_unique_ptr<TLegacyScenePortal>>        Portals;
    xr_vector<xr_unique_ptr<TLegacySceneSector>>        Sectors;
    TLegacyRenderGraph                                  RenderGraph;
    
    CDB::MODEL*                                         PortalsCollisionModel = nullptr;
    CDB::COLLIDER										PortalsCollider;  
    TLegacySceneSector*                                 LastSector = nullptr;
};
