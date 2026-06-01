#pragma once
#include "Visual/XRayRenderVisual.h"
#include "TLegacySceneSector.h"
#include "TLegacyRenderGraph.h"


class TRenderLegacyScene
{
public:
                                                        ~TRenderLegacyScene ();
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
    const FLegacySceneShader&                           GetShaders          (uint32_t id)const { return Shaders[id]; }

    float                                               SsaDiscardThreshold = 0.f;
    float                                               GlodSsaStartThreshold = 0.f;
    float                                               GlodSsaEndThreshold = 0.f;
    float                                               PortalFadeSsaStartThreshold = 0.f;
    float                                               PortalFadeSsaEndThreshold = 0.f;
    nri::Buffer*                                        GeometryBuffer = nullptr;
  
private:
    
    void								                LoadBuffers			(CStreamReader* Reader);
    void								                LoadVisuals			(IReader* Reader);
    void								                LoadSectors			(IReader* Reader);
    void								                LoadSWIs			(CStreamReader* Reader);
    TLegacySceneSector*                                 GetSectorByRay      (const Fvector& Position, const Fvector& Direction);
    static EXRayLegacyLevelVertexType	                GetAndConvertFVF	(CStreamReader* Reader, uint32_t& OutSize);
    
    xr_vector<FLegacyVisualSceneVertexBuffer>                 VertexBuffers;
    xr_vector<FLegacyVisualSceneIndexBuffer>                  IndexBuffers;
    
    nri::Memory*                                        GeometryBufferMemory = nullptr;
    xr_vector<CDS0_RenderVisual*>                       Visuals;
    xr_vector<FLegacySceneShader>                       Shaders;
    xr_vector<xr_unique_ptr<TLegacyScenePortal>>        Portals;
    xr_vector<xr_unique_ptr<TLegacySceneSector>>        Sectors;
    TLegacyRenderGraph                                  RenderGraph;
    
    CDB::MODEL*                                         PortalsCollisionModel = nullptr;
    CDB::COLLIDER										PortalsCollider;  
    TLegacySceneSector*                                 LastSector = nullptr;
};
