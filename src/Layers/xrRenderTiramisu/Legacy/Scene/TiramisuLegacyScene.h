#pragma once

#include "TiramisuRenderTypes.h"
#include "Legacy/Visual/XRayRenderVisual.h"
#include "TiramisuLegacySceneSector.h"
#include "TiramisuLegacyRenderGraph.h"
#include "Resources/Materials/TiramisuRenderMaterialInstanceDynamic.h"
#include "Scene/SceneProxy/TiramisuStaticMeshSceneProxy.h"


class TiramisuLegacySceneRenderProxy;
class TiramisuStaticMeshSceneProxy;

// Слой совместимости, загружающий старые level/OGF данные в ресурсы нового renderer.
class TiramisuLegacyScene
{
public:
	~TiramisuLegacyScene();
	void LoadLevel(IReader* FileReader);
	void Clear();
	CDS0_RenderVisual* GetVisual(u32 id) const { return Visuals[id]; }
	TiramisuLegacyScenePortal* GetPortal(u32 id) const { return Portals[id].get(); }
	TiramisuLegacySceneSector* GetSector(u32 id) const { return Sectors[id].get(); }
	TiramisuLegacySceneSector* GetSector(Fvector Position);
	const TiramisuLegacyRenderGraph& GetRenderGraph() const { return RenderGraph; }
	void Calculate();

	const FLegacyVisualSceneVertexBuffer& GetVertexBuffer(u32 id) const { return VertexBuffers[id]; }
	const FLegacyVisualSceneIndexBuffer& GetIndexBuffer(u32 id) const { return IndexBuffers[id]; }
	TiramisuRenderMaterialInterface* GetShaders(u32 id) const { return Shaders[id]; }

	float SsaDiscardThreshold = 0.f;
	float GlodSsaStartThreshold = 0.f;
	float GlodSsaEndThreshold = 0.f;
	float PortalFadeSsaStartThreshold = 0.f;
	float PortalFadeSsaEndThreshold = 0.f;

	TiramisuLegacySceneRenderProxy* SceneRenderProxy = nullptr;
	TiramisuStaticMeshRenderData* StaticMeshRenderData = nullptr;

private:
	void LoadBuffers(CStreamReader* Reader);
	void LoadVisuals(IReader* Reader);
	void LoadSectors(IReader* Reader);
	void LoadSWIs(CStreamReader* Reader);
	TiramisuLegacySceneSector* GetSectorByRay(const Fvector& Position, const Fvector& Direction);
	static EVertexType GetAndConvertFVF(CStreamReader* Reader, u32& OutSize);

	xr_vector<FLegacyVisualSceneVertexBuffer> VertexBuffers;
	xr_vector<FLegacyVisualSceneIndexBuffer> IndexBuffers;

	xr_vector<CDS0_RenderVisual*> Visuals;
	xr_vector<TiramisuRenderMaterialInterface*> Shaders;
	xr_vector<xr_unique_ptr<TiramisuLegacyScenePortal>> Portals;
	xr_vector<xr_unique_ptr<TiramisuLegacySceneSector>> Sectors;
	TiramisuLegacyRenderGraph RenderGraph;

	CDB::MODEL* PortalsCollisionModel = nullptr;
	CDB::COLLIDER PortalsCollider;
	TiramisuLegacySceneSector* LastSector = nullptr;
};
