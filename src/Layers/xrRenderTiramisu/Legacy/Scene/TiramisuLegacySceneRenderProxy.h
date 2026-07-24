#pragma once

#include "TiramisuRenderTypes.h"
#include "Scene/SceneProxy/TiramisuPrimitiveSceneProxy.h"


// Scene proxy, преобразующий legacy render items в mesh batches Tiramisu.
class TiramisuLegacySceneRenderProxy : public TiramisuPrimitiveSceneProxy
{
public:
	TiramisuLegacySceneRenderProxy();
	~TiramisuLegacySceneRenderProxy();
	virtual bool GetMeshBatch(u32 BatchIndex, FMeshBatch& OutMeshBatch) override;
	virtual u32 GetNumMeshBatches() const override;

	TiramisuStaticMeshRenderData* RenderData = nullptr;
	xr_vector<FMeshBatch> MeshBatches;
};
