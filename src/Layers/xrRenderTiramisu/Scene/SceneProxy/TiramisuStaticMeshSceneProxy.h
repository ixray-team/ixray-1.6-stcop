#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuPrimitiveSceneProxy.h"

class TiramisuMaterialRenderProxy;

// GPU-ресурсы и sections одного LOD static mesh.
struct FStaticMeshLODResources
{
	FMeshVertexBufferView VertexBuffer;
	FMeshIndexBufferView IndexBuffer;
	xr_vector<FStaticMeshSection> Sections;
	EVertexType VertexType = EVertexType::BaseWithLightColor;
};

// Владеет всеми LOD resources загруженного static mesh.
class TiramisuStaticMeshRenderData
{
public:
	TiramisuStaticMeshRenderData();
	~TiramisuStaticMeshRenderData();
	nri::Buffer* GeometryBuffer = nullptr;
	xr_vector<FStaticMeshLODResources> LODResources;
};

// Scene proxy static mesh, создающий draw batches по material slots.
class TiramisuStaticMeshSceneProxy : public TiramisuPrimitiveSceneProxy
{
public:
	TiramisuStaticMeshSceneProxy();
	~TiramisuStaticMeshSceneProxy();
	// Разрешает section выбранного LOD в готовый draw batch и material proxy.
	virtual const FMeshBatch* GetMeshBatch(u32 BatchIndex) override;
	virtual u32 GetNumMeshBatches() const override;

	TiramisuStaticMeshRenderData* RenderData = nullptr;
	xr_vector<TiramisuMaterialRenderProxy*> Materials;
	u32 LODIndex = 0;

private:
	// Static mesh пока строит один лёгкий view на section. Capacity Elements
	// переиспользуется и не создаёт heap allocation на каждом draw.
	FMeshBatch ResolvedMeshBatch;
};
