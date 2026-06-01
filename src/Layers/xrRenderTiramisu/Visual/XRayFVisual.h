#pragma once
#define VLOAD_SWI 0x10
#include "XRayRenderVisual.h"
#include "Resources/LegacyScene/TRenderLegacyScene.h"

class CDS0_FVisual :
	public CDS0_RenderVisual
{
public:
	CDS0_FVisual();
	virtual ~CDS0_FVisual();
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* from);
	virtual bool MakeRenderItem(float LOD, FLegacyVisualRenderItem& RenderItem) override;

	FLegacyVisualSceneVertexBuffer SceneVertexBuffer;
	FLegacyVisualSceneIndexBuffer  SceneIndexBuffer;
	
	uint32_t CountIndex;
	uint32_t OffsetIndex;
	uint32_t CountVertex;
	uint32_t OffsetVertex;
	u32 FVF;

	size_t CountMeshlet;
	size_t OffsetUniqueVertexIndices;
	size_t OffsetPrimitiveIndices;

protected:
	FSlideWindowItem nSWI;

};
