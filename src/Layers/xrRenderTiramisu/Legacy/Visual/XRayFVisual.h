#pragma once

#include "TiramisuRenderTypes.h"
#define VLOAD_SWI 0x10
#include "XRayRenderVisual.h"

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
	FLegacyVisualSceneIndexBuffer SceneIndexBuffer;

	u32 CountIndex;
	u32 OffsetIndex;
	u32 CountVertex;
	u32 OffsetVertex;
	u32 FVF;

	size_t CountMeshlet;
	size_t OffsetUniqueVertexIndices;
	size_t OffsetPrimitiveIndices;

protected:
	FSlideWindowItem nSWI;
};
