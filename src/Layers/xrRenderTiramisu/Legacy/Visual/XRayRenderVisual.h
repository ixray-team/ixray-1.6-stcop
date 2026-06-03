#pragma once
#include "Legacy/Scene/TLegacyScene.h"
class TRenderMaterialInterface;
class TLegacyScene;
struct FLegacySceneShader;
class TRenderTexture;

struct FLegacyVisualSceneVertexBuffer
{
	EVertexType		VertexType = EVertexType::BaseWithLightColor;
	uint32_t                    Offset = 0;
	uint32_t                    Size = 0;
	uint32_t                    Stride = 0;
	uint32_t                    Count = 0;
};

struct FLegacyVisualSceneIndexBuffer
{
	uint32_t                    Offset = 0;
	uint32_t                    Size = 0;
	uint32_t                    Count = 0;
};

struct FLegacySceneShader
{
	shared_str                  LegacyShaderName;
	xr_vector<TRenderTexture*>		Textures;
};

struct FLegacyVisualRenderItem
{
	FLegacyVisualSceneVertexBuffer SceneVertexBuffer;
	FLegacyVisualSceneIndexBuffer  SceneIndexBuffer;
	
	uint32_t 		CountVertex = 0;
	uint32_t 		OffsetVertex = 0;
	
	uint32_t		CountIndex = 0;
	uint32_t		OffsetIndex = 0;
	
	TRenderMaterialInterface*	Material = nullptr;
	
	nri::Buffer*	VertexBuffer = nullptr;
	nri::Buffer*	IndexBuffer = nullptr;
	
	class CDS0_RenderVisual* Owner = nullptr;
};

class CDS0_RenderVisual :
	public IRenderVisual
{
public:
								CDS0_RenderVisual	();
	virtual						~CDS0_RenderVisual	();
	virtual void				Load				(const char* N, IReader* data, u32 dwFlags);
	virtual void				Copy				(CDS0_RenderVisual* from);
	virtual void				Depart				() {};
	virtual void				Spawn				() {};
	virtual void				Release				() {}
	virtual u32					getType				();
	virtual vis_data& 			getVisData			();
	virtual shared_str			getDebugName		() ;
	virtual bool				MakeRenderItem		(float LOD,FLegacyVisualRenderItem& RenderItem);

public:
	vis_data Vis;
	u32 Type;
	shared_str DebugName;
	TLegacyScene* LegacyOwner = nullptr;
	TRenderMaterialInterface* Material = nullptr;
	
};
