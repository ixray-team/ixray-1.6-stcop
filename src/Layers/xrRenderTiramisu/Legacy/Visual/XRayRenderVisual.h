#pragma once

#include "TiramisuRenderTypes.h"
#include "Legacy/Scene/TiramisuLegacyScene.h"
class TiramisuRenderMaterialInterface;
class TiramisuLegacyScene;
struct FLegacySceneShader;
class TiramisuRenderTexture;

// Описание legacy vertex buffer визуала, зарегистрированного в новой сцене.
struct FLegacyVisualSceneVertexBuffer
{
	EVertexType VertexType = EVertexType::BaseWithLightColor;
	u32 Offset = 0;
	u32 Size = 0;
	u32 Stride = 0;
	u32 Count = 0;
};

// Описание legacy index buffer визуала, зарегистрированного в новой сцене.
struct FLegacyVisualSceneIndexBuffer
{
	u32 Offset = 0;
	u32 Size = 0;
	u32 Count = 0;
};

// Разрешённая legacy shader/texture пара и соответствующий новый material.
struct FLegacySceneShader
{
	shared_str LegacyShaderName;
	xr_vector<TiramisuRenderTexture*> Textures;
};

// Один готовый legacy draw item для передачи в scene proxy.
struct FLegacyVisualRenderItem
{
	FLegacyVisualSceneVertexBuffer SceneVertexBuffer;
	FLegacyVisualSceneIndexBuffer SceneIndexBuffer;

	u32 CountVertex = 0;
	u32 OffsetVertex = 0;

	u32 CountIndex = 0;
	u32 OffsetIndex = 0;

	TiramisuRenderMaterialInterface* Material = nullptr;

	nri::Buffer* VertexBuffer = nullptr;
	nri::Buffer* IndexBuffer = nullptr;

	class CDS0_RenderVisual* Owner = nullptr;
};

class CDS0_RenderVisual :
	public IRenderVisual
{
public:
	CDS0_RenderVisual();
	virtual ~CDS0_RenderVisual();
	virtual void Load(const char* N, IReader* data, u32 dwFlags);
	virtual void Copy(CDS0_RenderVisual* from);
	virtual void Depart() {};
	virtual void Spawn() {};
	virtual void Release() {}
	virtual u32 getType();
	virtual vis_data& getVisData();
	virtual shared_str getDebugName();
	virtual bool MakeRenderItem(float LOD, FLegacyVisualRenderItem& RenderItem);

public:
	vis_data Vis;
	u32 Type;
	shared_str DebugName;
	TiramisuLegacyScene* LegacyOwner = nullptr;
	TiramisuRenderMaterialInterface* Material = nullptr;
};
