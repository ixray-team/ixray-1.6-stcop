#pragma once

#include "TiramisuRenderTypes.h"

#include "Scene/StaticMesh/TStaticMeshTypes.h"

// Описание vertex buffer, offset и stride одного mesh batch.
struct FMeshVertexBufferView
{
    nri::Buffer*            VertexBuffer = nullptr;
    u32                Offset = 0;
    u32                Size = 0;
    u32                Stride = 0;
    u32                Count = 0;
};

// Описание index buffer, offset и index format одного mesh batch.
struct FMeshIndexBufferView
{
    nri::Buffer*    IndexBuffer = nullptr;
    nri::IndexType  IndexType = nri::IndexType::UINT16;
    u32        Offset = 0;
    u32        Size = 0;
    u32        Count = 0;
};


// Полное описание одного draw batch без владения GPU-ресурсами.
struct FMeshBatch
{
    xr_vector<FMeshBatchElement> Elements;
    FMeshVertexBufferView VertexBuffer;
    FMeshIndexBufferView IndexBuffer;
    // Базовый render-thread proxy материала для разрешения pass pipeline и параметров.
    class TiramisuMaterialRenderProxy* Material = nullptr;
    EVertexType VertexType = EVertexType::BaseWithLightColor;
    u32 LODIndex = 0;
    u32 MaterialSlot = 0;
};


// Базовый render-thread proxy видимого примитива сцены.
class TiramisuPrimitiveSceneProxy
{
public:
                    TiramisuPrimitiveSceneProxy    ();
    virtual         ~TiramisuPrimitiveSceneProxy   ();
    virtual u32     GetNumMeshBatches       () const = 0;
    virtual bool    GetMeshBatch            (u32 BatchIndex, FMeshBatch& OutMeshBatch) = 0;
    bool            bNeedRemove = false;
};
