#pragma once

struct FRenderMeshBathVertexBuffer
{
    nri::Buffer*            VertexBuffer = nullptr;
    uint32_t                Offset = 0;
    uint32_t                Size = 0;
    uint32_t                Stride = 0;
    uint32_t                Count = 0;
};

struct FRenderMeshBathIndexBuffer
{
    nri::Buffer*    IndexBuffer = nullptr;
    nri::IndexType  IndexType = nri::IndexType::UINT16;
    uint32_t        Offset = 0;
    uint32_t        Size = 0;
    uint32_t        Count = 0;
};


struct FRenderMeshBathElement
{
    uint32_t    CountVertex = 0;
    uint32_t    OffsetVertex = 0;
    uint32_t    CountIndex = 0;
    uint32_t    OffsetIndex = 0;
};

struct FRenderMeshBath
{
    xr_vector<FRenderMeshBathElement>   Elements;
    FRenderMeshBathVertexBuffer         VertexBuffer;
    FRenderMeshBathIndexBuffer          IndexBuffer;
    class TMaterialRenderProxy*         Material = nullptr;
    EVertexType                         VertexType = EVertexType::BaseWithLightColor;
};


class TPrimitiveSceneProxy
{
public:
                    TPrimitiveSceneProxy    ();
    virtual         ~TPrimitiveSceneProxy   ();
    virtual u32     GetNumMeshBatches       () const = 0;
    virtual bool    GetMeshBath             (uint32_t BathIndex, FRenderMeshBath&OutMeshBath) = 0;
    bool            bNeedRemove = false;
};
