#pragma once

namespace nri
{
    struct VertexAttributeDesc;
}

struct FUIVertex 
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[3];
    
    Fvector position;
    u32     color;
    float   uv[2];
};

enum class ELegacyLevelVertexType:uint8_t
{
    BaseWithLightMap = 0,
    BaseWithLightColor = 1,
    VertexOnly = 2,
    MultipleUsageModel = 3,
};

struct FLegacyLevelVertex_BaseWithLightMap
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[6];
    Fvector position;
    u32     normal;
    u32     tangent;
    u32     binormal;
    u16     uv0[2];
    u16     uv1[2];
};

struct FLegacyLevelVertex_BaseWithLightColor
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[6];
    Fvector position;
    u32     normal;
    u32     tangent;
    u32     binormal;
    u32     color;
    u16     uv0[2];
};

struct FLegacyLevelVertex_OnlyVertex
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[1];
    Fvector position;
};

struct FLegacyLevelVertex_MultipleUsageModel
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[6];
    Fvector position;
    u32     normal;
    u32     tangent;
    u32     binormal;
    u16     uv0[4];
};
