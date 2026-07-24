#ifndef TIRAMISU_MATERIAL_GPU_ABI
#define TIRAMISU_MATERIAL_GPU_ABI

#include "NRI.hlsl"

#define TIRAMISU_MATERIAL_GPU_ABI_VERSION 2u
#define TIRAMISU_MATERIAL_INSTANCE_GPU_DATA_SIZE 16u
#define TIRAMISU_MATERIAL_DRAW_GPU_DATA_SIZE 144u
#define TIRAMISU_MATERIAL_LIGHT_GPU_DATA_SIZE 64u
#define TIRAMISU_INVALID_DESCRIPTOR_INDEX 0xffffffffu

NRI_RESOURCE(cbuffer, GlobalConstants, b, 0, 2)
{
    float4 SceneView;
    float4x4 ViewProjectionWorldMatrix;
    float4 CameraPositionAndTime;
    uint DrawDataBufferIndex;
    uint MaterialInstanceBufferIndex;
    uint MaterialParameterBufferIndex;
    uint DefaultMaterialSamplerIndex;
    uint LightDataBufferIndex;
    uint LightDataOffset;
    uint LightCount;
    uint LightingFlags;
};

struct MaterialInstanceGpuData
{
    uint ParameterDataOffset;
    uint ParameterDataSize;
    uint LayoutHashLow;
    uint LayoutHashHigh;
};

MaterialInstanceGpuData LoadMaterialInstanceGpuData(uint MaterialInstanceIndex)
{
    ByteAddressBuffer InstanceTable = ResourceDescriptorHeap[MaterialInstanceBufferIndex];
    const uint4 Packed = InstanceTable.Load4(
        MaterialInstanceIndex * TIRAMISU_MATERIAL_INSTANCE_GPU_DATA_SIZE);

    MaterialInstanceGpuData Result;
    Result.ParameterDataOffset = Packed.x;
    Result.ParameterDataSize = Packed.y;
    Result.LayoutHashLow = Packed.z;
    Result.LayoutHashHigh = Packed.w;
    return Result;
}

struct MaterialDrawGpuData
{
    float4x4 LocalToWorld;
    float4x4 PreviousLocalToWorld;
    uint MaterialInstanceIndex;
    uint ObjectId;
    uint Flags;
};

float4x4 LoadMaterialGpuMatrix(ByteAddressBuffer Buffer, uint Offset)
{
    float4x4 Result;
    Result[0] = asfloat(Buffer.Load4(Offset + 0u));
    Result[1] = asfloat(Buffer.Load4(Offset + 16u));
    Result[2] = asfloat(Buffer.Load4(Offset + 32u));
    Result[3] = asfloat(Buffer.Load4(Offset + 48u));
    return Result;
}

MaterialDrawGpuData LoadMaterialDrawGpuData(uint DrawIndex)
{
    ByteAddressBuffer DrawTable = ResourceDescriptorHeap[DrawDataBufferIndex];
    const uint Offset = DrawIndex * TIRAMISU_MATERIAL_DRAW_GPU_DATA_SIZE;

    MaterialDrawGpuData Result;
    Result.LocalToWorld = LoadMaterialGpuMatrix(DrawTable, Offset);
    Result.PreviousLocalToWorld = LoadMaterialGpuMatrix(DrawTable, Offset + 64u);
    const uint4 Metadata = DrawTable.Load4(Offset + 128u);
    Result.MaterialInstanceIndex = Metadata.x;
    Result.ObjectId = Metadata.y;
    Result.Flags = Metadata.z;
    return Result;
}

struct MaterialLightGpuData
{
    float3 Position;
    float Range;
    float3 Direction;
    uint Type;
    float3 Color;
    float Intensity;
    float CosInnerCone;
    float CosOuterCone;
    uint Flags;
};

MaterialLightGpuData LoadMaterialLightGpuData(uint LightIndex)
{
    ByteAddressBuffer LightTable =
        ResourceDescriptorHeap[LightDataBufferIndex];
    const uint Offset = (LightDataOffset + LightIndex) *
        TIRAMISU_MATERIAL_LIGHT_GPU_DATA_SIZE;
    const uint4 PositionAndRange = LightTable.Load4(Offset);
    const uint4 DirectionAndType = LightTable.Load4(Offset + 16u);
    const uint4 ColorAndIntensity = LightTable.Load4(Offset + 32u);
    const uint4 ConeAndFlags = LightTable.Load4(Offset + 48u);

    MaterialLightGpuData Result;
    Result.Position = asfloat(PositionAndRange.xyz);
    Result.Range = asfloat(PositionAndRange.w);
    Result.Direction = asfloat(DirectionAndType.xyz);
    Result.Type = DirectionAndType.w;
    Result.Color = asfloat(ColorAndIntensity.xyz);
    Result.Intensity = asfloat(ColorAndIntensity.w);
    Result.CosInnerCone = asfloat(ConeAndFlags.x);
    Result.CosOuterCone = asfloat(ConeAndFlags.y);
    Result.Flags = ConeAndFlags.z;
    return Result;
}

#endif
