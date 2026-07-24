#pragma once

#include "../../../xrCore/xrCore.h"

#include <string_view>

inline constexpr xr_string_view EditorViewportSceneShaderSource = R"(
#include "NRI.hlsl"

struct EditorDrawConstants
{
    float4x4 LocalToWorld;
    float4x4 ViewProjection;
    uint2 MaterialSlot;
    uint InstanceFlags;
    uint Padding;
};

NRI_ROOT_CONSTANTS(EditorDrawConstants, gEditorDrawConstants, 0, 0);

struct VertexInput
{
    float3 Position : POSITION0;
    float3 Normal : NORMAL0;
};

struct PixelInput
{
    float4 Position : SV_Position;
    float3 WorldNormal : NORMAL0;
    nointerpolation uint MaterialHash : TEXCOORD0;
    nointerpolation uint InstanceFlags : TEXCOORD1;
};

PixelInput VSMain(VertexInput Input)
{
    PixelInput Output;
    const float4 WorldPosition = mul(gEditorDrawConstants.LocalToWorld,
        float4(Input.Position, 1.0f));
    Output.Position = mul(gEditorDrawConstants.ViewProjection, WorldPosition);
    Output.WorldNormal = normalize(mul(
        (float3x3)gEditorDrawConstants.LocalToWorld, Input.Normal));
    Output.MaterialHash = gEditorDrawConstants.MaterialSlot.x ^
        (gEditorDrawConstants.MaterialSlot.y * 0x9e3779b9u);
    Output.InstanceFlags = gEditorDrawConstants.InstanceFlags;
    return Output;
}

float4 PSMain(PixelInput Input) : SV_Target0
{
    const float3 NormalColor = abs(normalize(Input.WorldNormal));
    const float3 MaterialColor = float3(
        float((Input.MaterialHash >> 0u) & 255u),
        float((Input.MaterialHash >> 8u) & 255u),
        float((Input.MaterialHash >> 16u) & 255u)) / 255.0f;
    float3 Color = 0.08f + NormalColor * 0.54f + MaterialColor * 0.26f;
    if ((Input.InstanceFlags & 1u) != 0u)
        Color = lerp(Color, float3(1.0f, 0.55f, 0.08f), 0.38f);
    return float4(Color, 1.0f);
}

struct DebugVertexInput
{
    float3 Position : POSITION0;
    float4 Color : COLOR0;
};

struct DebugPixelInput
{
    float4 Position : SV_Position;
    float4 Color : COLOR0;
};

DebugPixelInput VSDebug(DebugVertexInput Input)
{
    DebugPixelInput Output;
    Output.Position = mul(gEditorDrawConstants.ViewProjection,
        float4(Input.Position, 1.0f));
    Output.Color = Input.Color;
    return Output;
}

float4 PSDebug(DebugPixelInput Input) : SV_Target0
{
    return Input.Color;
}

DebugPixelInput VSOverlay(DebugVertexInput Input)
{
    DebugPixelInput Output;
    Output.Position = float4(Input.Position.xy, 0.0f, 1.0f);
    Output.Color = Input.Color;
    return Output;
}
)";
