#ifndef TIRAMISU_MATERIAL_PASS_COMMON
#define TIRAMISU_MATERIAL_PASS_COMMON

struct MaterialPassPixelInput
{
    float4 Position : SV_Position;
    float2 TexCoord0 : TEXCOORD0;
    float4 VertexColor : COLOR0;
    float3 WorldNormal : NORMAL0;
    float3 WorldPosition : TEXCOORD1;
    float4 CurrentClipPosition : TEXCOORD2;
    float4 PreviousClipPosition : TEXCOORD3;
    nointerpolation uint MaterialInstanceIndex : TEXCOORD4;
    float2 TexCoord1 : TEXCOORD5;
    nointerpolation uint MaterialDrawFlags : TEXCOORD6;
};

MaterialContext BuildMaterialContext(MaterialPassPixelInput Input)
{
    MaterialContext Context;
    Context.TexCoord0 = Input.TexCoord0;
    Context.TexCoord1 = Input.TexCoord1;
    Context.VertexColor = Input.VertexColor;
    Context.WorldNormal = normalize(Input.WorldNormal);
    Context.WorldPosition = Input.WorldPosition;
    Context.CameraPosition = CameraPositionAndTime.xyz;
    const float3 CameraDelta = Context.CameraPosition - Context.WorldPosition;
    Context.CameraVector = CameraDelta * rsqrt(max(dot(CameraDelta, CameraDelta), 1.0e-8f));
    Context.Time = CameraPositionAndTime.w;
    return Context;
}

MaterialInputs EvaluateMaterialPass(MaterialPassPixelInput Input)
{
    MaterialInputs Result;
    const MaterialContext Context = BuildMaterialContext(Input);
    const MaterialParameters Parameters = LoadMaterialParametersForInstance(Input.MaterialInstanceIndex);
    EvaluateMaterial(Context, Parameters, Result);
    return Result;
}

void ApplyMaterialOpacityMask(MaterialInputs Inputs)
{
#if MATERIAL_BLEND_MASKED
    clip(Inputs.OpacityMask - MATERIAL_OPACITY_MASK_CLIP_VALUE);
#endif
}

#endif
