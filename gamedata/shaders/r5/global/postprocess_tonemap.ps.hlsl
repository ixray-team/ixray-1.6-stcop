#include "common/NRI.hlsl"
#include "postprocess/ToneMapping.hlsl"

NRI_RESOURCE(cbuffer, ToneMappingConstants, b, 0, 2)
{
    uint ToneMappingSceneColorIndex;
    uint ToneMappingBloomIndex;
    uint ToneMappingSamplerIndex;
    float ToneMappingExposure;
    float ToneMappingBloomIntensity;
    float ToneMappingStartCompression;
    float ToneMappingDesaturation;
    float ToneMappingOutputGamma;
};

struct ToneMappingInput
{
    float4 Position : SV_Position;
    float2 TexCoord : TEXCOORD0;
};

float4 Main(ToneMappingInput Input) : SV_Target0
{
    Texture2D<float4> SceneColor =
        ResourceDescriptorHeap[NonUniformResourceIndex(ToneMappingSceneColorIndex)];
    Texture2D<float4> Bloom = ResourceDescriptorHeap[NonUniformResourceIndex(ToneMappingBloomIndex)];
    SamplerState LinearSampler = SamplerDescriptorHeap[NonUniformResourceIndex(ToneMappingSamplerIndex)];
    float3 HdrColor = SceneColor.SampleLevel(LinearSampler, Input.TexCoord, 0.0f).rgb;
    HdrColor += Bloom.SampleLevel(LinearSampler, Input.TexCoord, 0.0f).rgb * ToneMappingBloomIntensity;
    HdrColor *= max(ToneMappingExposure, 0.0f);
    float3 Mapped = TiramisuCommerceToneMap(HdrColor,
        saturate(ToneMappingStartCompression), max(ToneMappingDesaturation, 0.0f));
    if (ToneMappingOutputGamma > 0.0f)
        Mapped = pow(saturate(Mapped), rcp(ToneMappingOutputGamma));
    return float4(saturate(Mapped), 1.0f);
}
