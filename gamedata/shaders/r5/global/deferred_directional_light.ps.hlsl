#include "deferred/DeferredLightingCommon.hlsl"

float4 Main(DeferredFullscreenInput Input) : SV_Target0
{
    const int2 PixelPosition = int2(Input.Position.xy);
    const TiramisuGBufferData GBuffer = LoadDeferredGBuffer(PixelPosition);
    const float DeviceDepth = LoadDeferredDepth(PixelPosition);
    const float3 WorldPosition = ReconstructDeferredWorldPosition(Input.TexCoord, DeviceDepth);
    const float3 ViewDirection = normalize(DeferredCameraPosition.xyz - WorldPosition);
    const float3 LightDirection = normalize(-DeferredLightDirectionAndIntensity.xyz);
    const float3 Radiance = DeferredLightColorAndAmbientIntensity.rgb *
        max(DeferredLightDirectionAndIntensity.w, 0.0f);
    const TiramisuPbrSurface Surface = MakeDeferredPbrSurface(GBuffer);
    const float3 Direct = EvaluateTiramisuDirectLight(Surface, ViewDirection, LightDirection, Radiance);
    const float3 Ambient = Surface.BaseColor * Surface.AmbientOcclusion *
        max(DeferredLightColorAndAmbientIntensity.w, 0.0f);
    return float4(Direct + Ambient + Surface.Emissive, 1.0f);
}
