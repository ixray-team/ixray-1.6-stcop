#include "deferred/DeferredLightingCommon.hlsl"

float4 Main(DeferredFullscreenInput Input) : SV_Target0
{
    const int2 PixelPosition = int2(Input.Position.xy);
    const TiramisuGBufferData GBuffer = LoadDeferredGBuffer(PixelPosition);
    const float DeviceDepth = LoadDeferredDepth(PixelPosition);
    const float3 WorldPosition = ReconstructDeferredWorldPosition(Input.TexCoord, DeviceDepth);
    const float3 ToLight = DeferredPointLightPositionAndInverseRadiusSquared.xyz - WorldPosition;
    const float DistanceSquared = dot(ToLight, ToLight);
    const float3 LightDirection = ToLight * rsqrt(max(DistanceSquared, 1.0e-6f));
    const float3 ViewDirection = normalize(DeferredCameraPosition.xyz - WorldPosition);
    const float Attenuation = ComputeTiramisuPointAttenuation(DistanceSquared,
        max(DeferredPointLightPositionAndInverseRadiusSquared.w, 0.0f));
    const float3 Radiance = DeferredLightColorAndAmbientIntensity.rgb *
        max(DeferredLightDirectionAndIntensity.w, 0.0f) * Attenuation;
    const TiramisuPbrSurface Surface = MakeDeferredPbrSurface(GBuffer);
    return float4(EvaluateTiramisuDirectLight(Surface, ViewDirection, LightDirection, Radiance), 1.0f);
}
