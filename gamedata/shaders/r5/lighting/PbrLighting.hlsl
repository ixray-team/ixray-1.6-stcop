#ifndef TIRAMISU_PBR_LIGHTING_HLSL
#define TIRAMISU_PBR_LIGHTING_HLSL

static const float TIRAMISU_PI = 3.14159265358979323846f;

struct TiramisuPbrSurface
{
    float3 BaseColor;
    float3 Normal;
    float Roughness;
    float Metallic;
    float AmbientOcclusion;
    float3 Emissive;
};

float TiramisuDistributionGGX(float NdotH, float Roughness)
{
    const float Alpha = max(Roughness * Roughness, 0.0025f);
    const float AlphaSquared = Alpha * Alpha;
    const float Denominator = NdotH * NdotH * (AlphaSquared - 1.0f) + 1.0f;
    return AlphaSquared / max(TIRAMISU_PI * Denominator * Denominator, 1.0e-6f);
}

float TiramisuGeometrySchlickGGX(float NdotDirection, float Roughness)
{
    const float Remapped = Roughness + 1.0f;
    const float K = Remapped * Remapped * 0.125f;
    return NdotDirection / max(NdotDirection * (1.0f - K) + K, 1.0e-6f);
}

float TiramisuGeometrySmith(float NdotV, float NdotL, float Roughness)
{
    return TiramisuGeometrySchlickGGX(NdotV, Roughness) *
        TiramisuGeometrySchlickGGX(NdotL, Roughness);
}

float3 TiramisuFresnelSchlick(float VdotH, float3 F0)
{
    const float OneMinus = 1.0f - saturate(VdotH);
    const float Factor = OneMinus * OneMinus * OneMinus * OneMinus * OneMinus;
    return F0 + (1.0f - F0) * Factor;
}

float3 EvaluateTiramisuDirectLight(TiramisuPbrSurface Surface, float3 ViewDirection,
    float3 LightDirection, float3 Radiance)
{
    const float3 N = normalize(Surface.Normal);
    const float3 V = normalize(ViewDirection);
    const float3 L = normalize(LightDirection);
    const float3 H = normalize(V + L);
    const float NdotV = saturate(dot(N, V));
    const float NdotL = saturate(dot(N, L));
    const float NdotH = saturate(dot(N, H));
    const float VdotH = saturate(dot(V, H));

    const float Roughness = clamp(Surface.Roughness, 0.045f, 1.0f);
    const float3 F0 = lerp(0.04f.xxx, saturate(Surface.BaseColor), saturate(Surface.Metallic));
    const float D = TiramisuDistributionGGX(NdotH, Roughness);
    const float G = TiramisuGeometrySmith(NdotV, NdotL, Roughness);
    const float3 F = TiramisuFresnelSchlick(VdotH, F0);
    const float3 Specular = (D * G * F) / max(4.0f * NdotV * NdotL, 1.0e-5f);
    const float3 DiffuseWeight = (1.0f - F) * (1.0f - saturate(Surface.Metallic));
    const float3 Diffuse = DiffuseWeight * saturate(Surface.BaseColor) * (1.0f / TIRAMISU_PI);
    return (Diffuse + Specular) * Radiance * NdotL;
}

float ComputeTiramisuPointAttenuation(float DistanceSquared, float InverseRadiusSquared)
{
    const float NormalizedDistance = DistanceSquared * InverseRadiusSquared;
    const float Smooth = saturate(1.0f - NormalizedDistance * NormalizedDistance);
    return (Smooth * Smooth) / max(DistanceSquared, 1.0f);
}

#endif
