#ifndef TIRAMISU_MATERIAL_LIGHTING_COMMON
#define TIRAMISU_MATERIAL_LIGHTING_COMMON

static const float TIRAMISU_MATERIAL_PI = 3.14159265359f;

float MaterialDistributionGGX(float NdotH, float Roughness)
{
    const float A = max(Roughness * Roughness, 0.0025f);
    const float A2 = A * A;
    const float Denominator = NdotH * NdotH * (A2 - 1.0f) + 1.0f;
    return A2 / max(TIRAMISU_MATERIAL_PI * Denominator * Denominator, 1.0e-5f);
}

float MaterialGeometrySchlickGGX(float NdotX, float Roughness)
{
    const float R = Roughness + 1.0f;
    const float K = (R * R) * 0.125f;
    return NdotX / max(NdotX * (1.0f - K) + K, 1.0e-5f);
}

float3 MaterialFresnelSchlick(float CosTheta, float3 F0)
{
    return F0 + (1.0f - F0) * pow(saturate(1.0f - CosTheta), 5.0f);
}

float3 EvaluateMaterialDirectLighting(MaterialInputs Material, float3 View,
    float3 Light, float3 Radiance)
{
    const float3 N = normalize(Material.Normal);
    const float3 V = normalize(View);
    const float3 L = normalize(Light);
    const float3 H = normalize(V + L);
    const float NdotL = saturate(dot(N, L));
    const float NdotV = saturate(dot(N, V));
    const float NdotH = saturate(dot(N, H));
    const float HdotV = saturate(dot(H, V));
    const float Roughness = saturate(Material.Roughness);
    const float Metallic = saturate(Material.Metallic);
    const float3 BaseColor = max(Material.BaseColor, 0.0f);
    const float3 F0 = lerp(0.04f.xxx, BaseColor, Metallic);
    const float3 F = MaterialFresnelSchlick(HdotV, F0);
    const float D = MaterialDistributionGGX(NdotH, Roughness);
    const float G = MaterialGeometrySchlickGGX(NdotV, Roughness) *
        MaterialGeometrySchlickGGX(NdotL, Roughness);
    const float3 Specular = (D * G * F) /
        max(4.0f * NdotV * NdotL, 1.0e-4f);
    const float3 Diffuse = (1.0f - F) * (1.0f - Metallic) * BaseColor /
        TIRAMISU_MATERIAL_PI;
    return (Diffuse + Specular) * Radiance * NdotL;
}

float3 EvaluateMaterialSceneDirectLighting(MaterialInputs Material,
    float3 View, float3 WorldPosition)
{
    float3 Result = 0.0f;
    const uint Count = min(LightCount, 64u);
    for (uint Index = 0u; Index < Count; ++Index)
    {
        const MaterialLightGpuData Light =
            LoadMaterialLightGpuData(Index);
        float3 ToLight = 0.0f;
        float Attenuation = 1.0f;
        if (Light.Type == 0u)
        {
            ToLight = -Light.Direction;
        }
        else
        {
            ToLight = Light.Position - WorldPosition;
            const float Distance = length(ToLight);
            if (Distance <= 1.0e-5f || Distance >= Light.Range)
                continue;
            ToLight /= Distance;
            const float RangeRatio = saturate(Distance / Light.Range);
            const float SmoothFalloff =
                saturate(1.0f - RangeRatio * RangeRatio);
            Attenuation = SmoothFalloff * SmoothFalloff;
            if (Light.Type == 2u)
            {
                const float ConeCosine =
                    dot(-ToLight, normalize(Light.Direction));
                Attenuation *= smoothstep(
                    Light.CosOuterCone,
                    Light.CosInnerCone,
                    ConeCosine);
            }
        }
        const float3 Radiance =
            max(Light.Color, 0.0f) *
            max(Light.Intensity, 0.0f) * Attenuation;
        Result += EvaluateMaterialDirectLighting(
            Material, View, ToLight, Radiance);
    }
    return Result;
}

#endif
