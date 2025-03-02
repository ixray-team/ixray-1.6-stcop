#ifndef metalic_roughness_light_h_ixray_included
#define metalic_roughness_light_h_ixray_included

#include "common.hlsli"

float DistributionGGX(float NdotH, float Roughness)
{
    float Alpha = Roughness * Roughness;
    float AlphaTwo = Alpha * Alpha;

    float AlphaTwoInv = AlphaTwo - 1.0f;

    float Divider = NdotH * NdotH * AlphaTwoInv + 1.0f;
    return AlphaTwo * rcp(Divider * Divider);
}

#ifndef USE_LEGACY_LIGHT
// Simple PBR - like attention
float ComputeLightAttention(float3 PointToLight, float MinAttention)
{
	float lightDistSqr = dot(PointToLight, PointToLight);
    return saturate(1.0f - pow(lightDistSqr * MinAttention, 2.0f)) * rcp(lightDistSqr + 1.0f);
}
#else
// Simple GSC - like attention
float ComputeLightAttention(float3 PointToLight, float MinAttention)
{
    return saturate(1.0f - dot(PointToLight, PointToLight) * MinAttention);
}
#endif

float GeometrySmithD(float NdotL, float NdotV, float Roughness)
{
    float R = Roughness + 1.0f;
    float K = R * R * 0.125f;
    float InvK = 1.0f - K;

    float DivGGXL = 1.0f * rcp(K + NdotL * InvK);
    float DivGGXV = 1.0f * rcp(K + NdotV * InvK);

    return 0.25f * DivGGXL * DivGGXV;
}

float3 FresnelSchlick(float3 F, float NdotV)
{
    return F + (1.0f - F) * pow(1.0f - NdotV, 5.0f);
}

float3 DirectLight(float4 Radiance, float3 Light, float3 Normal, float3 View, float3 Color, float Metalness, float Roughness, float3 F0 = 0.04f)
{
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, -dot(Normal, Light));
    float NdotH = max(0.0f, -dot(Normal, Half));

#ifndef USE_LEGACY_LIGHT
    float NdotV = max(0.0f, -dot(Normal, View));
    float HdotV = max(0.0f, dot(Half, View));

    float3 D = DistributionGGX(NdotH, Roughness);
    float3 G = GeometrySmithD(NdotL, NdotV, Roughness);
    float3 F = FresnelSchlick(lerp(F0, Color, Metalness), HdotV);

    float3 Specular = D * F * G;
    float3 Diffuse = Color * (1.0f - Metalness) * (1.0f - F);

    float3 BRDF = Specular + Diffuse;
    return PushGamma(Radiance.xyz) * NdotL * BRDF;
#else
    float2 Material = s_material.SampleLevel(smp_material, float3(NdotL, NdotH, Metalness), 0).xy;
    return Radiance.xyz * (Material.x * Color.xyz + Material.y * Roughness.x * Radiance.w);
#endif
}

float3 SimpleTranslucency(float3 Radiance, float3 Light, float3 Normal)
{
	float NdotL = dot(Light, Normal);
	float Scale = 0.36f * NdotL;

	float Attention = Scale + 0.0769f; Attention *= Attention * 1.171f;	
	float Factor = 1.0f - saturate(abs(Scale) * 13.0f - 1.0f);

	float SSS = lerp(saturate(NdotL), Attention, Factor * Factor);
	return PushGamma(Radiance) * saturate(3.5f * SSS + 0.1f);
}

#endif

