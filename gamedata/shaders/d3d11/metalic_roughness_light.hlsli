#ifndef metalic_roughness_light_h_ixray_included
#define metalic_roughness_light_h_ixray_included

#include "common.hlsli"

float DistributionGGX(float NdotH, float Roughness)
{
    float Alpha = Roughness * Roughness;
    float AlphaTwo = Alpha * Alpha + EPS_S;

    float AlphaTwoInv = AlphaTwo - 1.0f;

    float Divider = NdotH * NdotH * AlphaTwoInv + 1.0f;
    return AlphaTwo * rcp(Divider * Divider);
}

float FresnelSchlickScalar(float F0, float HdotV)
{
    return F0 + (1.0f - F0) * pow(1.0f - HdotV, 5.0f);
}

float VisibilityKelemen(float HdotV)
{
    float HdotV2 = max(HdotV * HdotV, EPS_S);
    return 0.25f / HdotV2;
}

float3 ClearCoatBRDF(float3 Normal, float3 Light, float3 View, float CoatRoughness, float CoatStrength)
{
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, -dot(Normal, Light));
    float NdotH = max(0.0f, -dot(Normal, Half));
    float HdotV = max(0.0f,  dot(Half, View));

    // Верхний слой — диэлектрик, фиксированный F0
    const float CoatF0 = 0.04f;

    float D = DistributionGGX(NdotH, CoatRoughness);
    float V = VisibilityKelemen(HdotV);
    float F = FresnelSchlickScalar(CoatF0, HdotV);

    return (D * V * F * CoatStrength).xxx;
}

// Simple GSC - like attention
float ComputeLightAttention(float3 PointToLight, float MinAttention)
{
    return saturate(1.0f - dot(PointToLight, PointToLight) * MinAttention);
}

float getSquareFalloffAttenuation(float3 posToLight, float lightInvRadius2) 
{
    float distanceSquare = dot(posToLight, posToLight);
    float factor = distanceSquare * lightInvRadius2;
    float smoothFactor = max(1.0 - factor * factor, 0.0);
    return (smoothFactor * smoothFactor) / max(distanceSquare, 1e-4);
}
float getLightAttenuation(float3 posToLight, float lightInvRadius2, float falloff)
{
    float distanceSqr = dot(posToLight, posToLight);
    float s2 = distanceSqr * lightInvRadius2;
    float smooth = max(1.0f - s2, 0.0f);
    return (smooth * smooth) / (1.0f + falloff * s2);
}
float getLightAttenuation(float3 posToLight, float lightInvRadius2)
{
    return getLightAttenuation(posToLight, lightInvRadius2, 10.0f);
}

float getSpotAngleAttenuation(float3 l, float3 lightDir,
        float innerAngle, float outerAngle) {
    // the scale and offset computations can be done CPU-side
    float cosOuter = cos(outerAngle);
    float spotScale = 1.0 / max(cos(innerAngle) - cosOuter, 1e-4);
    float spotOffset = -cosOuter * spotScale;

    float cd = dot(normalize(-lightDir), l);
    float attenuation = clamp(cd * spotScale + spotOffset, 0.0, 1.0);
    return attenuation * attenuation;
}

float GeometrySmithD(float NdotL, float NdotV, float Roughness)
{
    float R = Roughness + 1.0f;
    float K = R * R * 0.125f;
    float InvK = 1.0f - K;

    float DivGGXL = 1.0f * rcp(K + NdotL * InvK);
    float DivGGXV = 1.0f * rcp(K + NdotV * InvK);

    return 0.25f * DivGGXL * DivGGXV;
}

float3 FresnelSchlick(float3 F0, float HdotV)
{
    return F0 + (1.0f - F0) * pow(1.0f - HdotV, 5.0f);
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

    float3 Specular = D * G;
    float3 Diffuse = Color * (1.0f - Metalness);

    float3 BRDF = lerp(Diffuse, Specular, F);
    return Radiance.xyz * NdotL * BRDF;
#else
    float2 Material = s_material.SampleLevel(smp_material, float3(NdotL, NdotH, Metalness), 0).xy;
    return Radiance.xyz * (Material.x * Color.xyz + Material.y * Roughness.x * Radiance.w);
#endif
}


float3 DirectLightCC(
    float4 Radiance,
    float3 Light,
    float3 Normal,
    float3 ClearCoatNormal,
    float3 View,
    float3 Color,
    float Metalness,
    float Roughness,
    float3 F0 = 0.04f,
    float ThinCoat = 0.0f,
    float ClearCoatRoughness = 0.12f
)
{
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, -dot(Normal, Light));
    float NdotH = max(0.0f, -dot(Normal, Half));

#ifndef USE_LEGACY_LIGHT
    float NdotV = max(0.0f, -dot(Normal, View));
    float HdotV = max(0.0f,  dot(Half, View));

    float D = DistributionGGX(NdotH, Roughness);
    float G = GeometrySmithD(NdotL, NdotV, Roughness);
    float3 F = FresnelSchlick(lerp(F0, Color, Metalness), HdotV);

    float3 Specular = (D * G).xxx;
    float3 Diffuse = Color * (1.0f - Metalness);

    // Оставляю твой текущий стиль BRDF
    float3 BaseBRDF = lerp(Diffuse, Specular, F);

    // Clear coat layer
    float coatMask = saturate(ThinCoat);
    float coatF = FresnelSchlickScalar(0.04f, HdotV) * coatMask;
    float3 CoatBRDF = ClearCoatBRDF(ClearCoatNormal, Light, View, ClearCoatRoughness, coatMask);

    // Простое энергосбережение:
    // часть энергии уходит в верхний слой, база немного гасится
    float3 BRDF = BaseBRDF * (1.0f - coatF) + CoatBRDF;

    return Radiance.xyz * NdotL * BRDF;
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
	return (Radiance) * saturate(3.5f * SSS + 0.1f);
}

#endif

