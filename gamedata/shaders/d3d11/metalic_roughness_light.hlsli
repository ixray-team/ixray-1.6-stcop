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

float GetLightAttention(float3 PointToLight, float LightInvRadius2, float Falloff)
{
    float DistanceSqr = dot(PointToLight, PointToLight);
    float Factor = dot(PointToLight, PointToLight) * LightInvRadius2;
    float Attention = max(1.0f - Factor, 0.0f);
	
    return (Attention * Attention) / (1.0f + Falloff * Factor);
}

float ComputeLightAttention(float3 PointToLight, float LightInvRadius2)
{
    return saturate(1.0f - dot(PointToLight, PointToLight) * LightInvRadius2);
}

float GeometrySmithD(float NdotL, float NdotV, float Roughness)
{
    float R = Roughness + 1.0f;
    float K = R * R * 0.125f;

    float DivGGXL = rcp(lerp(NdotL, 1.0f, K));
    float DivGGXV = rcp(lerp(NdotV, 1.0f, K));

    return DivGGXL * DivGGXV;
}

float3 FresnelSchlick(float3 F, float NdotV)
{
	float F90 = min(1.0, dot(F, 16.66667));
    return lerp(F, F90, pow(1.0f - NdotV, 5.0f));
}

float3 DirectLight(float4 Radiance, float3 Light, float3 Normal, float3 View, float3 Diffuse, float3 Specular, float Roughness)
{
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, dot(Normal, -Light));
    float NdotH = max(0.0f, dot(Normal, -Half));

    float NdotV = max(0.0f, dot(Normal, -View));
    float HdotV = max(0.0f, dot(Half, View));

    float3 D = DistributionGGX(NdotH, Roughness);
    float3 G = GeometrySmithD(NdotL, NdotV, Roughness);
    float3 F = FresnelSchlick(Specular, HdotV);

    float3 BRDF = lerp(Diffuse, 0.25f * D * G, F);
    return GammaToLinear(Radiance.xyz) * NdotL * BRDF;
}

float3 DirectLightLegacy(float4 Radiance, float3 Light, float3 Normal, float3 View, float3 Color, float Material, float Gloss)
{
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, -dot(Normal, Light));
    float NdotH = max(0.0f, -dot(Normal, Half));

    float2 Surface = s_material.SampleLevel(smp_material, float3(NdotL, NdotH, Material), 0).xy;
    return Radiance * float3(Surface.x * Color.xyz + Surface.y * Gloss * Radiance.w);
}

float3 SimpleTranslucency(float3 Radiance, float3 Light, float3 Normal)
{
	float NdotL = dot(Light, Normal);
	float Scale = 0.36f * NdotL;

	float Attention = Scale + 0.0769f; Attention *= Attention * 1.171f;	
	float Factor = 1.0f - saturate(abs(Scale) * 13.0f - 1.0f);

	float SSS = lerp(saturate(NdotL), Attention, Factor * Factor);
	return GammaToLinear(Radiance.xyz) * saturate(3.5f * SSS + 0.1f);
}

float3 sample_vndf_isotropic(float3 n, float3 wi, float2 u, float alpha)
{
	alpha = max(1e-3, alpha);

	// decompose the floattor in parallel and perpendicular components
	float3 wi_z = -n * dot(wi, n);
	float3 wi_xy = wi + wi_z;
 
	// warp to the hemisphere configuration
	float3 wiStd = -normalize(alpha * wi_xy + wi_z);
 
	// sample a spherical cap in (-wiStd.z, 1]
	float wiStd_z = dot(wiStd, n);
	float z = 1.0 - u.y * (1.0 + wiStd_z);
	float sinTheta = sqrt(saturate(1.0f - z * z));
	float phi = 6.28 * u.x - 3.14;
	float x = sinTheta * cos(phi);
	float y = sinTheta * sin(phi);
	float3 cStd = float3(x, y, z);
 
	// reflect sample to align with normal
	float3 up = float3(0, 0, 1.000001); // Used for the singularity
	float3 wr = n + up;
	float3 c = dot(wr, cStd) * wr / wr.z - cStd;
 
	// compute halfway direction as standard normal
	float3 wmStd = c + wiStd;
	float3 wmStd_z = n * dot(n, wmStd);
	float3 wmStd_xy = wmStd_z - wmStd;

	return normalize(alpha * wmStd_xy + wmStd_z);
}

float pdf_vndf_isotropic(float3 n, float3 wi, float3 wo, float alpha)
{
	alpha = max(1e-3, alpha);

	float alphaSquare = alpha * alpha;
	float3 wm = normalize(wo + wi);
	float zm = dot(wm, n);
	float zi = dot(wi, n);
	float nrm = rsqrt((zi * zi) * (1.0f - alphaSquare) + alphaSquare);
	float sigmaStd = (zi * nrm) * 0.5f + 0.5f;
	float sigmaI = sigmaStd / nrm;
	float nrmN = (zm * zm) * (alphaSquare - 1.0f) + 1.0f;
	return alphaSquare / (3.14 * 4.0f * nrmN * nrmN * sigmaI);
}

#endif

