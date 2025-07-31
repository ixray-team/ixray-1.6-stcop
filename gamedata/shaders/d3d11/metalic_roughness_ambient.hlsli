#ifndef metalic_roughness_ambient_h_ixray_included
#define metalic_roughness_ambient_h_ixray_included

#include "common.hlsli"

float2 EpicGamesEnvBRDFApprox(float NdotV, float Roughness)
{
	float4 R = Roughness * float4(-1, -0.0275, -0.572, 0.022) + float4(1, 0.0425, 1.04, -0.04);
	float A004 = min(R.x * R.x, exp2(-9.28 * NdotV)) * R.x + R.y;
	return float2(-1.04, 1.04) * A004 + R.zw;
}

float3 CompureDiffuseIrradance(float3 N, float Hemi)
{
	float3 LightDirection = mul((float3x3)m_invV, N).xyz;

#ifdef IBL_REMAP_IRRADANCE
	RemapVector(LightDirection);
#endif

	float3 SampleLast = env_s0.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	float3 SampleNext = env_s1.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;

	float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);

#ifdef USE_DIFFUSE_SKY_COLOR
	Irradance *= L_sky_color.xyz;
#else
	Irradance *= L_hemi_color.xyz;
#endif

#ifdef USE_LEGACY_LIGHT
	Irradance *= Irradance;
#endif

	return Irradance * Hemi;
}

float3 CompureSpecularIrradance(float3 R, float Hemi, float Roughness)
{
	float3 LightDirection = mul((float3x3)m_invV, R);

#ifdef USE_OLD_VIEW_REFLECTIONS
	R = mul(m_V_old, LightDirection);
#endif
	
#ifndef IBL_MAX_LOD
	float4 MipLevels = 0.0f;
	sky_s0.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
	float2 Lod = MipLevels.w * Roughness;
	#ifdef USE_HQ_SKY2_LOD
		sky_s1.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
		Lod.y = MipLevels.w * Roughness;
	#endif
#else
	float2 Lod = IBL_MAX_LOD * Roughness;
#endif
	
#ifdef IBL_FAKE_IRRADANCE
	float3 SampleLastD = env_s0.SampleLevel(smp_rtlinear, LightDirection, 0.0f).xyz;
	float3 SampleNextD = env_s1.SampleLevel(smp_rtlinear, LightDirection, 0.0f).xyz;
#endif

#ifdef IBL_REMAP_POSITIVE_Y
	LightDirection.y = abs(LightDirection.y);
#endif

#ifdef IBL_REMAP_REFLECTIONS
	RemapVector(LightDirection);
#endif
	
	float3 SampleLast = sky_s0.SampleLevel(smp_linear, LightDirection, Lod.x).xyz;
	float3 SampleNext = sky_s1.SampleLevel(smp_linear, LightDirection, Lod.y).xyz;
	
#ifdef IBL_FAKE_IRRADANCE
	SampleLast = lerp(SampleLast, SampleLastD, Roughness);
	SampleNext = lerp(SampleNext, SampleNextD, Roughness);
#endif

	float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);

#ifdef USE_SPECULAR_HEMI_COLOR
	Irradance *= L_hemi_color.xyz;
#else
	Irradance *= L_sky_color.xyz;
#endif

#ifdef USE_VIEW_REFLECTIONS
	float4 SampleRef = s_env.SampleLevel(smp_linear, R, 8.0f * Roughness);
	SampleRef.xyz *= rcp(1.00001f - SampleRef.xyz);
	
    float fog = saturate(SampleRef.w * fog_params.w + fog_params.x);
	Irradance = lerp(PopGamma(SampleRef.xyz), Irradance, fog);
#endif

	return Irradance * Hemi;
}

float3 AmbientLighting(float3 DiffuseIrradance, float3 SpecularIrradance, float NdotV, float3 Color, float Metalness, float Roughness, float3 F0 = 0.04f)
{
	DiffuseIrradance = PushGamma(DiffuseIrradance);
	SpecularIrradance = PushGamma(SpecularIrradance);

	DiffuseIrradance *= (1.0f - Metalness) * Color;

	float2 BRDF = EpicGamesEnvBRDFApprox(NdotV, Roughness);
	float3 F = lerp(F0, Color, Metalness) * BRDF.x + BRDF.y;

	return lerp(DiffuseIrradance, SpecularIrradance, F);
}

float3 AmbientLighting(float3 View, float3 Normal, float3 Color, float Metalness, float Roughness, float Hemi, float3 F0 = 0.04f)
{
	float3 Reflect = reflect(View, Normal);

#ifndef USE_LEGACY_LIGHT
	float3 DiffuseIrradance = CompureDiffuseIrradance(Normal, Hemi) + L_ambient.xyz;
	float3 SpecularIrradance = CompureSpecularIrradance(Reflect, Hemi, Roughness);
	float NdotV = max(0.0, dot(Normal, -View));
	
	return AmbientLighting(DiffuseIrradance, SpecularIrradance, NdotV, Color, Metalness, Roughness, F0);
#else
	float Specular = 0.5f - 0.5f * dot(View, Reflect);
	float2 Material = s_material.SampleLevel(smp_material, float3(Hemi, Specular, Metalness), 0).xy;

	float3 DiffuseIrradance = CompureDiffuseIrradance(Normal, Material.x) + L_ambient.xyz;
	float3 SpecularIrradance = CompureDiffuseIrradance(Reflect, Material.y);

	return DiffuseIrradance * Color + SpecularIrradance * Roughness;
#endif
}
#endif

