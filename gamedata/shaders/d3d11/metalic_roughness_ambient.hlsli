#ifndef metalic_roughness_ambient_h_ixray_included
#define metalic_roughness_ambient_h_ixray_included

#include "common.hlsli"

// #define USE_IRRADANCE_SATURATION

//fitted for height-correlated smith
float2 EpicGamesEnvBRDFApprox(float NdotV, float roughness)
{
    //clamped cuz of extreme spike
    NdotV = min(NdotV, 0.998);

    float nsqr = NdotV * NdotV;
    float rsqr = roughness * roughness;
    
    float4 fac = float4(0.0187, 1.0133, 1.0000, 1.0000) +
    float4(1.9496, -2.4717, -0.0333, 2.0508) * NdotV +
    float4(1.2265, -1.2172, -1.3097, 0.2342) * roughness +
    float4(-7.6907, 3.4300, 0.5972, -26.9406) * NdotV * roughness +
    float4(18.3314, 1.4794, 19.3537, 11.1429) * nsqr +
    float4(-0.2894, 0.5564, 1.5052, 7.0828) * rsqr +
    float4(-19.3056, -2.2456, -28.2302, 18.5470) * nsqr * roughness +
    float4(7.0144, -1.8934, 1.3307, 50.6469) * NdotV * rsqr +
    float4(1.5728, 1.3618, 15.2939, -63.3557) * nsqr * rsqr;
    
    return saturate(fac.xy / fac.zw);
}

float3 CompureDiffuseIrradance(float3 N, float3 Hemi)
{
	float3 LightDirection = mul((float3x3)m_invV, N).xyz;

#ifdef IBL_REMAP_IRRADANCE
	RemapVector(LightDirection);
#endif

#ifdef USE_NORMAL_HEMI_DISTRIBUTION
	Hemi = min(Hemi, LightDirection.yyy * 0.375f + 0.375f);
#endif

	float3 SampleLast = env_s0.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	float3 SampleNext = env_s1.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;

#ifdef USE_CGIM_SKY_TWEAK
	float topToDownVec = saturate(LightDirection.y);
	topToDownVec *= topToDownVec;
	
	float Factor = SMALLSKY_TOP_VECTOR_POWER;
	Factor = saturate(Factor + (1.0 - Factor) * topToDownVec) + (1.0 - Factor) * 0.5f;
	
	Hemi *= Factor * Factor; float3 Irradance = 1.0f;
	Hemi *= lerp(SampleLast, SampleNext, L_hemi_color.w);
#else
	float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);
#endif

#ifdef USE_DIFFUSE_SKY_COLOR
	#ifdef USE_BGRA_SKYCOLOR
		Irradance *= L_sky_color.zyx;
	#else
		Irradance *= L_sky_color.xyz;
	#endif
#else
	Irradance *= L_hemi_color.xyz;
#endif

#ifdef USE_IRRADANCE_SATURATION
	Irradance *= Irradance;
#endif

	return Irradance * Hemi;
}

float3 CompureSpecularIrradance(float3 R, float3 Hemi, float Roughness)
{
	float3 LightDirection = mul((float3x3)m_invV, R);
	
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
	float3 SampleLastD = env_s0.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	float3 SampleNextD = env_s1.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
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
	#ifdef USE_BGRA_SKYCOLOR
	   	Irradance *= L_sky_color.zyx;
	#else
	    Irradance *= L_sky_color.xyz;
	#endif
#endif

#ifdef USE_IRRADANCE_SATURATION
	Irradance *= Irradance;
#endif

#ifdef USE_VIEW_REFLECTIONS
	float4 SampleRef = s_env.SampleLevel(smp_linear, mul((float3x3)m_env_view, R), 8.0f * Roughness);
	SampleRef.xyz *= rcp(1.00001f - SampleRef.xyz);
	
    float fog = saturate(SampleRef.w * fog_params.w + fog_params.x);
	Irradance = lerp(LinearToGamma(SampleRef.xyz), Irradance * saturate(Hemi * 3.0f), fog);
#else
	Irradance *= Hemi;
#endif

	return Irradance;
}

float3 AmbientLightingImpl(float3 DiffuseIrradance, float3 SpecularIrradance, float NdotV, float3 Diffuse, float3 Specular, float Roughness)
{
	DiffuseIrradance = GammaToLinear(DiffuseIrradance);
	SpecularIrradance = GammaToLinear(SpecularIrradance);

	DiffuseIrradance *= Diffuse;

	float2 BRDF = EpicGamesEnvBRDFApprox(NdotV, Roughness);
	
	float F90 = min(1.0, dot(Specular, 16.66667));
	float3 F = Specular * BRDF.x + F90 * BRDF.y;

	return lerp(DiffuseIrradance, SpecularIrradance, F);
}

float3 AmbientLighting(float3 View, float3 Normal, float3 Diffuse, float3 Specular, float Roughness, float Hemi)
{
	float3 Reflect = reflect(View, Normal);
	float NdotV = max(0.0, dot(Normal, -View));

	float3 DiffuseIrradance = CompureDiffuseIrradance(Normal, Hemi) + L_ambient.xyz;
	float3 SpecularIrradance = CompureSpecularIrradance(Reflect, Hemi, Roughness);
	
	return AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, NdotV, Diffuse, Specular, Roughness);
}

float3 AmbientLightingLegcay(float3 View, float3 Normal, float3 Color, float Material, float Gloss, float Hemi)
{
	float3 Reflect = reflect(View, Normal);

	float Specular = 0.5f - 0.5f * dot(View, Reflect);
	float2 Surface = s_material.SampleLevel(smp_material, float3(Hemi, Specular, Material), 0).xy;

	float3 DiffuseIrradance = CompureDiffuseIrradance(Normal, Surface.x) + L_ambient.xyz;
	float3 SpecularIrradance = CompureDiffuseIrradance(Reflect, Surface.y);

	return DiffuseIrradance * Color + SpecularIrradance * Gloss;
}

float3 AmbientLightingUI(float3 View, float3 Normal, float3 Diffuse, float3 Specular, float Roughness)
{
	float3 Reflect = reflect(View, Normal);

#ifndef USE_LEGACY_LIGHT
	float3 DiffuseIrradance = env_s0.SampleLevel(smp_linear, Normal, 0.0f).xyz;
	float3 SpecularIrradance = sky_s0.SampleLevel(smp_linear, Reflect, 10.0f * Roughness).xyz;
	
	float NdotV = max(0.0, dot(Normal, -View));
	
	return AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, NdotV, Diffuse, Specular, Roughness);
#else
	// float Specular = 0.5f - 0.5f * dot(View, Reflect);
	// float2 Material = s_material.SampleLevel(smp_material, float3(Hemi, Specular, Metalness), 0).xy;

	// float3 DiffuseIrradance = Material.x * env_s0.SampleLevel(smp_linear, Normal, 0.0f).xyz;
	// float3 SpecularIrradance = Material.y * env_s0.SampleLevel(smp_linear, Reflect, 0.0f).xyz;

	return Diffuse;
#endif
}
#endif


