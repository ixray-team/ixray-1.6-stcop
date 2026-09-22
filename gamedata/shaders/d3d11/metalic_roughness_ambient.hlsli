#ifndef metalic_roughness_ambient_h_ixray_included
#define metalic_roughness_ambient_h_ixray_included

#include "common.hlsli"

#ifdef USE_PROCEDURAL_SKY_IBL

#include "common_sky.hlsli"

Texture2D<float4> s_sky_octo_ibl;
Texture2D<float4> s_sky_octo_ibl_small;
Texture2D<float4> s_sky_octo_ibl_diffuse;

// Текущая полноразмерная текстура создаётся как 512x512,
// а ComputeOctoUnwrap использует по одному физическому texel padding.
//
// Маленькая текстура получается обычным downsample всего ресурса,
// поэтому целочисленного padding в ней сейчас уже нет.
static const float SKY_IBL_FULL_PADDING = 4.0f;
static const float SKY_IBL_SMALL_PADDING = 1.0f;

#endif

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

float3 IBLPrepareShadingNormal(float3 view_to_surface, float3 shading_normal)
{
    const float3 V = safe_normalize(view_to_surface);
    float3 N = safe_normalize(shading_normal);
    const float NdotV = dot(N, -V);
    const float minimum_NdotV = 1e-4f;
    if (NdotV < minimum_NdotV)
    {
        N = safe_normalize(N + (-V) * (minimum_NdotV - NdotV));
    }
    return N;
}

float3 IBLReflectionDirection(float3 view_to_surface, float3 shading_normal)
{
    const float3 V = safe_normalize(view_to_surface);
    const float3 N = IBLPrepareShadingNormal(V, shading_normal);
    float3 R = safe_normalize(reflect(V, N));
    const float NdotR = dot(N, R);
    if (NdotR < 0.0f)
    {
        R = safe_normalize(
            R - N * NdotR + N * 1e-4f);
    }
    return R;
}

// temp comment old function, will be removed after testing
/*
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
*/

float3 CompureDiffuseIrradance(float3 N, float3 Hemi)
{
    float3 LightDirection = mul((float3x3) m_invV, N).xyz;

#ifdef USE_PROCEDURAL_SKY_IBL

    // Procedural octomap находится непосредственно в world space.
    // Старые cube-map remap операции здесь не нужны.
    float3 Irradance = sky_sample_gt7_octahedral_map(s_sky_octo_ibl_diffuse, smp_rtlinear, LightDirection, SKY_IBL_SMALL_PADDING).rgb;

    #ifdef USE_NORMAL_HEMI_DISTRIBUTION
        Hemi = min(Hemi, LightDirection.y * 0.375f + 0.375f);
    #endif

#else // USE_PROCEDURAL_SKY_IBL

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
        Factor = saturate(Factor + (1.0f - Factor) * topToDownVec) + (1.0f - Factor) * 0.5f;
    
        Hemi *= Factor * Factor; float3 Irradance = 1.0f;
        Hemi *= lerp(SampleLast, SampleNext, L_hemi_color.w);

    #else

        float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);

    #endif
#endif // USE_PROCEDURAL_SKY_IBL

    #ifdef USE_DIFFUSE_SKY_COLOR

        #ifdef USE_BGRA_SKYCOLOR
            //Irradance *= L_sky_color.zyx;
        #else
            //Irradance *= L_sky_color.xyz;
        #endif

    #else
        Irradance *= L_hemi_color.xyz;
    #endif

    #ifdef USE_IRRADANCE_SATURATION
        Irradance *= Irradance;
    #endif

    return Irradance * Hemi;
}

Texture2D s_env_fwd;
// temp comment old function, will be removed after testing
/*
float3 CompureSpecularIrradance(float3 R, float3 Hemi, float Roughness)
{
	float3 LightDirection = mul((float3x3)m_invV, R);
	
#ifdef USE_VIEW_REFLECTIONS
	float2 View = NormalEncode(LightDirection.xzy) * 0.875f;
	View = View * 0.5f + 0.5f;
	
	Roughness = 1.0f - Roughness;
	Roughness *= Roughness * Roughness;
	Roughness = 1.0f - Roughness;
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
	float4 SampleRef = saturate(s_env_fwd.SampleLevel(smp_linear, View, 6.0f * Roughness));
	SampleRef.xyz *= SampleRef.xyz < 1.0f ? rcp(1.0f - SampleRef.xyz) : 1.0f;
	
	Irradance = lerp(SampleRef.xyz, Irradance * saturate(Hemi * 3.0f), SampleRef.w);
#else
	Irradance *= Hemi;
#endif

	return Irradance;
}
*/

float3 CompureSpecularIrradance(float3 R, float3 Hemi, float Roughness)
{
    float3 LightDirection = mul((float3x3) m_invV, R);

#ifdef USE_VIEW_REFLECTIONS

    float2 View = NormalEncode(LightDirection.xzy) * 0.875f;
    View = View * 0.5f + 0.5f;

#endif

#ifdef USE_PROCEDURAL_SKY_IBL

    // Full map представляет резкий specular.
    const float3 sharp_radiance = sky_sample_gt7_octahedral_map(s_sky_octo_ibl, smp_rtlinear, LightDirection, SKY_IBL_FULL_PADDING).rgb;

    // Small map используется как приближённый rough specular.
    const float3 rough_radiance = sky_sample_gt7_octahedral_map(s_sky_octo_ibl_small, smp_rtlinear, LightDirection, SKY_IBL_SMALL_PADDING).rgb;
    
    float3 Irradance = lerp(sharp_radiance, rough_radiance, saturate(Roughness));
    

#else //USE_PROCEDURAL_SKY_IBL

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
        float3 SampleNextD = env_s1.SampleLevel( smp_linear, LightDirection, 0.0f).xyz;

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

#endif // USE_PROCEDURAL_SKY_IBL

#ifdef USE_SPECULAR_HEMI_COLOR
    //Irradance *= L_hemi_color.xyz;
#else

#ifdef USE_BGRA_SKYCOLOR
    //Irradance *= L_sky_color.zyx;
#else
    //Irradance *= L_sky_color.xyz;
#endif

#endif

#ifdef USE_IRRADANCE_SATURATION
    Irradance *= Irradance;
#endif

#ifdef USE_VIEW_REFLECTIONS

    float4 SampleRef = saturate(s_env_fwd.SampleLevel(smp_linear, View, 6.0f * Roughness));

    SampleRef.xyz *= SampleRef.xyz < 1.0f ? rcp(1.0f - SampleRef.xyz + 1e-8f) : 1.0f;

    Irradance = lerp(SampleRef.xyz, Irradance * saturate(Hemi * 3.0f), SampleRef.w);
    
    //Irradance = saturate(Irradance);

#else
    Irradance *= Hemi;
#endif

    return Irradance;
}

float3 AmbientLightingImpl(float3 DiffuseIrradance, float3 SpecularIrradance, float NdotV, float3 Diffuse, float3 Specular, float Roughness)
{
    #ifndef USE_PROCEDURAL_SKY_IBL
	    DiffuseIrradance = GammaToLinear(DiffuseIrradance);
	    SpecularIrradance = GammaToLinear(SpecularIrradance);
    #endif
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
	#ifdef USE_PROCEDURAL_SKY_IBL
		// Legacy gloss has no physical roughness: use an approximate mapping.
		float3 SpecularIrradance = CompureSpecularIrradance(Reflect, Surface.y, 1.0f - saturate(Gloss));
	#else
	float3 SpecularIrradance = CompureDiffuseIrradance(Reflect, Surface.y);
	#endif

	return DiffuseIrradance * Color + SpecularIrradance * Gloss;
}

#endif


