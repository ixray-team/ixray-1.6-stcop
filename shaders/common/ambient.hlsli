#ifndef SHARED_AMBIENT_HLSLI
#define SHARED_AMBIENT_HLSLI

// Ambient light and fog. As common\lighting.hlsli, by name: m_invV, L_hemi_color, L_sky_color, L_ambient, fog_params,
// fog_color, env_s0, env_s1, sky_s0, sky_s1, s_env_fwd, smp_linear, s_material, smp_material, LUMINANCE_VECTOR

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


	return Irradance * Hemi;
}

float3 CompureSpecularIrradance(float3 R, float3 Hemi, float Roughness)
{
	float3 LightDirection = mul((float3x3)m_invV, R);
	float LodRoughness = Roughness * (1.7f - 0.7f * Roughness);

#ifdef USE_VIEW_REFLECTIONS
	float2 View = NormalEncode(LightDirection.xzy) * 0.875f;
	View = View * 0.5f + 0.5f;

	float ViewRoughness = 1.0f - Roughness;
	ViewRoughness *= ViewRoughness * ViewRoughness;
	ViewRoughness = 1.0f - ViewRoughness;
#endif

#ifndef IBL_MAX_LOD
	float4 MipLevels = 0.0f;
	sky_s0.GetDimensions(MipLevels.x, MipLevels.y, MipLevels.z, MipLevels.w);
	float2 Lod = max(MipLevels.w - 1.0f, 0.0f) * LodRoughness;
#else
	float2 Lod = IBL_MAX_LOD * LodRoughness;
#endif

#if defined(IBL_FAKE_IRRADANCE) || !defined(IBL_MAX_LOD)
	float3 SampleLastD = env_s0.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	float3 SampleNextD = env_s1.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	#ifdef IBL_FAKE_IRRADANCE
		float FakeIrradance = LodRoughness;
	#else
		float FakeIrradance = MipLevels.w > 1.0f ? 0.0f : LodRoughness;
	#endif
#endif

#ifdef IBL_REMAP_POSITIVE_Y
	LightDirection.y = abs(LightDirection.y);
#endif

#ifdef IBL_REMAP_REFLECTIONS
	RemapVector(LightDirection);
#endif
	
	float3 SampleLast = sky_s0.SampleLevel(smp_linear, LightDirection, Lod.x).xyz;
	float3 SampleNext = sky_s1.SampleLevel(smp_linear, LightDirection, Lod.y).xyz;
	
#if defined(IBL_FAKE_IRRADANCE) || !defined(IBL_MAX_LOD)
	SampleLast = lerp(SampleLast, SampleLastD, FakeIrradance);
	SampleNext = lerp(SampleNext, SampleNextD, FakeIrradance);
#endif

	float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);

#ifdef USE_BGRA_SKYCOLOR
	Irradance *= L_sky_color.zyx;
#else
	Irradance *= L_sky_color.xyz;
#endif

#ifdef USE_VIEW_REFLECTIONS	
	float4 SampleRef = saturate(s_env_fwd.SampleLevel(smp_linear, View, 6.0f * ViewRoughness));
	SampleRef.xyz *= rcp(max(1.0f - SampleRef.xyz, 0.02f));
	
	Irradance = lerp(SampleRef.xyz, Irradance * Hemi, SampleRef.w);
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

// Share of the SSLR result in the image: none where the specular reflectance is near zero (a rough dielectric still has
// about 1.6%, and the environment would light its occluded parts). The SSLR passes skip pixels at 0, combine blends with the
// prefiltered environment.
float SslrWeight(float3 Normal, float3 View, float3 Specular, float Roughness)
{
	float2 BRDF = EpicGamesEnvBRDFApprox(saturate(dot(Normal, -View)), Roughness);
	float3 F = Specular * BRDF.x + min(1.0f, dot(Specular, 16.66667f)) * BRDF.y;
	return saturate((dot(F, LUMINANCE_VECTOR) - 0.005f) * 200.0f);
}

// Rough reflections are blurred by the filter and the history: from roughness 0.5 to 0.7 the trace gets shorter
float SslrRoughTrace(float Roughness)
{
	return saturate((Roughness - 0.5f) * 5.0f);
}

float SpecularOcclusion(float NdotV, float Occlusion, float Roughness)
{
	return saturate(pow(NdotV + Occlusion, exp2(-16.0f * Roughness - 1.0f)) - 1.0f + Occlusion);
}

float3 AmbientLighting(float3 View, float3 Normal, float3 Diffuse, float3 Specular, float Roughness, float Hemi)
{
	float3 Reflect = reflect(View, Normal);
	float NdotV = max(0.0, dot(Normal, -View));

	float3 DiffuseIrradance = CompureDiffuseIrradance(Normal, Hemi) + L_ambient.xyz;
	float3 SpecularIrradance = CompureSpecularIrradance(Reflect, SpecularOcclusion(NdotV, Hemi, Roughness), Roughness);
	
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

// Ambient light of a surface. Occ: screen-space occlusion; Sslr: the traced reflection of the pixel, w: 1 where
// the pass has one
float3 AmbientLight(IXRayGbuffer O, float Occ, float4 Sslr)
{
	if (O.MaterialID == FOLIAGE_ID)
	{
		float WorldNormalY = mul((float3x3)m_invV, O.Normal.xyz).y;
		O.Hemi = min(O.Hemi, WorldNormalY * 0.25f + 0.5f);
	}

#ifdef USE_LEGACY_LIGHT
	return Occ * AmbientLightingLegcay(O.View, O.Normal, O.Color, O.Material, O.Gloss, O.Hemi);
#else
	float Roughness = O.Roughness;
	Occ *= O.AO;

	float NdotV = max(0.0, dot(O.Normal, -O.View.xyz));

	#ifdef USE_VIEW_REFLECTIONS
		float SkyHemi = O.Depth > 0.02 ? O.Hemi : 1.0f;
	#else
		float SkyHemi = O.Hemi;
	#endif

	#ifdef USE_SSLR_REFLECTIONS
		float SslrBlend = SslrWeight(O.Normal, O.View, O.Specular, Roughness) * Sslr.w;
	#else
		float SslrBlend = 0.0f;
	#endif

	// Irradiance is gamma space here: x * LinearToGamma(o) is x * o after AmbientLightingImpl
	float3 SpecularIrradance = 0.0f;

	// Prefiltered environment where the SSLR passes skip the pixel or blend out
	[branch]
	if (SslrBlend < 1.0f)
	{
		SpecularIrradance = CompureSpecularIrradance(reflect(O.View, O.Normal), SpecularOcclusion(NdotV, SkyHemi, Roughness), Roughness) * LinearToGamma(Occ);
	}

	#ifdef USE_SSLR_REFLECTIONS
		// sslr_render applies screen-space AO to missed rays only, traced hits keep just the material AO
		SpecularIrradance = lerp(SpecularIrradance, Sslr.xyz * LinearToGamma(SpecularOcclusion(NdotV, O.AO, Roughness)), SslrBlend);
	#endif

	float3 DiffuseIrradance = (CompureDiffuseIrradance(O.Normal, O.Hemi) + L_ambient.xyz) * LinearToGamma(Occ);
	return AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, NdotV, O.Color, O.Specular, Roughness);
#endif
}

// xyz: the colour under the fog; w: what blends it over the sky
float4 ApplyFog(float3 Color, float ViewDist)
{
	float Fog = GammaToLinear(saturate(ViewDist * fog_params.w + fog_params.x));
	return float4(lerp(Color, GammaToLinear(fog_color.xyz), Fog), Fog * Fog);
}

#endif
