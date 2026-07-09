#include "common.hlsli"
#include "sload.hlsli"
#include "shadow.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

float3 AmbientLightingUI(float3 View, float3 Normal, float3 Diffuse, float3 Specular, float Roughness)
{
	float3 Reflect = reflect(View, Normal);

#ifndef USE_LEGACY_LIGHT
	float3 DiffuseIrradance = env_s0.SampleLevel(smp_linear, Normal, 0.0f).xyz;
	float3 SpecularIrradance = sky_s0.SampleLevel(smp_linear, Reflect, 10.0f * Roughness).xyz;
	
	float NdotV = max(0.0, dot(Normal, -View));
	
	return AmbientLightingImpl(DiffuseIrradance, SpecularIrradance, NdotV, Diffuse, Specular, Roughness);
#else
	float HdotV = 0.5f - 0.5f * dot(View, Reflect);
	float2 Material = s_material.SampleLevel(smp_material, float3(1.0f, HdotV, Roughness), 0).xy;

	float3 DiffuseIrradance = Material.x * env_s0.SampleLevel(smp_linear, Normal, 0.0f).xyz;
	float3 SpecularIrradance = Material.y * env_s0.SampleLevel(smp_linear, Reflect, 0.0f).xyz;

	return DiffuseIrradance * Diffuse + SpecularIrradance * Specular;
#endif
}

void main(p_bumped_new I, out float4 Color : SV_Target)
{
    IXRayMaterial M = (IXRayMaterial)NULL;
	
    M.Depth = I.position.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);
	
#if defined(USE_AREF)
	clip(M.Color.w - def_aref);
	
    #ifdef USE_DXT1_HACK
		M.Color.xyz *= M.Color.w > 0.0f ? rcp(M.Color.w) : 0.0f;
    #endif
#endif

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
#else
	M.Normal = float3(I.M1.z, I.M2.z, I.M3.z);
#endif

    M.Normal = normalize(M.Normal);

#ifdef USE_LEGACY_LIGHT
	M.Material = L_material.w;
#else
    M.Color.xyz = GammaToLinear(M.Color.xyz);
	M.Specular = M.Specular * M.Specular * 0.16f;
#endif

	float4 LightColor = float2(1.0f, 0.0f).xxxy;
	float3 View = float2(1.0f, 0.0f).yyx;
	
	float3 LightDirection = float2(1.0f, 0.0f).yyx;
	LightDirection = normalize(LightDirection);
	
	#ifndef USE_LEGACY_LIGHT
		float3 Diffuse = M.Color.xyz * float(1.0f - M.Metalness);
		float3 Specular = lerp(M.Specular, M.Color.xyz, M.Metalness);
		
		float3 Light = DirectLight(LightColor, LightDirection, M.Normal, View, Diffuse, Specular, M.Roughness);
		float3 Ambient = GammaToLinear(M.AO) * AmbientLightingUI(View, M.Normal, Diffuse, Specular, M.Roughness);
	#else
		float3 Light = DirectLightLegacy(LightColor, LightDirection, M.Normal, View, M.Color.xyz, M.Material, M.Gloss);
		float3 Ambient = AmbientLightingUI(View, M.Normal, M.Color.xyz, M.Gloss.xxx, M.Material);
	#endif
	
    Color.xyz = Ambient + Light.xyz;
    Color.w = saturate(M.Color.w + EPS_L);
}

