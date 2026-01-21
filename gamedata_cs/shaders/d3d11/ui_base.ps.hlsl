#include "common.hlsli"
#include "sload.hlsli"
#include "shadow.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

void main(p_bumped_new I, out float4 Color : SV_Target)
{
    IXrayMaterial M = (IXrayMaterial)0;
	
    M.Depth = I.position.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);
	
#if defined(USE_LENGTH_BUFFER) && defined(USE_AREF)
	clip(M.Color.w - def_aref);
	
    #ifdef USE_DXT1_HACK
		M.Color.xyz *= rcp(max(0.0001f, M.Color.w));
    #endif
#endif

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
#else
	M.Normal = float3(I.M1.z, I.M2.z, I.M3.z);
#endif

    M.Normal = normalize(M.Normal);
    M.Color.xyz = PushGamma(saturate(M.Color.xyz));

#ifndef USE_PBR
	float3 F0 = 0.0f;
#else
	float3 F0 = 0.04f;
#endif

#ifdef USE_LEGACY_LIGHT
    #ifndef USE_PBR
		M.Metalness = L_material.w;
    #else
		M.Color.xyz *= M.AO;
		M.AO = 1.0f;
		float Specular = M.Metalness * dot(M.Color.xyz, LUMINANCE_VECTOR);
		M.Color.xyz = lerp(M.Color.xyz, 0.04f, M.Metalness);
		M.Metalness = 0.5f - M.Roughness * M.Roughness * 0.5f;
		M.Roughness = Specular;
    #endif
#endif

	float4 LightColor = float2(1.0f, 0.0f).xxxy;
	float3 View = float2(1.0f, 0.0f).yyx;
	
	float3 LightDirection = float2(1.0f, 0.0f).yyx;
	LightDirection = normalize(LightDirection);
	
    float3 Light = DirectLight(LightColor, LightDirection, M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
    float3 Ambient = PushGamma(M.AO) * AmbientLightingUI(View, M.Normal, M.Color.xyz, M.Metalness, M.Roughness, M.Hemi, F0);
	
    Color.xyz = Ambient + Light.xyz;
    Color.w = saturate(M.Color.w + EPS_L);
}

