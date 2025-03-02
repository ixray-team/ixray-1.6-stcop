#include "common.hlsli"
#include "sload.hlsli"

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

uniform float3 L_sun_dir_e;

void main(p_bumped_new I, out f_forward O)
{
    IXrayMaterial M;
    M.Depth = I.position.z;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);

#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    M.Normal = mul(float3x3(I.M1, I.M2, I.M3), M.Normal);
#else
	M.Normal = float3(I.M1.z, I.M2.z, I.M3.z);
#endif

    M.Normal = normalize(M.Normal);

#ifdef USE_LM_HEMI
    float4 lm = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);

    M.Sun = get_sun(lm);
    M.Hemi = get_hemi(lm);
#endif

    M.Sun = saturate(M.Sun * 2.0f);
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

	float4 LightColor = float4(L_sun_color.xyz, 0.5f);
	
	float ViewLength = length(M.Point);
	float3 View = M.Point.xyz * rcp(ViewLength);
	
    float3 Light = M.Sun * DirectLight(LightColor, L_sun_dir_e.xyz, M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
    float3 Ambient = PushGamma(M.AO) * AmbientLighting(View, M.Normal, M.Color.xyz, M.Metalness, M.Roughness, M.Hemi, F0);
	
    O.Color.xyz = Ambient + Light.xyz;
    O.Color.w = M.Color.w;

    float fog = PushGamma(saturate(ViewLength * fog_params.w + fog_params.x));
    O.Color = lerp(O.Color, PushGamma(fog_color), fog);

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
    O.Reactive = O.Color.w * 0.9f;
}

