#include "common.hlsli"
#include "sload.hlsli"

#ifdef USE_OFFSCREEN_REFLECTIONS
#define USE_VIEW_REFLECTIONS
#define USE_OLD_VIEW_REFLECTIONS
#endif

#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "shadow.hlsli"

void main(p_bumped_new I, out f_forward O)
{
    IXrayMaterial M;
    M.Depth = I.position.z;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
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

#ifdef USE_LM_HEMI
    float4 hs = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);

    M.Sun = get_sun(hs);
    M.Hemi = get_hemi(hs);
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

    float4 Point = float4(M.Point.xyz, 1.f);
    Point.xyz += M.Normal * 0.025f;

	Point.xyz = mul(m_invV, Point).xyz;

	//Sample cascades
	int cascade_index;
	float3 smap_texcoord;
	bool is_in_bounds = calc_cascades(Point.xyz, m_shadow_sun, cascade_index, smap_texcoord);

	float Shadow = 1.0;

	if(is_in_bounds) {
		Shadow = pcf_5x5(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
	}

	//Hozar's far cascade tricks!
	//Imperfect port, fix it later.
	if(cascade_index == 3)
	{
		float3 Factor = smoothstep(0.499f, 0.498f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;

		float FarShadow = saturate(M.Hemi * 8.0f - 2.0f);
		Shadow = lerp(FarShadow, Shadow, Fade);
	}
	
	M.Sun = Shadow;

	float4 LightColor = float4(L_sun_color.xyz, 0.5f);
	
	float ViewLength = length(M.Point);
	float3 View = M.Point.xyz * rcp(ViewLength);
	
    float3 Light = M.Sun * DirectLight(LightColor, mul((float3x3)m_V, L_sun_dir_w.xyz), M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
    float3 Ambient = PushGamma(M.AO) * AmbientLighting(View, M.Normal, M.Color.xyz, M.Metalness, M.Roughness, M.Hemi, F0);
	
#ifdef USE_LENGTH_BUFFER
	#ifdef USE_LM_HEMI
		float3 Lmap = s_lmap.Sample(smp_rtlinear, I.tcdh.zw).xyz;
	#else
		float3 Lmap = I.lmap;
	#endif
	
	Light += DirectLight(float4(Lmap.xyz * 2.0f, 0.5f), View, M.Normal, View, M.Color.xyz, M.Metalness, M.Roughness, F0);
#endif
	
    O.Color.xyz = Ambient + Light.xyz;
    O.Color.w = M.Color.w;

    float Fog = PushGamma(saturate(ViewLength * fog_params.w + fog_params.x));
    O.Color = lerp(O.Color, PushGamma(fog_color), Fog);

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
    O.Reactive = O.Color.w * 0.9f;
	
#ifdef USE_LENGTH_BUFFER
	O.Color.w = ViewLength;
	O.Color.xyz = saturate(O.Color.xyz * rcp(1.0f + O.Color.xyz));
#endif
}

